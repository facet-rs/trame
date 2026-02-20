fn main() {
    println!("Run with: cargo test");
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use std::alloc::Layout;
    use std::panic::catch_unwind;
    use trame::{
        IRuntime, Op, Path, Source, Trame, VRuntime, VShapeDef, VShapeHandle,
        runtime::{IHeap, IShape},
        vshape_register, vshape_store, vshape_store_reset, vshape_view,
    };

    // ============================================================================
    // Shape store generation
    // ============================================================================

    /// A recipe for building a shape - either a scalar or a struct referencing earlier shapes
    #[derive(Debug, Clone)]
    enum ShapeRecipe {
        Scalar { size: usize },
        Struct { fields: Vec<usize> }, // indices into the shape store (must be < current index)
        Option { some: usize },        // index of payload shape (must be < current index)
    }

    fn arb_shape_recipe(max_existing: usize) -> impl Strategy<Value = ShapeRecipe> {
        prop_oneof![
            // Scalar with size 1, 2, 4, or 8
            prop::sample::select(vec![1usize, 2, 4, 8])
                .prop_map(|size| ShapeRecipe::Scalar { size }),
            // Struct with 1-4 fields referencing existing shapes
            if max_existing > 0 {
                prop_oneof![
                    prop::collection::vec(0..max_existing, 1..=4)
                        .prop_map(|fields| ShapeRecipe::Struct { fields }),
                    (0..max_existing).prop_map(|some| ShapeRecipe::Option { some }),
                ]
                .boxed()
            } else {
                // No existing shapes to reference, just make a scalar
                Just(ShapeRecipe::Scalar { size: 4 }).boxed()
            },
        ]
    }

    /// Generate a sequence of shape recipes that form a valid DAG
    fn arb_shape_recipes(count: usize) -> impl Strategy<Value = Vec<ShapeRecipe>> {
        // Build recipes one at a time, each can only reference earlier ones
        let mut strat: BoxedStrategy<Vec<ShapeRecipe>> = Just(vec![]).boxed();

        for i in 0..count {
            strat = (strat, arb_shape_recipe(i))
                .prop_map(|(mut recipes, recipe)| {
                    recipes.push(recipe);
                    recipes
                })
                .boxed();
        }

        strat
    }

    /// Build the shape store from recipes, returns handles
    fn build_shape_store(recipes: &[ShapeRecipe]) -> Vec<VShapeHandle> {
        let mut handles = Vec::new();

        fn option_layout_for(payload: Layout) -> Layout {
            let align = payload.align().max(1);
            let size = (payload.size() + 1).next_multiple_of(align);
            Layout::from_size_align(size, align).expect("valid option layout")
        }

        for recipe in recipes {
            let handle = match recipe {
                ShapeRecipe::Scalar { size } => {
                    let layout = Layout::from_size_align(*size, *size).unwrap();
                    vshape_register(VShapeDef::scalar(layout))
                }
                ShapeRecipe::Struct { fields } => {
                    // Calculate offsets - pack fields sequentially with proper alignment
                    let mut offset = 0usize;
                    let mut field_defs = Vec::new();

                    for &field_idx in fields {
                        let field_handle = handles[field_idx];
                        let field_shape = vshape_view(field_handle);
                        let field_layout = field_shape.layout().unwrap();

                        // Align offset
                        let align = field_layout.align();
                        offset = (offset + align - 1) & !(align - 1);

                        field_defs.push((offset, field_handle));
                        offset += field_layout.size();
                    }

                    vshape_register(VShapeDef::struct_with_fields(vshape_store(), &field_defs))
                }
                ShapeRecipe::Option { some } => {
                    let payload_handle = handles[*some];
                    let payload_layout = vshape_view(payload_handle).layout().unwrap();
                    let option_layout = option_layout_for(payload_layout);
                    vshape_register(VShapeDef::option_of(
                        payload_handle,
                        option_layout,
                        trame::runtime::verified::VTypeOps::pod(),
                    ))
                }
            };
            handles.push(handle);
        }

        handles
    }

    // ============================================================================
    // Op generation
    // ============================================================================

    #[derive(Debug, Clone)]
    enum OpRecipe {
        SetField { field: u8, src_kind: SrcKind },
        SetRoot { src_kind: SrcKind },
        End,
    }

    #[derive(Debug, Clone)]
    enum SrcKind {
        Imm { shape_idx: usize },
        Default,
        Stage,
    }

    fn arb_src_kind(num_shapes: usize) -> impl Strategy<Value = SrcKind> {
        prop_oneof![
            (0..num_shapes).prop_map(|shape_idx| SrcKind::Imm { shape_idx }),
            Just(SrcKind::Default),
            Just(SrcKind::Stage),
        ]
    }

    fn arb_op_recipe(num_shapes: usize) -> impl Strategy<Value = OpRecipe> {
        prop_oneof![
            ((0u8..8), arb_src_kind(num_shapes))
                .prop_map(|(field, src_kind)| OpRecipe::SetField { field, src_kind }),
            arb_src_kind(num_shapes).prop_map(|src_kind| OpRecipe::SetRoot { src_kind }),
            Just(OpRecipe::End),
        ]
    }

    fn arb_op_recipes(num_shapes: usize, count: usize) -> impl Strategy<Value = Vec<OpRecipe>> {
        prop::collection::vec(arb_op_recipe(num_shapes), 0..count)
    }

    // ============================================================================
    // Test execution
    // ============================================================================

    fn with_reset<F, R>(f: F) -> R
    where
        F: FnOnce() -> R,
    {
        struct ResetGuard;
        impl Drop for ResetGuard {
            fn drop(&mut self) {
                unsafe { vshape_store_reset() };
            }
        }
        let _guard = ResetGuard;
        f()
    }

    fn run_test(
        shape_recipes: Vec<ShapeRecipe>,
        target_shape_idx: usize,
        op_recipes: Vec<OpRecipe>,
    ) {
        with_reset(|| {
            // Build the shape store
            let handles = build_shape_store(&shape_recipes);
            if handles.is_empty() {
                return;
            }

            let target_idx = target_shape_idx % handles.len();
            let target_handle = handles[target_idx];
            let target_shape = vshape_view(target_handle);

            // Allocate source values for each shape (for Imm sources)
            let mut heap = VRuntime::heap();
            let mut src_ptrs = Vec::new();
            for &h in &handles {
                let shape = vshape_view(h);
                let ptr = unsafe { heap.alloc(shape) };
                unsafe { heap.default_in_place(ptr, shape) };
                src_ptrs.push((ptr, shape));
            }

            // Create trame and apply ops
            let mut trame = unsafe { Trame::<VRuntime>::new(heap, target_shape) };

            for op in op_recipes {
                let result = match op {
                    OpRecipe::SetField { field, src_kind } => {
                        let path = Path::field(field as u32);
                        let src = match src_kind {
                            SrcKind::Imm { shape_idx } => {
                                let idx = shape_idx % handles.len();
                                let (ptr, shape) = src_ptrs[idx];
                                unsafe { Source::from_vptr(ptr, shape) }
                            }
                            SrcKind::Default => Source::default_value(),
                            SrcKind::Stage => Source::stage(None),
                        };
                        trame.apply(Op::Set { dst: path, src })
                    }
                    OpRecipe::SetRoot { src_kind } => {
                        let src = match src_kind {
                            SrcKind::Imm { shape_idx } => {
                                let idx = shape_idx % handles.len();
                                let (ptr, shape) = src_ptrs[idx];
                                unsafe { Source::from_vptr(ptr, shape) }
                            }
                            SrcKind::Default => Source::default_value(),
                            SrcKind::Stage => Source::stage(None),
                        };
                        trame.apply(Op::Set {
                            dst: Path::empty(),
                            src,
                        })
                    }
                    OpRecipe::End => trame.apply(Op::End),
                };

                // Errors are fine, panics are not
                let _ = result;
            }

            // Drop exercises cleanup paths
        });
    }

    // ============================================================================
    // Proptests
    // ============================================================================

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(1_000))]

        #[test]
        fn arbitrary_shapes_and_ops(
            shape_recipes in arb_shape_recipes(6),
            target_idx in 0usize..100,
            op_recipes in arb_op_recipes(6, 30),
        ) {
            let result = catch_unwind(std::panic::AssertUnwindSafe(|| {
                run_test(shape_recipes, target_idx, op_recipes);
            }));
            prop_assert!(result.is_ok(), "Test panicked!");
        }
    }
}

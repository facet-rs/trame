fn main() {
    println!("Run with: cargo test");
}

#[cfg(test)]
mod tests {
    use facet::Facet;
    use proptest::prelude::*;
    use std::panic::catch_unwind;
    use std::{alloc::Layout, collections::BTreeSet};
    use trame::{
        IRuntime, Op, Path, Source, Trame, VRuntime, VShapeDef, VShapeHandle,
        runtime::{IHeap, IShape},
        vshape_register, vshape_store, vshape_store_reset, vshape_view,
    };
    use trame_solver::{Schema as SolverSchema, SolveError};

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
        StageDeferred,
    }

    fn arb_src_kind(num_shapes: usize) -> impl Strategy<Value = SrcKind> {
        prop_oneof![
            (0..num_shapes).prop_map(|shape_idx| SrcKind::Imm { shape_idx }),
            Just(SrcKind::Default),
            Just(SrcKind::Stage),
            Just(SrcKind::StageDeferred),
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
                            SrcKind::StageDeferred => Source::stage_deferred(None),
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
                            SrcKind::StageDeferred => Source::stage_deferred(None),
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
    // Solver fixtures and oracle
    // ============================================================================

    #[derive(Debug, Clone, Copy)]
    enum SolverFixture {
        Struct,
        OptionalStruct,
        EnumCase,
    }

    #[derive(Debug, Facet)]
    struct FlatInner {
        a: u32,
        b: u32,
    }

    #[derive(Debug, Facet)]
    struct FlatOuter {
        id: u32,
        #[facet(flatten)]
        inner: FlatInner,
    }

    #[derive(Debug, Facet)]
    struct FlatOuterOptional {
        id: u32,
        #[facet(flatten)]
        inner: Option<FlatInner>,
    }

    #[derive(Debug, Facet)]
    #[repr(u8)]
    enum FlatChoice {
        Alpha,
        Beta,
    }

    #[derive(Debug, Facet)]
    struct FlatEnumOuter {
        id: u32,
        #[facet(flatten)]
        choice: FlatChoice,
    }

    #[derive(Debug, Clone, Copy)]
    struct OracleResolution {
        fields: &'static [&'static str],
        required: &'static [&'static str],
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum OracleOutcome {
        Ok {
            index: usize,
        },
        NoMatch {
            missing_required: BTreeSet<&'static str>,
            unknown_fields: Vec<String>,
        },
        Ambiguous,
    }

    const ORACLE_FLAT_STRUCT: [OracleResolution; 1] = [OracleResolution {
        fields: &["id", "a", "b"],
        required: &["id", "a", "b"],
    }];
    const ORACLE_FLAT_OPTIONAL_STRUCT: [OracleResolution; 1] = [OracleResolution {
        fields: &["id", "a", "b"],
        required: &["id"],
    }];
    const ORACLE_FLAT_ENUM: [OracleResolution; 2] = [
        OracleResolution {
            fields: &["id", "Alpha"],
            required: &["id", "Alpha"],
        },
        OracleResolution {
            fields: &["id", "Beta"],
            required: &["id", "Beta"],
        },
    ];

    fn solver_resolutions(fixture: SolverFixture) -> &'static [OracleResolution] {
        match fixture {
            SolverFixture::Struct => &ORACLE_FLAT_STRUCT,
            SolverFixture::OptionalStruct => &ORACLE_FLAT_OPTIONAL_STRUCT,
            SolverFixture::EnumCase => &ORACLE_FLAT_ENUM,
        }
    }

    fn fixture_known_fields(fixture: SolverFixture) -> &'static [&'static str] {
        match fixture {
            SolverFixture::Struct | SolverFixture::OptionalStruct => &["id", "a", "b"],
            SolverFixture::EnumCase => &["id", "Alpha", "Beta"],
        }
    }

    fn fixture_root_field_count(_fixture: SolverFixture) -> u32 {
        2
    }

    fn build_solver_schema(fixture: SolverFixture) -> SolverSchema<&'static facet_core::Shape> {
        match fixture {
            SolverFixture::Struct => {
                SolverSchema::build_auto(FlatOuter::SHAPE).expect("flat struct schema should build")
            }
            SolverFixture::OptionalStruct => SolverSchema::build_auto(FlatOuterOptional::SHAPE)
                .expect("optional flat struct schema should build"),
            SolverFixture::EnumCase => SolverSchema::build_auto(FlatEnumOuter::SHAPE)
                .expect("flat enum schema should build"),
        }
    }

    fn oracle_solve(fixture: SolverFixture, keys: &[String]) -> OracleOutcome {
        let resolutions = solver_resolutions(fixture);
        let all_known_fields = resolutions
            .iter()
            .flat_map(|resolution| resolution.fields.iter().copied())
            .collect::<BTreeSet<_>>();

        let unknown_fields = keys
            .iter()
            .filter(|k| !all_known_fields.contains(k.as_str()))
            .cloned()
            .collect::<Vec<_>>();

        let mut candidates = (0..resolutions.len()).collect::<Vec<_>>();
        for key in keys {
            if !all_known_fields.contains(key.as_str()) {
                continue;
            }

            let filtered = candidates
                .iter()
                .copied()
                .filter(|idx| resolutions[*idx].fields.contains(&key.as_str()))
                .collect::<Vec<_>>();
            if !filtered.is_empty() {
                candidates = filtered;
            }
        }

        if candidates.is_empty() {
            return OracleOutcome::NoMatch {
                missing_required: BTreeSet::new(),
                unknown_fields,
            };
        }

        let seen_key_set = keys.iter().map(String::as_str).collect::<BTreeSet<_>>();
        let viable = candidates
            .iter()
            .copied()
            .filter(|idx| {
                resolutions[*idx]
                    .required
                    .iter()
                    .all(|name| seen_key_set.contains(*name))
            })
            .collect::<Vec<_>>();

        match viable.len() {
            0 => {
                let (_, missing_required) = candidates
                    .iter()
                    .copied()
                    .map(|idx| {
                        let missing = resolutions[idx]
                            .required
                            .iter()
                            .copied()
                            .filter(|name| !seen_key_set.contains(*name))
                            .collect::<BTreeSet<_>>();
                        (idx, missing)
                    })
                    .min_by_key(|(_, missing)| missing.len())
                    .expect("candidates are known non-empty");

                OracleOutcome::NoMatch {
                    missing_required,
                    unknown_fields,
                }
            }
            1 => OracleOutcome::Ok { index: viable[0] },
            _ => OracleOutcome::Ambiguous,
        }
    }

    fn arb_solver_fixture() -> impl Strategy<Value = SolverFixture> {
        prop_oneof![
            Just(SolverFixture::Struct),
            Just(SolverFixture::OptionalStruct),
            Just(SolverFixture::EnumCase),
        ]
    }

    fn arb_keys_from_pool(pool: &'static [&'static str]) -> BoxedStrategy<Vec<String>> {
        prop::collection::vec(prop::sample::select(pool.to_vec()), 0..8)
            .prop_map(|keys| keys.into_iter().map(str::to_owned).collect::<Vec<_>>())
            .boxed()
    }

    fn arb_solver_fixture_and_keys() -> impl Strategy<Value = (SolverFixture, Vec<String>)> {
        arb_solver_fixture().prop_flat_map(|fixture| {
            let key_strategy = match fixture {
                SolverFixture::Struct | SolverFixture::OptionalStruct => {
                    arb_keys_from_pool(&["id", "a", "b", "unknown", "extra"])
                }
                SolverFixture::EnumCase => prop_oneof![
                    arb_keys_from_pool(&["id", "Alpha", "unknown", "extra"]),
                    arb_keys_from_pool(&["id", "Beta", "unknown", "extra"]),
                ]
                .boxed(),
            };
            key_strategy.prop_map(move |keys| (fixture, keys))
        })
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

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(1_000))]

        #[test]
        fn solver_routes_and_errors_match_oracle(
            (fixture, keys) in arb_solver_fixture_and_keys(),
        ) {
            let schema = build_solver_schema(fixture);
            let oracle = oracle_solve(fixture, &keys);
            let solve = schema.solve_keys(keys.iter().map(String::as_str));

            match (solve, oracle) {
                (Ok(resolved), OracleOutcome::Ok { index }) => {
                    prop_assert_eq!(resolved.index(), index);

                    let stable = schema
                        .solve_keys(keys.iter().map(String::as_str))
                        .expect("re-solving should stay successful");
                    prop_assert_eq!(stable.index(), resolved.index());

                    for key in keys.iter().filter(|k| fixture_known_fields(fixture).contains(&k.as_str())) {
                        let Some(route) = resolved.resolution().field_by_name(key.as_str()) else {
                            prop_assert!(false, "known key `{}` had no route in resolved schema", key);
                            continue;
                        };
                        let Some(top_idx) = route.top_level_field_index() else {
                            prop_assert!(false, "route for key `{}` had no top-level field index", key);
                            continue;
                        };
                        prop_assert!(top_idx < fixture_root_field_count(fixture));
                        prop_assert!(!route.path_display.is_empty());

                        let stable_route = stable
                            .resolution()
                            .field_by_name(key.as_str())
                            .expect("stable solve must return same route");
                        prop_assert_eq!(&stable_route.path_display, &route.path_display);
                    }
                }
                (
                    Err(SolveError::NoMatch {
                        missing_required,
                        missing_required_detailed,
                        unknown_fields,
                    }),
                    OracleOutcome::NoMatch {
                        missing_required: oracle_missing_required,
                        unknown_fields: oracle_unknown_fields,
                    },
                ) => {
                    let seen_keys = keys.iter().map(String::as_str).collect::<BTreeSet<_>>();

                    for field in &missing_required {
                        prop_assert!(!seen_keys.contains(field));
                    }
                    for info in &missing_required_detailed {
                        prop_assert!(!seen_keys.contains(info.name));
                        prop_assert!(!info.path.is_empty());
                    }

                    let missing_required_set = missing_required.into_iter().collect::<BTreeSet<_>>();
                    prop_assert_eq!(missing_required_set, oracle_missing_required);
                    prop_assert_eq!(unknown_fields, oracle_unknown_fields);
                }
                (Err(SolveError::Ambiguous { .. }), OracleOutcome::Ambiguous) => {}
                (actual, expected) => {
                    prop_assert!(
                        false,
                        "solver/oracle mismatch for fixture {:?} and keys {:?}: actual={:?}, expected={:?}",
                        fixture,
                        keys,
                        actual,
                        expected
                    );
                }
            }
        }
    }
}

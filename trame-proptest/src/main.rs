use proptest::prelude::*;
use std::alloc::Layout;
use std::panic::{AssertUnwindSafe, catch_unwind};
use trame::{
    IRuntime, Op, PathSegment, Source, Trame, VRuntime, VShapeDef, VShapeHandle, runtime::IHeap,
    vshape_register, vshape_store, vshape_store_reset, vshape_view,
};

// ============================================================================
// Op generation
// ============================================================================

#[derive(Debug, Clone)]
enum TestOp {
    SetFieldImm { field: u8 },
    SetFieldDefault { field: u8 },
    SetFieldStage { field: u8 },
    SetRootImm,
    SetRootDefault,
    End,
}

fn arb_op() -> impl Strategy<Value = TestOp> {
    prop_oneof![
        (0u8..8).prop_map(|field| TestOp::SetFieldImm { field }),
        (0u8..8).prop_map(|field| TestOp::SetFieldDefault { field }),
        (0u8..8).prop_map(|field| TestOp::SetFieldStage { field }),
        Just(TestOp::SetRootImm),
        Just(TestOp::SetRootDefault),
        Just(TestOp::End),
    ]
}

fn arb_ops() -> impl Strategy<Value = Vec<TestOp>> {
    prop::collection::vec(arb_op(), 0..20)
}

// ============================================================================
// Test runner
// ============================================================================

/// Run a test case, always resetting the shape store afterward (even on panic)
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

fn run_test<F>(f: F)
where
    F: FnOnce() + std::panic::UnwindSafe,
{
    let result = catch_unwind(|| {
        with_reset(f);
    });
    if let Err(e) = result {
        std::panic::resume_unwind(e);
    }
}

fn apply_ops(shape_h: VShapeHandle, scalar_h: VShapeHandle, ops: Vec<TestOp>) {
    let shape = vshape_view(shape_h);
    let scalar_shape = vshape_view(scalar_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(scalar_shape) };
    unsafe { heap.default_in_place(src, scalar_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    for op in ops {
        let result = match op {
            TestOp::SetFieldImm { field } => {
                let path = [PathSegment::Field(field as u32)];
                trame.apply(Op::Set {
                    dst: &path,
                    src: unsafe { Source::from_vptr(src, scalar_shape) },
                })
            }
            TestOp::SetFieldDefault { field } => {
                let path = [PathSegment::Field(field as u32)];
                trame.apply(Op::Set {
                    dst: &path,
                    src: Source::default_value(),
                })
            }
            TestOp::SetFieldStage { field } => {
                let path = [PathSegment::Field(field as u32)];
                trame.apply(Op::Set {
                    dst: &path,
                    src: Source::stage(None),
                })
            }
            TestOp::SetRootImm => trame.apply(Op::Set {
                dst: &[],
                src: unsafe { Source::from_vptr(src, scalar_shape) },
            }),
            TestOp::SetRootDefault => trame.apply(Op::Set {
                dst: &[],
                src: Source::default_value(),
            }),
            TestOp::End => trame.apply(Op::End),
        };

        // Errors are fine, panics are not
        let _ = result;
    }
}

// ============================================================================
// Proptests
// ============================================================================

proptest! {
    #[test]
    fn arbitrary_ops_on_scalar(ops in arb_ops()) {
        run_test(move || {
            let scalar_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
            apply_ops(scalar_h, scalar_h, ops);
        });
    }

    #[test]
    fn arbitrary_ops_on_struct(
        field_count in 1usize..=4,
        ops in arb_ops()
    ) {
        run_test(move || {
            let scalar_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
            let fields: Vec<(usize, VShapeHandle)> = (0..field_count)
                .map(|i| (i * 4, scalar_h))
                .collect();
            let struct_h = vshape_register(VShapeDef::struct_with_fields(vshape_store(), &fields));
            apply_ops(struct_h, scalar_h, ops);
        });
    }

    #[test]
    fn arbitrary_ops_on_nested_struct(
        outer_fields in 1usize..=3,
        inner_fields in 1usize..=3,
        ops in arb_ops()
    ) {
        run_test(move || {
            let scalar_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));

            // Inner struct
            let inner_field_defs: Vec<(usize, VShapeHandle)> = (0..inner_fields)
                .map(|i| (i * 4, scalar_h))
                .collect();
            let inner_h = vshape_register(VShapeDef::struct_with_fields(vshape_store(), &inner_field_defs));

            // Outer struct: first field is inner struct, rest are scalars
            let mut outer_field_defs = vec![(0, inner_h)];
            let inner_size = inner_fields * 4;
            for i in 1..outer_fields {
                outer_field_defs.push((inner_size + (i - 1) * 4, scalar_h));
            }
            let outer_h = vshape_register(VShapeDef::struct_with_fields(vshape_store(), &outer_field_defs));

            apply_ops(outer_h, scalar_h, ops);
        });
    }
}

fn main() {
    println!("Run with: cargo test");
}

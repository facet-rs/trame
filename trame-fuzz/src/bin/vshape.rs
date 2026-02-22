#[cfg(not(feature = "standalone"))]
use afl::fuzz;
use arbitrary::Arbitrary;
use core::alloc::Layout;
use trame::runtime::verified::{VPointerVTable, VShapeView, VTypeOps};
use trame::{
    IRuntime, LRuntime, Op, Path, PathSegment, Source, Trame, VShapeDef, VShapeStore,
    vshape_register, vshape_store, vshape_store_reset, vshape_view,
};

#[derive(Clone, Copy, Arbitrary)]
pub enum FuzzScenario {
    Scalar,
    NestedStruct,
    Pointer,
    Option,
    List,
    Map,
}

#[derive(Clone, Copy, Arbitrary)]
pub enum FuzzPathSegment {
    Field(u8),
    Append,
    Root,
}

impl From<FuzzPathSegment> for PathSegment {
    fn from(seg: FuzzPathSegment) -> Self {
        match seg {
            FuzzPathSegment::Field(n) => PathSegment::Field(n as u32),
            FuzzPathSegment::Append => PathSegment::Append,
            FuzzPathSegment::Root => PathSegment::Root,
        }
    }
}

#[derive(Clone, Arbitrary)]
pub enum FuzzSource {
    Default,
    Stage { len_hint: Option<u8> },
    StageDeferred { len_hint: Option<u8> },
}

#[derive(Clone, Arbitrary)]
pub enum FuzzOp {
    Set {
        path: Vec<FuzzPathSegment>,
        source: FuzzSource,
    },
    End,
}

#[derive(Clone, Arbitrary)]
pub struct FuzzInput {
    pub scenario: FuzzScenario,
    pub ops: Vec<FuzzOp>,
}

struct FreshStore;

impl FreshStore {
    fn new() -> Self {
        unsafe { vshape_store_reset() };
        Self
    }
}

impl Drop for FreshStore {
    fn drop(&mut self) {
        if !std::thread::panicking() {
            unsafe { vshape_store_reset() };
        }
    }
}

unsafe fn drop_noop(_ptr: *mut u8) {}

unsafe fn default_magic(ptr: *mut u8) -> bool {
    unsafe {
        ptr.cast::<usize>().write(0xA11C_E555);
    }
    true
}

unsafe fn pointer_copy_new_into(dst: *mut u8, src: *mut u8) {
    let v = unsafe { src.cast::<usize>().read() };
    unsafe {
        dst.cast::<usize>().write(v);
    }
}

fn build_scenario_shape(scenario: FuzzScenario) -> VShapeView<'static, VShapeStore> {
    match scenario {
        FuzzScenario::Scalar => {
            let root = vshape_register(VShapeDef::scalar_with_ops(
                Layout::new::<usize>(),
                VTypeOps::new(false, drop_noop, Some(default_magic)),
            ));
            vshape_view(root)
        }
        FuzzScenario::NestedStruct => {
            let scalar = vshape_register(VShapeDef::scalar_with_ops(
                Layout::new::<usize>(),
                VTypeOps::new(false, drop_noop, Some(default_magic)),
            ));
            let inner = vshape_register(VShapeDef::struct_with_fields(
                vshape_store(),
                &[(0, scalar), (core::mem::size_of::<usize>(), scalar)],
            ));
            let root = vshape_register(VShapeDef::struct_with_fields(
                vshape_store(),
                &[(0, inner), (core::mem::size_of::<[usize; 2]>(), scalar)],
            ));
            vshape_view(root)
        }
        FuzzScenario::Pointer => {
            let pointee = vshape_register(VShapeDef::scalar_with_ops(
                Layout::new::<usize>(),
                VTypeOps::new(false, drop_noop, Some(default_magic)),
            ));
            let pointer = vshape_register(VShapeDef::pointer_to_with_ops(
                pointee,
                true,
                true,
                VTypeOps::pod(),
                VPointerVTable::new(Some(pointer_copy_new_into)),
            ));
            let root = vshape_register(VShapeDef::struct_with_fields(
                vshape_store(),
                &[(0, pointer)],
            ));
            vshape_view(root)
        }
        FuzzScenario::Option => {
            let payload = vshape_register(VShapeDef::scalar_with_ops(
                Layout::new::<usize>(),
                VTypeOps::new(false, drop_noop, Some(default_magic)),
            ));
            let option = vshape_register(VShapeDef::option_of(
                payload,
                Layout::new::<Option<usize>>(),
                VTypeOps::pod(),
            ));
            vshape_view(option)
        }
        FuzzScenario::List => {
            let elem = vshape_register(VShapeDef::scalar_with_ops(
                Layout::new::<usize>(),
                VTypeOps::new(false, drop_noop, Some(default_magic)),
            ));
            let list = vshape_register(VShapeDef::list_of(
                elem,
                Layout::new::<Vec<usize>>(),
                VTypeOps::pod(),
            ));
            vshape_view(list)
        }
        FuzzScenario::Map => {
            let key = vshape_register(VShapeDef::scalar_with_ops(
                Layout::new::<usize>(),
                VTypeOps::new(false, drop_noop, Some(default_magic)),
            ));
            let value = vshape_register(VShapeDef::scalar_with_ops(
                Layout::new::<usize>(),
                VTypeOps::new(false, drop_noop, Some(default_magic)),
            ));
            let map = vshape_register(VShapeDef::map_of(
                key,
                value,
                Layout::new::<usize>(),
                VTypeOps::pod(),
            ));
            vshape_view(map)
        }
    }
}

#[cfg(not(feature = "standalone"))]
fn main() {
    fuzz!(|input: FuzzInput| {
        run_fuzz(input);
    });
}

#[cfg(feature = "standalone")]
fn main() {
    use arbitrary::Unstructured;
    use std::io::Read;

    let mut data = Vec::new();
    std::io::stdin().read_to_end(&mut data).unwrap();
    if let Ok(input) = FuzzInput::arbitrary(&mut Unstructured::new(&data)) {
        run_fuzz(input);
    }
}

fn run_fuzz(input: FuzzInput) {
    let _store = FreshStore::new();
    let root_shape = build_scenario_shape(input.scenario);

    type LiveVShapeRuntime = LRuntime<VShapeView<'static, VShapeStore>>;
    let heap = LiveVShapeRuntime::heap();
    let mut trame = unsafe { Trame::<LiveVShapeRuntime>::new(heap, root_shape) };

    for op in input.ops {
        match op {
            FuzzOp::Set { path, source } => {
                let path_buf: Vec<PathSegment> = path.into_iter().map(Into::into).collect();
                let path = Path::from_segments(&path_buf);
                let result = match source {
                    FuzzSource::Default => trame.apply(Op::Set {
                        dst: path,
                        src: Source::default_value(),
                    }),
                    FuzzSource::Stage { len_hint } => trame.apply(Op::Set {
                        dst: path,
                        src: Source::stage(len_hint.map(|n| n as usize)),
                    }),
                    FuzzSource::StageDeferred { len_hint } => trame.apply(Op::Set {
                        dst: path,
                        src: Source::stage_deferred(len_hint.map(|n| n as usize)),
                    }),
                };
                if result.is_err() {
                    return;
                }
            }
            FuzzOp::End => {
                if trame.apply(Op::End).is_err() {
                    return;
                }
            }
        }
    }

    let _ = trame.build();
}

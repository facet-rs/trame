#[cfg(not(feature = "standalone"))]
use afl::fuzz;
use arbitrary::Arbitrary;
use facet::Facet;
use facet_core::Shape;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use trame::{IRuntime, LRuntime, Op, Path, PathSegment, Source, Trame};

// ============================================================================
// Compound types for fuzzing
// ============================================================================

#[derive(Clone, Debug, Facet, Arbitrary)]
pub struct Point {
    x: Box<i32>,
    y: Box<i32>,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
pub struct Nested {
    name: String,
    point: Point,
    value: Box<u64>,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
pub struct WithOption {
    required: Box<u32>,
    optional: Option<String>,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
pub struct WithVec {
    items: Vec<Box<u32>>,
    label: String,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
pub struct WithResult {
    id: u32,
    outcome: Result<String, i32>,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
pub struct WithNestedOption {
    outer: Option<Option<u32>>,
    name: String,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
pub struct WithNestedResult {
    inner: Result<Result<u32, String>, i32>,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
pub struct WithOptionVec {
    items: Option<Vec<u32>>,
    count: u32,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
pub struct WithResultVec {
    data: Result<Vec<u32>, String>,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
pub struct WithSets {
    hash_set: HashSet<u32>,
    btree_set: BTreeSet<String>,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
pub struct WithArrays {
    small: [u32; 3],
    medium: [u8; 8],
}

// ============================================================================
// Enum types for fuzzing
// ============================================================================

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(u8)]
pub enum UnitEnumU8 {
    A,
    B,
    C,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(u16)]
pub enum UnitEnumU16 {
    X,
    Y,
    Z,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(u32)]
pub enum UnitEnumU32 {
    One,
    Two,
    Three,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(u64)]
pub enum UnitEnumU64 {
    Alpha,
    Beta,
    Gamma,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(i8)]
pub enum UnitEnumI8 {
    Neg,
    Zero,
    Pos,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(i16)]
pub enum UnitEnumI16 {
    Low,
    Mid,
    High,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(i32)]
pub enum UnitEnumI32 {
    Small,
    Medium,
    Large,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(i64)]
pub enum UnitEnumI64 {
    Past,
    Present,
    Future,
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(u8)]
pub enum DataEnumU8 {
    Empty,
    WithU32(u32),
    WithString(String),
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(u16)]
pub enum DataEnumU16 {
    None,
    Bool(bool),
    Pair(u32, u32),
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(u32)]
pub enum DataEnumU32 {
    Vacant,
    Single(i64),
    Double(String, String),
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(i8)]
pub enum DataEnumI8 {
    Nothing,
    Something(u8),
    Everything(Vec<u8>),
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(i32)]
pub enum DataEnumI32 {
    Nil,
    Value(f64),
    Values(Vec<f64>),
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(u8)]
pub enum MixedEnumU8 {
    Unit,
    Tuple(u32, String),
    Struct { x: i32, y: i32 },
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(u32)]
pub enum MixedEnumU32 {
    Empty,
    Wrapped(Box<u32>),
    Named { value: Option<String> },
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(u16)]
pub enum NestedEnumU8 {
    Leaf(u8),
    Branch(Box<UnitEnumU8>),
    Tree { left: u8, right: u8 },
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(u32)]
pub enum NestedEnumU32 {
    Leaf(u32),
    Branch(Box<UnitEnumU32>),
    Tree { left: u32, right: u32 },
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(i32)]
pub enum NestedEnumI32 {
    Leaf(i32),
    Branch(Box<UnitEnumI32>),
    Tree { left: i32, right: i32 },
}

#[derive(Clone, Debug, Facet, Arbitrary)]
#[repr(u16)]
pub enum DeepEnum {
    Leaf(u32),
    Branch(Box<NestedEnumU8>),
    Tree {
        left: NestedEnumU32,
        right: NestedEnumI32,
    },
}

// ============================================================================
// Macro to generate FuzzValue and FuzzTargetType
// ============================================================================

macro_rules! fuzz_types {
    (
        values {
            $(
                $val_variant:ident => $val_type:ty
            ),* $(,)?
        }
        targets_only {
            $(
                $tgt_variant:ident => $tgt_type:ty
            ),* $(,)?
        }
    ) => {
        #[derive(Clone, Arbitrary)]
        pub enum FuzzValue {
            $( $val_variant($val_type), )*
        }

        impl FuzzValue {
            fn as_ptr_and_shape(&mut self) -> (*mut u8, &'static Shape) {
                match self {
                    $( FuzzValue::$val_variant(v) => (v as *mut $val_type as *mut u8, <$val_type>::SHAPE), )*
                }
            }
        }

        #[derive(Debug, Clone, Copy, Arbitrary)]
        pub enum FuzzTargetType {
            $( $val_variant, )*
            $( $tgt_variant, )*
        }

        impl FuzzTargetType {
            fn shape(&self) -> &'static Shape {
                match self {
                    $( FuzzTargetType::$val_variant => <$val_type>::SHAPE, )*
                    $( FuzzTargetType::$tgt_variant => <$tgt_type>::SHAPE, )*
                }
            }
        }
    };
}

fuzz_types! {
    values {
        // Scalars
        Bool => bool,
        U8 => u8,
        U16 => u16,
        U32 => u32,
        U64 => u64,
        U128 => u128,
        Usize => usize,
        I8 => i8,
        I16 => i16,
        I32 => i32,
        I64 => i64,
        I128 => i128,
        Isize => isize,
        F32 => f32,
        F64 => f64,
        Char => char,
        String => String,

        // Custom structs
        Point => Point,
        Nested => Nested,
        WithOption => WithOption,
        WithVec => WithVec,
        WithResult => WithResult,
        WithNestedOption => WithNestedOption,
        WithNestedResult => WithNestedResult,
        WithOptionVec => WithOptionVec,
        WithResultVec => WithResultVec,
        WithSets => WithSets,
        WithArrays => WithArrays,

        // Option (basic)
        OptionU32 => Option<u32>,
        OptionString => Option<String>,
        OptionBool => Option<bool>,
        OptionPoint => Option<Point>,
        OptionVecU32 => Option<Vec<u32>>,
        OptionBoxU32 => Option<Box<u32>>,

        // Option (nested)
        OptionOptionU32 => Option<Option<u32>>,
        OptionOptionString => Option<Option<String>>,

        // Result (basic)
        ResultU32 => Result<u32, i32>,
        ResultString => Result<String, i32>,
        ResultPoint => Result<Point, i32>,

        // Collections
        VecU32 => Vec<u32>,
        VecString => Vec<String>,
        VecPoint => Vec<Point>,
        VecVecU32 => Vec<Vec<u32>>,
        HashMapStringU32 => HashMap<String, u32>,
        BTreeMapStringU32 => BTreeMap<String, u32>,
        HashSetU32 => HashSet<u32>,
        BTreeSetString => BTreeSet<String>,

        // Arrays and tuples
        ArrayU8_8 => [u8; 8],
        ArrayU32_4 => [u32; 4],
        TupleU32U32 => (u32, u32),
        TupleStringU32 => (String, u32),

        // Enums
        UnitEnumU8 => UnitEnumU8,
        UnitEnumU16 => UnitEnumU16,
        UnitEnumU32 => UnitEnumU32,
        UnitEnumU64 => UnitEnumU64,
        UnitEnumI8 => UnitEnumI8,
        UnitEnumI16 => UnitEnumI16,
        UnitEnumI32 => UnitEnumI32,
        UnitEnumI64 => UnitEnumI64,
        DataEnumU8 => DataEnumU8,
        DataEnumU16 => DataEnumU16,
        DataEnumU32 => DataEnumU32,
        DataEnumI8 => DataEnumI8,
        DataEnumI32 => DataEnumI32,
        MixedEnumU8 => MixedEnumU8,
        MixedEnumU32 => MixedEnumU32,
        NestedEnumU8 => NestedEnumU8,
        NestedEnumU32 => NestedEnumU32,
        NestedEnumI32 => NestedEnumI32,
        DeepEnum => DeepEnum,
    }
    targets_only {
        Unit => (),
    }
}

// ============================================================================
// FuzzOp
// ============================================================================

#[derive(Clone, Arbitrary)]
pub enum FuzzSource {
    Imm(FuzzValue),
    Default,
    Stage { len_hint: Option<u8> },
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
pub enum FuzzOp {
    Set {
        path: Vec<FuzzPathSegment>,
        source: FuzzSource,
    },
    End,
}

#[derive(Clone, Arbitrary)]
pub struct FuzzInput {
    pub target: FuzzTargetType,
    pub ops: Vec<FuzzOp>,
}

// ============================================================================
// Main fuzz target
// ============================================================================

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
    let target_shape = input.target.shape();
    let heap = LRuntime::heap();
    let mut trame = unsafe { Trame::<LRuntime>::new(heap, target_shape) };

    for op in input.ops {
        match op {
            FuzzOp::Set { path, mut source } => {
                let path_buf: Vec<PathSegment> = path.into_iter().map(Into::into).collect();
                let path = Path::from_segments(&path_buf);
                let result = match &mut source {
                    FuzzSource::Imm(value) => {
                        // Get the shape from the FuzzValue
                        let (ptr, shape) = value.as_ptr_and_shape();
                        trame.apply(Op::Set {
                            dst: path.clone(),
                            src: unsafe { Source::from_ptr_shape(ptr, shape) },
                        })
                    }
                    FuzzSource::Default => trame.apply(Op::Set {
                        dst: path.clone(),
                        src: Source::default_value(),
                    }),
                    FuzzSource::Stage { len_hint } => trame.apply(Op::Set {
                        dst: path,
                        src: Source::stage(len_hint.map(|n| n as usize)),
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
}

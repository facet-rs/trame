#[cfg(not(feature = "standalone"))]
use afl::fuzz;
use arbitrary::Arbitrary;
use facet::Facet;
use facet_core::Shape;
use std::collections::BTreeSet;
use trame_solver::{Schema as SolverSchema, SolveError};

#[derive(Debug, Clone, Copy, Arbitrary)]
enum SolverFixture {
    Struct,
    OptionalStruct,
    EnumCase,
}

#[derive(Debug, Clone, Arbitrary)]
enum FuzzKey {
    Id,
    A,
    B,
    Alpha,
    Beta,
    Empty,
    Unknown([u8; 8]),
}

impl FuzzKey {
    fn into_key(self) -> String {
        match self {
            Self::Id => "id".to_owned(),
            Self::A => "a".to_owned(),
            Self::B => "b".to_owned(),
            Self::Alpha => "Alpha".to_owned(),
            Self::Beta => "Beta".to_owned(),
            Self::Empty => String::new(),
            Self::Unknown(bytes) => format!("x{:02x?}", bytes),
        }
    }
}

#[derive(Debug, Clone, Arbitrary)]
struct FuzzInput {
    fixture: SolverFixture,
    keys: Vec<FuzzKey>,
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
    std::io::stdin()
        .read_to_end(&mut data)
        .expect("stdin should be readable");
    if let Ok(input) = FuzzInput::arbitrary(&mut Unstructured::new(&data)) {
        run_fuzz(input);
    }
}

fn build_schema(fixture: SolverFixture) -> SolverSchema<&'static Shape> {
    match fixture {
        SolverFixture::Struct => {
            SolverSchema::build_auto(FlatOuter::SHAPE).expect("schema should build")
        }
        SolverFixture::OptionalStruct => {
            SolverSchema::build_auto(FlatOuterOptional::SHAPE).expect("schema should build")
        }
        SolverFixture::EnumCase => {
            SolverSchema::build_auto(FlatEnumOuter::SHAPE).expect("schema should build")
        }
    }
}

fn known_fields(fixture: SolverFixture) -> &'static [&'static str] {
    match fixture {
        SolverFixture::Struct | SolverFixture::OptionalStruct => &["id", "a", "b"],
        SolverFixture::EnumCase => &["id", "Alpha", "Beta"],
    }
}

fn root_field_count(_fixture: SolverFixture) -> u32 {
    2
}

fn run_fuzz(input: FuzzInput) {
    let schema = build_schema(input.fixture);
    let keys = input
        .keys
        .into_iter()
        .map(FuzzKey::into_key)
        .collect::<Vec<_>>();

    match schema.solve_keys(keys.iter().map(String::as_str)) {
        Ok(resolved) => {
            let stable = schema
                .solve_keys(keys.iter().map(String::as_str))
                .expect("solver should be deterministic for same input");
            assert_eq!(stable.index(), resolved.index());

            for key in keys
                .iter()
                .filter(|key| known_fields(input.fixture).contains(&key.as_str()))
            {
                let Some(route) = resolved.resolution().field_by_name(key.as_str()) else {
                    continue;
                };
                let top_idx = route
                    .top_level_field_index()
                    .expect("resolved route should include top-level field index");
                assert!(top_idx < root_field_count(input.fixture));
                assert!(!route.path_display.is_empty());

                let stable_route = stable
                    .resolution()
                    .field_by_name(key.as_str())
                    .expect("if the first solve yielded a route for key, stable solve should too");
                assert_eq!(stable_route.path, route.path);
                assert_eq!(stable_route.path_display, route.path_display);
            }
        }
        Err(SolveError::NoMatch {
            missing_required,
            missing_required_detailed,
            ..
        }) => {
            let seen = keys.iter().map(String::as_str).collect::<BTreeSet<_>>();
            for missing in missing_required {
                assert!(!seen.contains(missing));
            }
            for info in missing_required_detailed {
                assert!(!seen.contains(info.name));
                assert!(!info.path.is_empty());
            }
        }
        Err(SolveError::Ambiguous { candidates, .. }) => {
            assert!(!candidates.is_empty());
        }
    }
}

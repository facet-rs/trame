#![doc = include_str!("../README.md")]

use std::collections::BTreeMap;

use facet_core::Shape;
use facet_solver::{
    FieldInfo as FacetFieldInfo, MissingFieldInfo as FacetMissingFieldInfo,
    PathSegment as FacetPathSegment, Schema as FacetSchema, SchemaError as FacetSchemaError,
    Solver as FacetSolver, SolverError as FacetSolverError,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathSegment {
    Field(&'static str),
    Variant {
        field: &'static str,
        variant: &'static str,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldRoute {
    pub serialized_name: &'static str,
    pub path: Vec<PathSegment>,
    pub required: bool,
}

impl FieldRoute {
    pub fn top_level_field_name(&self) -> Option<&'static str> {
        self.path.iter().find_map(|segment| match segment {
            PathSegment::Field(name) => Some(*name),
            PathSegment::Variant { .. } => None,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Resolution {
    fields_by_name: BTreeMap<&'static str, FieldRoute>,
}

impl Resolution {
    pub fn field_by_name(&self, name: &str) -> Option<&FieldRoute> {
        self.fields_by_name.get(name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingFieldInfo {
    pub name: &'static str,
    pub path: String,
    pub defined_in: String,
}

impl From<FacetMissingFieldInfo> for MissingFieldInfo {
    fn from(value: FacetMissingFieldInfo) -> Self {
        Self {
            name: value.name,
            path: value.path,
            defined_in: value.defined_in,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolveError {
    SchemaBuildFailed,
    NoMatch {
        missing_required: Vec<&'static str>,
        missing_required_detailed: Vec<MissingFieldInfo>,
        unknown_fields: Vec<String>,
    },
    Ambiguous {
        candidates: Vec<String>,
        disambiguating_fields: Vec<String>,
    },
}

impl SolveError {
    fn from_facet(value: FacetSolverError) -> Self {
        match value {
            FacetSolverError::NoMatch {
                missing_required,
                missing_required_detailed,
                unknown_fields,
                ..
            } => Self::NoMatch {
                missing_required,
                missing_required_detailed: missing_required_detailed
                    .into_iter()
                    .map(MissingFieldInfo::from)
                    .collect(),
                unknown_fields,
            },
            FacetSolverError::Ambiguous {
                candidates,
                disambiguating_fields,
            } => Self::Ambiguous {
                candidates,
                disambiguating_fields,
            },
        }
    }
}

#[derive(Debug)]
pub struct Schema {
    shape: &'static Shape,
    resolutions: Vec<Resolution>,
}

impl Schema {
    pub fn build_auto(shape: &'static Shape) -> Result<Self, FacetSchemaError> {
        let facet_schema = FacetSchema::build_auto(shape)?;
        let resolutions = facet_schema
            .resolutions()
            .iter()
            .map(|resolution| {
                let fields_by_name = resolution
                    .fields()
                    .values()
                    .map(field_route_from_facet)
                    .map(|route| (route.serialized_name, route))
                    .collect::<BTreeMap<_, _>>();
                Resolution { fields_by_name }
            })
            .collect();

        Ok(Self { shape, resolutions })
    }

    pub fn solve_keys<'a, I, K>(&'a self, keys: I) -> Result<Resolved<'a>, SolveError>
    where
        I: IntoIterator<Item = K>,
        K: AsRef<str>,
    {
        let facet_schema =
            FacetSchema::build_auto(self.shape).map_err(|_| SolveError::SchemaBuildFailed)?;
        let mut solver = FacetSolver::new(&facet_schema);
        let owned_keys: Vec<String> = keys
            .into_iter()
            .map(|key| key.as_ref().to_owned())
            .collect();
        for key in &owned_keys {
            let _ = solver.see_key(key);
        }

        let resolved = solver.finish().map_err(SolveError::from_facet)?;
        let index = resolved.index();
        let resolution = self
            .resolutions
            .get(index)
            .expect("facet solver returned an invalid resolution index");

        Ok(Resolved { index, resolution })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Resolved<'a> {
    index: usize,
    resolution: &'a Resolution,
}

impl<'a> Resolved<'a> {
    pub const fn index(self) -> usize {
        self.index
    }

    pub const fn resolution(self) -> &'a Resolution {
        self.resolution
    }
}

fn field_route_from_facet(info: &FacetFieldInfo) -> FieldRoute {
    let path = info
        .path
        .segments()
        .iter()
        .map(|segment| match segment {
            FacetPathSegment::Field(name) => PathSegment::Field(name),
            FacetPathSegment::Variant(field, variant) => PathSegment::Variant { field, variant },
        })
        .collect();

    FieldRoute {
        serialized_name: info.serialized_name,
        path,
        required: info.required,
    }
}

#[cfg(test)]
mod tests {
    use facet::Facet;

    use super::{PathSegment, Schema};

    #[derive(Facet)]
    struct Inner {
        a: u32,
        b: String,
    }

    #[derive(Facet)]
    struct Outer {
        id: u32,
        #[facet(flatten)]
        inner: Inner,
    }

    #[test]
    fn routes_flattened_fields_to_top_level_parent() {
        let schema = Schema::build_auto(Outer::SHAPE).expect("schema should build");
        let resolved = schema
            .solve_keys(["id", "a", "b"])
            .expect("keys should resolve");

        let route = resolved
            .resolution()
            .field_by_name("a")
            .expect("field should exist");
        assert_eq!(route.top_level_field_name(), Some("inner"));
        assert!(matches!(
            route.path.as_slice(),
            [PathSegment::Field("inner"), PathSegment::Field("a")]
        ));
    }
}

#![doc = include_str!("../README.md")]

use std::collections::{BTreeMap, BTreeSet};

use facet_core::{Def, Field, Shape, StructType, Type, UserType, Variant};

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
    pub defined_in: &'static str,
}

impl FieldRoute {
    pub fn top_level_field_name(&self) -> Option<&'static str> {
        self.path.iter().find_map(|segment| match segment {
            PathSegment::Field(name) => Some(*name),
            PathSegment::Variant { .. } => None,
        })
    }

    fn path_string(&self) -> String {
        path_to_string(&self.path)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingFieldInfo {
    pub name: &'static str,
    pub path: String,
    pub defined_in: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SchemaError {
    DuplicateField {
        field_name: &'static str,
        first_path: String,
        second_path: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolveError {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Resolution {
    fields_by_name: BTreeMap<&'static str, FieldRoute>,
    required_fields: BTreeSet<&'static str>,
    variant_selections: Vec<VariantSelection>,
}

impl Resolution {
    fn new() -> Self {
        Self {
            fields_by_name: BTreeMap::new(),
            required_fields: BTreeSet::new(),
            variant_selections: Vec::new(),
        }
    }

    pub fn field_by_name(&self, name: &str) -> Option<&FieldRoute> {
        self.fields_by_name.get(name)
    }

    fn add_variant_selection(&mut self, enum_name: &'static str, variant_name: &'static str) {
        self.variant_selections.push(VariantSelection {
            enum_name,
            variant_name,
        });
    }

    fn add_field(&mut self, route: FieldRoute) -> Result<(), SchemaError> {
        if let Some(existing) = self.fields_by_name.get_mut(route.serialized_name) {
            if existing.path != route.path {
                return Err(SchemaError::DuplicateField {
                    field_name: route.serialized_name,
                    first_path: existing.path_string(),
                    second_path: route.path_string(),
                });
            }
            existing.required |= route.required;
            if existing.required {
                self.required_fields.insert(route.serialized_name);
            }
            return Ok(());
        }

        if route.required {
            self.required_fields.insert(route.serialized_name);
        }
        self.fields_by_name.insert(route.serialized_name, route);
        Ok(())
    }

    fn merge(&mut self, other: &Resolution) -> Result<(), SchemaError> {
        for route in other.fields_by_name.values() {
            self.add_field(route.clone())?;
        }
        for sel in &other.variant_selections {
            self.variant_selections.push(sel.clone());
        }
        Ok(())
    }

    fn mark_all_optional(&mut self) {
        self.required_fields.clear();
        for route in self.fields_by_name.values_mut() {
            route.required = false;
        }
    }

    fn describe(&self) -> String {
        if self.variant_selections.is_empty() {
            return String::from("(no variants)");
        }

        self.variant_selections
            .iter()
            .map(|sel| format!("{}::{}", sel.enum_name, sel.variant_name))
            .collect::<Vec<_>>()
            .join(" + ")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Schema {
    resolutions: Vec<Resolution>,
    all_known_fields: BTreeSet<&'static str>,
}

impl Schema {
    pub fn build_auto(shape: &'static Shape) -> Result<Self, SchemaError> {
        let resolutions = match shape.ty {
            Type::User(UserType::Struct(st)) => analyze_struct(st, Vec::new())?,
            _ => vec![Resolution::new()],
        };

        let all_known_fields = resolutions
            .iter()
            .flat_map(|resolution| resolution.fields_by_name.keys().copied())
            .collect();

        Ok(Self {
            resolutions,
            all_known_fields,
        })
    }

    pub fn solve_keys<'a, I, K>(&'a self, keys: I) -> Result<Resolved<'a>, SolveError>
    where
        I: IntoIterator<Item = K>,
        K: AsRef<str>,
    {
        let seen_keys = keys
            .into_iter()
            .map(|k| k.as_ref().to_owned())
            .collect::<Vec<_>>();

        let unknown_fields = seen_keys
            .iter()
            .filter(|k| !self.all_known_fields.contains(k.as_str()))
            .cloned()
            .collect::<Vec<_>>();

        let seen_key_set = seen_keys
            .iter()
            .map(String::as_str)
            .collect::<BTreeSet<_>>();

        let mut candidates = (0..self.resolutions.len()).collect::<Vec<_>>();
        for key in &seen_keys {
            if !self.all_known_fields.contains(key.as_str()) {
                continue;
            }

            let filtered = candidates
                .iter()
                .copied()
                .filter(|idx| {
                    self.resolutions[*idx]
                        .fields_by_name
                        .contains_key(key.as_str())
                })
                .collect::<Vec<_>>();

            if !filtered.is_empty() {
                candidates = filtered;
            }
        }

        if candidates.is_empty() {
            return Err(SolveError::NoMatch {
                missing_required: Vec::new(),
                missing_required_detailed: Vec::new(),
                unknown_fields,
            });
        }

        let viable = candidates
            .iter()
            .copied()
            .filter(|idx| {
                self.resolutions[*idx]
                    .required_fields
                    .iter()
                    .all(|name| seen_key_set.contains(*name))
            })
            .collect::<Vec<_>>();

        match viable.len() {
            0 => {
                let (best_idx, missing_required) = candidates
                    .iter()
                    .copied()
                    .map(|idx| {
                        let missing = self.resolutions[idx]
                            .required_fields
                            .iter()
                            .filter(|name| !seen_key_set.contains(*name))
                            .copied()
                            .collect::<Vec<_>>();
                        (idx, missing)
                    })
                    .min_by_key(|(_, missing)| missing.len())
                    .unwrap_or((candidates[0], Vec::new()));

                let missing_required_detailed = missing_required
                    .iter()
                    .filter_map(|name| self.resolutions[best_idx].fields_by_name.get(name))
                    .map(|route| MissingFieldInfo {
                        name: route.serialized_name,
                        path: route.path_string(),
                        defined_in: route.defined_in.to_owned(),
                    })
                    .collect::<Vec<_>>();

                Err(SolveError::NoMatch {
                    missing_required,
                    missing_required_detailed,
                    unknown_fields,
                })
            }
            1 => {
                let index = viable[0];
                let resolution = self
                    .resolutions
                    .get(index)
                    .expect("internal solver index out of bounds");
                Ok(Resolved { index, resolution })
            }
            _ => {
                let candidates_desc = viable
                    .iter()
                    .map(|idx| self.resolutions[*idx].describe())
                    .collect::<Vec<_>>();

                let disambiguating_fields = find_disambiguating_fields(
                    viable
                        .iter()
                        .map(|idx| &self.resolutions[*idx])
                        .collect::<Vec<_>>(),
                );

                Err(SolveError::Ambiguous {
                    candidates: candidates_desc,
                    disambiguating_fields,
                })
            }
        }
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct VariantSelection {
    enum_name: &'static str,
    variant_name: &'static str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EnumRepr {
    Flattened,
    ExternallyTagged,
    InternallyTagged {
        tag: &'static str,
    },
    AdjacentlyTagged {
        tag: &'static str,
        content: &'static str,
    },
}

fn analyze_struct(
    st: StructType,
    parent_path: Vec<PathSegment>,
) -> Result<Vec<Resolution>, SchemaError> {
    let mut configs = vec![Resolution::new()];
    for field in st.fields {
        configs = analyze_field(field, &parent_path, configs)?;
    }
    Ok(configs)
}

fn analyze_field(
    field: &'static Field,
    parent_path: &[PathSegment],
    configs: Vec<Resolution>,
) -> Result<Vec<Resolution>, SchemaError> {
    if field.is_flattened() {
        return analyze_flattened_field(field, parent_path, configs);
    }

    let mut path = parent_path.to_vec();
    path.push(PathSegment::Field(field.name));

    let route = FieldRoute {
        serialized_name: field.effective_name(),
        path,
        required: !field.has_default() && !is_option_type(field.shape()),
        defined_in: field.shape().type_identifier,
    };

    let mut result = configs;
    for config in &mut result {
        config.add_field(route.clone())?;
    }
    Ok(result)
}

fn analyze_flattened_field(
    field: &'static Field,
    parent_path: &[PathSegment],
    configs: Vec<Resolution>,
) -> Result<Vec<Resolution>, SchemaError> {
    let mut field_path = parent_path.to_vec();
    field_path.push(PathSegment::Field(field.name));

    let original_shape = field.shape();
    let (shape, is_optional_flatten) = match unwrap_option_type(original_shape) {
        Some(inner) => (inner, true),
        None => (original_shape, false),
    };

    match shape.ty {
        Type::User(UserType::Struct(st)) => {
            let mut inner = analyze_struct(st, field_path)?;
            if is_optional_flatten {
                for config in &mut inner {
                    config.mark_all_optional();
                }
            }
            combine_configs(configs, &inner)
        }
        Type::User(UserType::Enum(enum_type)) => analyze_flattened_enum(
            field,
            shape,
            enum_type.variants,
            field_path,
            configs,
            is_optional_flatten,
        ),
        _ => {
            let required = !field.has_default() && !is_option_type(shape) && !is_optional_flatten;
            let route = FieldRoute {
                serialized_name: field.effective_name(),
                path: field_path,
                required,
                defined_in: shape.type_identifier,
            };
            let mut result = configs;
            for config in &mut result {
                config.add_field(route.clone())?;
            }
            Ok(result)
        }
    }
}

fn analyze_flattened_enum(
    field: &'static Field,
    enum_shape: &'static Shape,
    variants: &'static [Variant],
    field_path: Vec<PathSegment>,
    configs: Vec<Resolution>,
    is_optional_flatten: bool,
) -> Result<Vec<Resolution>, SchemaError> {
    let repr = enum_repr_from_shape(enum_shape);
    let enum_name = enum_shape.type_identifier;
    let mut result = Vec::new();

    for base in configs {
        for variant in variants {
            let mut forked = base.clone();
            forked.add_variant_selection(enum_name, variant.name);

            let mut variant_path = field_path.clone();
            variant_path.push(PathSegment::Variant {
                field: field.name,
                variant: variant.name,
            });

            match repr {
                EnumRepr::ExternallyTagged => {
                    let route = FieldRoute {
                        serialized_name: variant.name,
                        path: variant_path,
                        required: !is_optional_flatten,
                        defined_in: enum_shape.type_identifier,
                    };
                    forked.add_field(route)?;
                    result.push(forked);
                }
                EnumRepr::Flattened => {
                    let mut variant_configs = analyze_variant_content(variant, &variant_path)?;
                    if is_optional_flatten {
                        for config in &mut variant_configs {
                            config.mark_all_optional();
                        }
                    }
                    for variant_config in &variant_configs {
                        let mut merged = forked.clone();
                        merged.merge(variant_config)?;
                        result.push(merged);
                    }
                }
                EnumRepr::InternallyTagged { tag } => {
                    let tag_route = FieldRoute {
                        serialized_name: tag,
                        path: variant_path.clone(),
                        required: !is_optional_flatten,
                        defined_in: enum_shape.type_identifier,
                    };
                    forked.add_field(tag_route)?;

                    let mut variant_configs = analyze_variant_content(variant, &variant_path)?;
                    if is_optional_flatten {
                        for config in &mut variant_configs {
                            config.mark_all_optional();
                        }
                    }
                    for variant_config in &variant_configs {
                        let mut merged = forked.clone();
                        merged.merge(variant_config)?;
                        result.push(merged);
                    }
                }
                EnumRepr::AdjacentlyTagged { tag, content } => {
                    let tag_route = FieldRoute {
                        serialized_name: tag,
                        path: variant_path.clone(),
                        required: !is_optional_flatten,
                        defined_in: enum_shape.type_identifier,
                    };
                    forked.add_field(tag_route)?;

                    let content_route = FieldRoute {
                        serialized_name: content,
                        path: variant_path,
                        required: !is_optional_flatten,
                        defined_in: enum_shape.type_identifier,
                    };
                    forked.add_field(content_route)?;
                    result.push(forked);
                }
            }
        }
    }

    Ok(result)
}

fn analyze_variant_content(
    variant: &'static Variant,
    variant_path: &[PathSegment],
) -> Result<Vec<Resolution>, SchemaError> {
    if variant.data.fields.len() == 1 && variant.data.fields[0].name == "0" {
        let inner_shape = variant.data.fields[0].shape();
        if let Type::User(UserType::Struct(st)) = inner_shape.ty {
            let mut inner_path = variant_path.to_vec();
            inner_path.push(PathSegment::Field("0"));
            return analyze_struct(st, inner_path);
        }
    }

    let mut configs = vec![Resolution::new()];
    for field in variant.data.fields {
        configs = analyze_field(field, variant_path, configs)?;
    }
    Ok(configs)
}

fn combine_configs(
    base: Vec<Resolution>,
    extras: &[Resolution],
) -> Result<Vec<Resolution>, SchemaError> {
    let mut result = Vec::new();
    for base_cfg in base {
        for extra_cfg in extras {
            let mut merged = base_cfg.clone();
            merged.merge(extra_cfg)?;
            result.push(merged);
        }
    }
    Ok(result)
}

fn find_disambiguating_fields(candidates: Vec<&Resolution>) -> Vec<String> {
    if candidates.len() < 2 {
        return Vec::new();
    }

    let mut all_fields = BTreeSet::new();
    for candidate in &candidates {
        for name in candidate.fields_by_name.keys() {
            all_fields.insert(*name);
        }
    }

    all_fields
        .into_iter()
        .filter(|name| {
            let in_count = candidates
                .iter()
                .filter(|candidate| candidate.fields_by_name.contains_key(name))
                .count();
            in_count > 0 && in_count < candidates.len()
        })
        .map(str::to_owned)
        .collect()
}

fn enum_repr_from_shape(shape: &'static Shape) -> EnumRepr {
    let tag = shape.get_tag_attr();
    let content = shape.get_content_attr();
    let untagged = shape.is_untagged();

    match (tag, content, untagged) {
        (_, _, true) => EnumRepr::Flattened,
        (Some(t), Some(c), false) => EnumRepr::AdjacentlyTagged { tag: t, content: c },
        (Some(t), None, false) => EnumRepr::InternallyTagged { tag: t },
        (None, None, false) => EnumRepr::ExternallyTagged,
        (None, Some(_), false) => EnumRepr::ExternallyTagged,
    }
}

fn is_option_type(shape: &'static Shape) -> bool {
    matches!(shape.def, Def::Option(_))
}

fn unwrap_option_type(shape: &'static Shape) -> Option<&'static Shape> {
    match shape.def {
        Def::Option(opt) => Some(opt.t),
        _ => None,
    }
}

fn path_to_string(path: &[PathSegment]) -> String {
    let mut out = String::new();
    for (idx, seg) in path.iter().enumerate() {
        if idx > 0 {
            out.push('.');
        }
        match seg {
            PathSegment::Field(name) => out.push_str(name),
            PathSegment::Variant { field, variant } => {
                out.push_str(field);
                out.push_str("::");
                out.push_str(variant);
            }
        }
    }
    out
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

use super::{VDef, VShapeHandle, VShapeStore};
use std::collections::BTreeSet;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProofFieldDef {
    pub offset: usize,
    pub shape_handle: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProofShapeKind {
    Scalar,
    Struct(Vec<ProofFieldDef>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProofShapeDef {
    pub size: usize,
    pub kind: ProofShapeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ProofShapeStore {
    pub shapes: Vec<ProofShapeDef>,
}

impl ProofShapeStore {
    pub fn from_vshape_store(store: &VShapeStore) -> Self {
        let mut shapes = Vec::with_capacity(store.shape_count as usize);
        for idx in 0..store.shape_count as usize {
            let shape = &store.shapes[idx];
            let kind = match shape.def {
                VDef::Scalar => ProofShapeKind::Scalar,
                VDef::Pointer(_) => ProofShapeKind::Scalar,
                VDef::Option(_) => ProofShapeKind::Scalar,
                VDef::List(_) => ProofShapeKind::Scalar,
                VDef::Enum(_) => ProofShapeKind::Scalar,
                VDef::Struct(def) => {
                    let mut fields = Vec::with_capacity(def.field_count as usize);
                    for field_idx in 0..def.field_count as usize {
                        let field = def.fields[field_idx];
                        fields.push(ProofFieldDef {
                            offset: field.offset,
                            shape_handle: field.shape_handle.0 as usize,
                        });
                    }
                    ProofShapeKind::Struct(fields)
                }
            };

            shapes.push(ProofShapeDef {
                size: shape.layout.size(),
                kind,
            });
        }
        Self { shapes }
    }

    pub fn valid_handle(&self, handle: usize) -> bool {
        handle < self.shapes.len()
    }

    pub fn is_well_formed(&self) -> bool {
        self.shapes.iter().all(|shape| match &shape.kind {
            ProofShapeKind::Scalar => true,
            ProofShapeKind::Struct(fields) => fields.iter().all(|field| {
                self.valid_handle(field.shape_handle)
                    && field.offset <= shape.size
                    && self.shapes[field.shape_handle]
                        .size
                        .checked_add(field.offset)
                        .is_some_and(|end| end <= shape.size)
            }),
        })
    }
}

pub fn range_init(init: &BTreeSet<usize>, base: usize, len: usize) -> bool {
    base.checked_add(len)
        .is_some_and(|end| (base..end).all(|x| init.contains(&x)))
}

pub fn all_init_recursive(
    store: &ProofShapeStore,
    handle: usize,
    base: usize,
    init: &BTreeSet<usize>,
    fuel: usize,
) -> bool {
    if fuel == 0 || !store.valid_handle(handle) {
        return false;
    }

    let shape = &store.shapes[handle];
    if !range_init(init, base, shape.size) {
        return false;
    }

    match &shape.kind {
        ProofShapeKind::Scalar => true,
        ProofShapeKind::Struct(fields) => fields.iter().all(|field| {
            base.checked_add(field.offset).is_some_and(|field_base| {
                all_init_recursive(store, field.shape_handle, field_base, init, fuel - 1)
            })
        }),
    }
}

pub fn struct_child_all_init(
    store: &ProofShapeStore,
    handle: usize,
    base: usize,
    init: &BTreeSet<usize>,
    fuel: usize,
    target_idx: usize,
) -> bool {
    if fuel == 0 || !all_init_recursive(store, handle, base, init, fuel) {
        return false;
    }

    let Some(shape) = store.shapes.get(handle) else {
        return false;
    };

    let ProofShapeKind::Struct(fields) = &shape.kind else {
        return false;
    };

    let Some(field) = fields.get(target_idx) else {
        return false;
    };

    base.checked_add(field.offset).is_some_and(|field_base| {
        all_init_recursive(store, field.shape_handle, field_base, init, fuel - 1)
    })
}

fn full_init_set(base: usize, len: usize) -> BTreeSet<usize> {
    base.checked_add(len)
        .map_or_else(BTreeSet::new, |end| (base..end).collect())
}

#[test]
fn bridge_store_from_real_types_is_well_formed() {
    let mut store = VShapeStore::new();
    let u32_h = store.add(super::VShapeDef::scalar(std::alloc::Layout::new::<u32>()));
    let inner = store.add(super::VShapeDef::struct_with_fields(
        &store,
        &[(0, u32_h), (4, u32_h)],
    ));
    let outer = store.add(super::VShapeDef::struct_with_fields(
        &store,
        &[(0, u32_h), (4, inner)],
    ));

    let bridged = ProofShapeStore::from_vshape_store(&store);
    assert!(bridged.is_well_formed());
    assert!(bridged.valid_handle(outer.0 as usize));
    assert_eq!(bridged.shapes.len(), 3);
}

#[test]
fn bridge_recursive_all_init_matches_nested_real_shape() {
    let mut store = VShapeStore::new();
    let u32_h = store.add(super::VShapeDef::scalar(std::alloc::Layout::new::<u32>()));
    let inner = store.add(super::VShapeDef::struct_with_fields(
        &store,
        &[(0, u32_h), (4, u32_h)],
    ));
    let outer = store.add(super::VShapeDef::struct_with_fields(
        &store,
        &[(0, u32_h), (4, inner)],
    ));

    let bridged = ProofShapeStore::from_vshape_store(&store);
    let outer_size = store.get_def(outer).layout.size();
    let mut init = full_init_set(0, outer_size);
    assert!(all_init_recursive(&bridged, outer.0 as usize, 0, &init, 4));

    init.remove(&5);
    assert!(!all_init_recursive(&bridged, outer.0 as usize, 0, &init, 4));
}

#[test]
fn bridge_struct_child_property_holds_for_real_nested_shape() {
    let mut store = VShapeStore::new();
    let u32_h = store.add(super::VShapeDef::scalar(std::alloc::Layout::new::<u32>()));
    let inner = store.add(super::VShapeDef::struct_with_fields(
        &store,
        &[(0, u32_h), (4, u32_h)],
    ));
    let outer = store.add(super::VShapeDef::struct_with_fields(
        &store,
        &[(0, u32_h), (4, inner)],
    ));

    let bridged = ProofShapeStore::from_vshape_store(&store);
    let outer_size = store.get_def(outer).layout.size();
    let init = full_init_set(0, outer_size);

    assert!(struct_child_all_init(
        &bridged,
        outer.0 as usize,
        0,
        &init,
        4,
        1
    ));
}

#[test]
fn bridge_invalid_child_index_is_rejected() {
    let mut store = VShapeStore::new();
    let u32_h = store.add(super::VShapeDef::scalar(std::alloc::Layout::new::<u32>()));
    let outer = store.add(super::VShapeDef::struct_with_fields(&store, &[(0, u32_h)]));

    let bridged = ProofShapeStore::from_vshape_store(&store);
    let outer_size = store.get_def(outer).layout.size();
    let init = full_init_set(0, outer_size);

    assert!(!struct_child_all_init(
        &bridged,
        outer.0 as usize,
        0,
        &init,
        3,
        9
    ));
}

#[test]
fn bridge_invalid_handle_is_rejected() {
    let mut store = VShapeStore::new();
    let u32_h = store.add(super::VShapeDef::scalar(std::alloc::Layout::new::<u32>()));
    store.add(super::VShapeDef::struct_with_fields(&store, &[(0, u32_h)]));

    let bridged = ProofShapeStore::from_vshape_store(&store);
    let init = full_init_set(0, 8);
    assert!(!all_init_recursive(&bridged, 99, 0, &init, 3));
}

#[test]
fn bridge_struct_child_requires_positive_fuel() {
    let mut store = VShapeStore::new();
    let u32_h = store.add(super::VShapeDef::scalar(std::alloc::Layout::new::<u32>()));
    let outer = store.add(super::VShapeDef::struct_with_fields(&store, &[(0, u32_h)]));

    let bridged = ProofShapeStore::from_vshape_store(&store);
    let outer_size = store.get_def(outer).layout.size();
    let init = full_init_set(0, outer_size);
    assert!(!struct_child_all_init(
        &bridged,
        outer.0 as usize,
        0,
        &init,
        0,
        0
    ));
}

#[test]
fn bridge_handle_conversion_matches_real_handles() {
    let mut store = VShapeStore::new();
    let h0 = store.add(super::VShapeDef::scalar(std::alloc::Layout::new::<u8>()));
    let h1 = store.add(super::VShapeDef::scalar(std::alloc::Layout::new::<u16>()));
    let h2 = store.add(super::VShapeDef::struct_with_fields(
        &store,
        &[(0, h0), (2, h1)],
    ));

    let bridged = ProofShapeStore::from_vshape_store(&store);
    let shape = &bridged.shapes[h2.0 as usize];
    let ProofShapeKind::Struct(fields) = &shape.kind else {
        panic!("expected struct");
    };
    assert_eq!(fields[0].shape_handle, h0.0 as usize);
    assert_eq!(fields[1].shape_handle, h1.0 as usize);
}

#[test]
fn bridge_round_trip_shape_count_matches_real_store() {
    let mut store = VShapeStore::new();
    for _ in 0..4 {
        store.add(super::VShapeDef::scalar(std::alloc::Layout::new::<u32>()));
    }
    let bridged = ProofShapeStore::from_vshape_store(&store);
    assert_eq!(bridged.shapes.len(), store.shape_count as usize);
}

#[test]
fn bridge_scalar_shape_needs_full_range_init() {
    let mut store = VShapeStore::new();
    let h = store.add(super::VShapeDef::scalar(std::alloc::Layout::new::<u32>()));
    let bridged = ProofShapeStore::from_vshape_store(&store);

    let mut init = full_init_set(0, 4);
    assert!(all_init_recursive(&bridged, h.0 as usize, 0, &init, 1));
    init.remove(&3);
    assert!(!all_init_recursive(&bridged, h.0 as usize, 0, &init, 1));
}

#[test]
fn bridge_field_offset_contributes_to_child_base() {
    let mut store = VShapeStore::new();
    let leaf = store.add(super::VShapeDef::scalar(std::alloc::Layout::new::<u16>()));
    let root = store.add(super::VShapeDef::struct_with_fields(&store, &[(4, leaf)]));

    let bridged = ProofShapeStore::from_vshape_store(&store);
    let mut init = BTreeSet::new();
    init.insert(4);
    init.insert(5);
    init.insert(0);
    init.insert(1);
    init.insert(2);
    init.insert(3);

    assert!(all_init_recursive(&bridged, root.0 as usize, 0, &init, 3));
    init.remove(&4);
    assert!(!all_init_recursive(&bridged, root.0 as usize, 0, &init, 3));
}

#[test]
fn bridge_accepts_handle_zero_like_real_store() {
    let mut store = VShapeStore::new();
    let h0 = store.add(super::VShapeDef::scalar(std::alloc::Layout::new::<u8>()));
    let bridged = ProofShapeStore::from_vshape_store(&store);
    let init = full_init_set(0, 1);

    assert_eq!(h0, VShapeHandle(0));
    assert!(all_init_recursive(&bridged, 0, 0, &init, 1));
}

use creusot_std::prelude::*;

use crate::IHeap;

use super::{
    CHeap, CPtr, CShapeHandle, CShapeStore, init_fact, shape_is_pointer, shape_is_scalar,
    shape_size,
};

/// Sanity check: `can_drop` matches init membership.
#[check(ghost)]
pub fn can_drop_matches_init(store: &CShapeStore, handle: CShapeHandle, heap: CHeap, ptr: CPtr) {
    let shape = super::CShapeView { store, handle };
    proof_assert!(
        if shape_is_scalar(shape) {
            heap.can_drop(ptr, shape) == heap.range_init(ptr, shape_size(shape))
        } else if shape_is_pointer(shape) {
            heap.can_drop(ptr, shape)
                ==> heap@.init.contains(init_fact(
                        ptr.alloc_id,
                        ptr.offset as usize,
                        shape.handle
                    ))
        } else {
            heap.can_drop(ptr, shape)
                == heap@.init.contains(init_fact(
                    ptr.alloc_id,
                    ptr.offset as usize,
                    shape.handle
                ))
        }
    );
}

#![allow(unused)]
use creusot_std::prelude::*;

use crate::IHeap;

use super::{
    CHeap, CPtr, CShapeHandle, CShapeStore, init_fact, shape_is_pointer, shape_is_scalar,
    shape_size,
};

#[cfg(not(creusot))]
use crate::runtime::IShape;
#[cfg(not(creusot))]
use crate::runtime::LiveRuntime;
use crate::runtime::{IHeap, IRuntime};
use core::marker::PhantomData;
#[cfg(creusot)]
use creusot_std::macros::ensures;
#[cfg(creusot)]
use creusot_std::prelude::trusted;

type Heap<R> = <R as IRuntime>::Heap;
type Shape<R> = <R as IRuntime>::Shape;
type Ptr<R> = <Heap<R> as IHeap<Shape<R>>>::Ptr;

#[cfg(creusot)]
#[trusted]
#[ensures(result == heap.can_drop(ptr, shape))]
fn heap_can_drop<H, S>(heap: &H, ptr: <H as IHeap<S>>::Ptr, shape: S) -> bool
where
    H: IHeap<S>,
    S: crate::runtime::IShape,
{
    false
}

/// Owned heap value produced by Trame::build.
pub struct HeapValue<'facet, R>
where
    R: IRuntime,
{
    heap: Heap<R>,
    ptr: Ptr<R>,
    shape: Shape<R>,
    _marker: PhantomData<&'facet ()>,
}

impl<'facet, R> HeapValue<'facet, R>
where
    R: IRuntime,
{
    pub(crate) fn new(heap: Heap<R>, ptr: Ptr<R>, shape: Shape<R>) -> Self {
        Self {
            heap,
            ptr,
            shape,
            _marker: PhantomData,
        }
    }

    pub fn data_ptr(&self) -> Ptr<R> {
        self.ptr
    }

    pub fn shape(&self) -> Shape<R> {
        self.shape
    }
}

#[cfg(not(creusot))]
impl<'facet, R> HeapValue<'facet, R>
where
    R: LiveRuntime,
    Shape<R>: IShape + PartialEq,
{
    /// Materialize a concrete value from this heap allocation.
    pub fn materialize<T: facet_core::Facet<'facet>>(self) -> Result<T, crate::trame::TrameError> {
        if self.shape != T::SHAPE {
            return Err(crate::trame::TrameError::ShapeMismatch);
        }
        let mut this = core::mem::ManuallyDrop::new(self);
        let mut heap = core::mem::replace(&mut this.heap, R::heap());
        let value = unsafe { core::ptr::read(this.ptr.cast::<T>()) };
        unsafe { heap.dealloc(this.ptr, this.shape) };
        Ok(value)
    }
}

impl<'facet, R> Drop for HeapValue<'facet, R>
where
    R: IRuntime,
{
    fn drop(&mut self) {
        unsafe {
            #[cfg(creusot)]
            if heap_can_drop::<Heap<R>, Shape<R>>(&self.heap, self.ptr, self.shape) {
                self.heap.drop_in_place(self.ptr, self.shape);
            }
            #[cfg(not(creusot))]
            self.heap.drop_in_place(self.ptr, self.shape);
            self.heap.dealloc(self.ptr, self.shape);
        }
    }
}

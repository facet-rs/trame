use crate::runtime::{IHeap, IRuntime, IShape, LiveRuntime};
use core::marker::PhantomData;

type Heap<R> = <R as IRuntime>::Heap;
type Shape<R> = <R as IRuntime>::Shape;
type Ptr<R> = <Heap<R> as IHeap<Shape<R>>>::Ptr;

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

impl<'facet, R> HeapValue<'facet, R>
where
    R: LiveRuntime,
{
    /// Materialize a concrete value from this heap allocation.
    pub fn materialize<T: facet_core::Facet<'facet>>(self) -> Result<T, crate::trame::TrameError> {
        if !core::ptr::eq(self.shape, T::SHAPE) {
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
            self.heap.drop_in_place(self.ptr, self.shape);
            self.heap.dealloc(self.ptr, self.shape);
        }
    }
}

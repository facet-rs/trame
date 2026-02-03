use crate::runtime::IPtr;

/// Provide access to a raw pointer when available.
pub trait PtrAsMut: Copy {
    /// Returns a raw pointer if this pointer represents real memory.
    fn as_mut_ptr(self) -> Option<*mut u8>;
}

impl IPtr for *mut u8 {
    #[inline]
    fn byte_add(self, n: usize) -> Self {
        // SAFETY: caller ensures the resulting pointer is in-bounds.
        unsafe { self.add(n) }
    }
}

impl PtrAsMut for *mut u8 {
    #[inline]
    fn as_mut_ptr(self) -> Option<*mut u8> {
        Some(self)
    }
}

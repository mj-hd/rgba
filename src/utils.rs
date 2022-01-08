use std::cell::UnsafeCell;
use std::ops::Deref;
use std::rc::Rc;

// from: https://github.com/michelhe/rustboyadvance-ng/blob/ab5521e522e489cfb93cb52f1974c48b30b77ffe/core/src/util.rs#L205
#[repr(transparent)]
#[derive(Debug)]
pub struct Shared<T>(Rc<UnsafeCell<T>>);

impl<T> std::ops::Deref for Shared<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { &(*self.0.get()) }
    }
}

impl<T> std::ops::DerefMut for Shared<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut (*self.0.get()) }
    }
}

impl<T> Clone for Shared<T> {
    #[inline]
    fn clone(&self) -> Shared<T> {
        Shared(self.0.clone())
    }
}

impl<T> Shared<T> {
    pub fn new(t: T) -> Shared<T> {
        Shared(Rc::new(UnsafeCell::new(t)))
    }

    pub unsafe fn inner_unsafe(&self) -> &mut T {
        &mut (*self.0.get())
    }
}

impl<T> Shared<T>
where
    T: Clone,
{
    pub fn clone_inner(&self) -> T {
        self.deref().clone()
    }
}

impl<T> Default for Shared<T>
where
    T: Default,
{
    fn default() -> Shared<T> {
        Shared::new(Default::default())
    }
}

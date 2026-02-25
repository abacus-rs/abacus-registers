#![no_std]
use core::cell::Cell;
use core::convert::TryFrom;

pub use code_gen::process_register_block;
pub use tock_registers;

/// Trait denoting a register block and specifies
/// the StateEnum associated type (wrapper enum for all
/// states the peripheral can be in).
pub trait Reg
where
    Self: TryFrom<Self::StateEnum, Error = <Self as Reg>::StateEnum>,
{
    type StateEnum: StateEnum;
}

pub trait StableStateReg {}
pub trait TransientStateReg {}

/// User implemented trait that reconciles the state
/// from a transient state.
pub trait SyncState {
    type SyncStateEnum: StateEnum;
    fn sync_state(self) -> Self::SyncStateEnum;
}

/// Marker trait for all state enums.
pub trait StateEnum {
    fn sync_state(self) -> Self;
}

/// Marker trait for a state type. Specifies the
/// associated register block type and state enum type.
trait TransientState: SyncState {
    //    type Reg: Reg<StateEnum = Self::StateEnum>;
    //    type StateEnum: StateEnum;
}

// (TODO) This is based on Tock's cell types, but soundness
// needs to be thought more about (e.g. around init/uninit)
/// Custom Cell type for Abacus to all a driver to store
/// the current state and access the state using interior mutability.
pub struct AbacusCell<T: StateEnum> {
    val: Cell<Option<T>>,
}

impl<T: StateEnum> AbacusCell<T> {
    pub fn new(val: T) -> Self {
        AbacusCell {
            val: Cell::new(Some(val)),
        }
    }

    /// Accessor to obtain register struct and perform
    /// driver mmio operations.
    #[inline(always)]
    pub fn map<F, R>(&self, closure: F) -> Option<R>
    where
        F: FnOnce(T) -> (T, R),
    {
        if let Some(current) = self.val.take() {
            let (new_val, pass_thru) = closure(current);
            self.val.set(Some(new_val));
            Some(pass_thru)
        } else {
            None
        }
    }
}

use core::clone::Clone;
use core::fmt::Debug;
use core::marker::Copy;
use core::ops::Deref;
use core::ops::FnOnce;
use core::option::Option;
use core::option::Option::None;
use core::option::Option::Some;
use core::prelude::rust_2024::derive;
use core::ptr::NonNull;

/// A pointer to statically allocated mutable data such as memory mapped I/O
/// registers.
///
/// Note, this copies Tock's StaticRef but renames it to AbacusStaticRef to avoid
/// potential conflicts with Tock crates.
///
/// This is a simple wrapper around a raw pointer that encapsulates an unsafe
/// dereference in a safe manner. It serve the role of creating a `&'static T`
/// given a raw address and acts similarly to `extern` definitions, except
/// [`StaticRef`] is subject to module and crate boundaries, while `extern`
/// definitions can be imported anywhere.
///
/// Because this defers the actual dereference, this can be put in a `const`,
/// whereas `const I32_REF: &'static i32 = unsafe { &*(0x1000 as *const i32) };`
/// will always fail to compile since `0x1000` doesn't have an allocation at
/// compile time, even if it's known to be a valid MMIO address.
#[derive(Debug)]
pub struct AbacusStaticRef<T> {
    ptr: NonNull<T>,
}

impl<T> AbacusStaticRef<T> {
    /// Create a new [`AbacusStaticRef`] from a raw pointer
    ///
    /// ## Safety
    ///
    /// - `ptr` must be aligned, non-null, and dereferencable as `T`.
    /// - `*ptr` must be valid for the program duration.
    pub const unsafe fn new(ptr: *const T) -> AbacusStaticRef<T> {
        // SAFETY: `ptr` is non-null as promised by the caller.
        AbacusStaticRef {
            ptr: NonNull::new_unchecked(ptr.cast_mut()),
        }
    }
}

impl<T> Clone for AbacusStaticRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for AbacusStaticRef<T> {}

impl<T> Deref for AbacusStaticRef<T> {
    type Target = T;
    fn deref(&self) -> &T {
        // SAFETY: `ptr` is aligned and dereferencable for the program duration
        // as promised by the caller of `StaticRef::new`.
        unsafe { self.ptr.as_ref() }
    }
}

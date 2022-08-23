//! Contains basic wrappers for the Erlang NIF API. Should not be used directly.
//!
//! This module should perform validation and make them (reasonably) safe and easy to
//! use from Rust. This module should try to be as non-opinionated as possible, and
//! should try to stick as close as possible to the original C API.
//!
//! Making the APIs nice to use from Rust should be done in the root `rustler` crate.
#![allow(clippy::upper_case_acronyms)]

pub mod atom;
pub mod binary;
pub mod check;
pub mod env;
pub mod exception;
pub mod list;
pub mod map;
pub mod monitor;
pub mod pid;
pub mod resource;
pub mod term;
pub mod tuple;

pub use crate::sys::{
    enif_clear_env, enif_free_env, enif_get_local_pid, enif_make_pid, enif_map_iterator_create,
    enif_map_iterator_destroy, enif_map_iterator_get_pair, enif_map_iterator_next, enif_self,
    ErlNifMapIterator, ErlNifMapIteratorEntry, ErlNifMonitor, ErlNifPid, ErlNifResourceDown,
    ErlNifResourceDtor, ErlNifResourceDynCall, ErlNifResourceStop, ERL_NIF_THR_DIRTY_CPU_SCHEDULER,
    ERL_NIF_THR_DIRTY_IO_SCHEDULER, ERL_NIF_THR_NORMAL_SCHEDULER, ERL_NIF_THR_UNDEFINED,
};

pub use std::os::raw::{c_double, c_int, c_uchar, c_uint, c_void};
pub type size_t = usize;

pub type NIF_ENV = *mut crate::sys::ErlNifEnv;
pub type NIF_TERM = size_t;
pub type NIF_RESOURCE_TYPE = *const crate::sys::ErlNifResourceType;

pub fn get_nif_resource_type_init_size() -> usize {
    std::mem::size_of::<crate::sys::ErlNifResourceTypeInit>()
}

pub type NIF_RESOURCE_HANDLE = *const c_void;
pub type MUTABLE_NIF_RESOURCE_HANDLE = *mut c_void;

pub type NifResourceDtor =
    unsafe extern "C" fn(r_env: NIF_ENV, obj: MUTABLE_NIF_RESOURCE_HANDLE) -> ();
pub type NifResourceStop = ErlNifResourceStop;
pub type NifResourceDown = ErlNifResourceDown;
pub type NifResourceDynCall = ErlNifResourceDynCall;
pub type NifResourceFlags = crate::sys::ErlNifResourceFlags;

pub enum NIF_ERROR {
    BAD_ARG,
}

pub type DEF_NIF_FUNC = crate::sys::ErlNifFunc;
pub type DEF_NIF_ENTRY = crate::sys::ErlNifEntry;
pub use crate::sys::ErlNifResourceFlags as NIF_RESOURCE_FLAGS;
pub use crate::sys::NIF_MAJOR_VERSION;
pub use crate::sys::NIF_MINOR_VERSION;

pub use crate::sys::ErlNifBinaryToTerm as NIF_BINARY_TO_TERM_OPTS;
pub use crate::sys::ERL_NIF_BIN2TERM_SAFE;

#[repr(C)]
pub enum ErlNifTaskFlags {
    ERL_NIF_NORMAL_JOB = 0,
    ERL_NIF_DIRTY_JOB_CPU_BOUND = 1,
    ERL_NIF_DIRTY_JOB_IO_BOUND = 2,
}

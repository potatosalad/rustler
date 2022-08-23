use crate::wrapper::{
    NifResourceDtor, NifResourceStop, NifResourceDown, NifResourceDynCall, NifResourceFlags, NIF_ENV, NIF_RESOURCE_HANDLE, NIF_RESOURCE_TYPE, NIF_TERM,
};

pub use crate::sys::{
    enif_alloc_resource as alloc_resource, enif_keep_resource as keep_resource,
    enif_make_resource as make_resource,
    ErlNifResourceTypeInit,
    ErlNifResourceDtor,
    ErlNifResourceStop,
    ErlNifResourceDown,
    ErlNifResourceDynCall,
};

pub use crate::sys::enif_release_resource as release_resource;

use std::mem::MaybeUninit;
use std::ptr;

pub unsafe fn init_resource_type(
    env: NIF_ENV,
    name: &[u8],
    dtor: Option<NifResourceDtor>,
    stop: Option<NifResourceStop>,
    down: Option<NifResourceDown>,
    dyncall: Option<NifResourceDynCall>,
    flags: NifResourceFlags,
) -> Option<NIF_RESOURCE_TYPE> {
    // Panic if name is not null-terminated.
    assert_eq!(name.last().cloned(), Some(0u8));

    let name_p = name.as_ptr();
    let dtor: *const ErlNifResourceDtor = match dtor {
        Some(cb) => cb as *const _,
        None => ptr::null(),
    };
    let stop: *const ErlNifResourceStop = match stop {
        Some(cb) => cb as *const _,
        None => ptr::null(),
    };
    let down: *const ErlNifResourceDown = match down {
        Some(cb) => cb as *const _,
        None => ptr::null(),
    };
    let dyncall: *const ErlNifResourceDynCall = match dyncall {
        Some(cb) => cb as *const _,
        None => ptr::null(),
    };
    let init = ErlNifResourceTypeInit {
        dtor,
        stop,
        down,
        members: 4,
        dyncall,
    };
    let res = {
        let mut tried = MaybeUninit::uninit();
        crate::sys::enif_init_resource_type(env, name_p, &init, flags, tried.as_mut_ptr())
    };

    if res.is_null() {
        None
    } else {
        Some(res)
    }
}

// Functionally incomplete
pub unsafe fn get_resource(
    env: NIF_ENV,
    term: NIF_TERM,
    typ: NIF_RESOURCE_TYPE,
) -> Option<NIF_RESOURCE_HANDLE> {
    let mut ret_obj = MaybeUninit::uninit();
    let res = crate::sys::enif_get_resource(env, term, typ, ret_obj.as_mut_ptr());

    if res == 0 {
        None
    } else {
        Some(ret_obj.assume_init())
    }
}

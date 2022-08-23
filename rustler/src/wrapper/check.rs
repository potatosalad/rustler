use crate::wrapper::{NIF_ENV, NIF_TERM};

macro_rules! impl_check_fun {
    ($name:ident, $inner:path) => {
        pub unsafe fn $name(env: NIF_ENV, term: NIF_TERM) -> bool {
            $inner(env, term) == 1
        }
    };
}

impl_check_fun!(is_atom, crate::sys::enif_is_atom);
impl_check_fun!(is_binary, crate::sys::enif_is_binary);
impl_check_fun!(is_empty_list, crate::sys::enif_is_empty_list);
impl_check_fun!(is_exception, crate::sys::enif_is_exception);
impl_check_fun!(is_fun, crate::sys::enif_is_fun);
impl_check_fun!(is_list, crate::sys::enif_is_list);
impl_check_fun!(is_map, crate::sys::enif_is_map);
impl_check_fun!(is_number, crate::sys::enif_is_number);
impl_check_fun!(is_pid, crate::sys::enif_is_pid);
impl_check_fun!(is_port, crate::sys::enif_is_port);
impl_check_fun!(is_ref, crate::sys::enif_is_ref);
impl_check_fun!(is_tuple, crate::sys::enif_is_tuple);

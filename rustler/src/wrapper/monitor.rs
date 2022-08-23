use crate::wrapper::{ErlNifMonitor, NIF_ENV, NIF_TERM};

pub unsafe fn make_monitor(env: NIF_ENV, monitor: &ErlNifMonitor) -> NIF_TERM {
    crate::sys::enif_make_monitor_term(env, monitor)
}

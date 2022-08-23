use crate::wrapper::{monitor, ErlNifMonitor};
use crate::{Encoder, Env, Term};

#[derive(Copy, Clone)]
pub struct Monitor {
    c: ErlNifMonitor,
}

impl Monitor {
    pub fn as_c_arg(&self) -> &ErlNifMonitor {
        &self.c
    }

    pub fn from_c_arg(erl_nif_monitor: ErlNifMonitor) -> Self {
        Monitor { c: erl_nif_monitor }
    }
}

impl PartialEq for Monitor {
    fn eq(&self, other: &Self) -> bool {
        unsafe { crate::sys::enif_compare_monitors(&self.c, &other.c) == 0 }
    }
}

impl PartialOrd for Monitor {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering;
        match unsafe { crate::sys::enif_compare_monitors(&self.c, &other.c) } {
            x if x < 0 => Some(Ordering::Less),
            x if x == 0 => Some(Ordering::Equal),
            _ => Some(Ordering::Greater),
        }
    }
}

impl Encoder for Monitor {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        unsafe { Term::new(env, monitor::make_monitor(env.as_c_arg(), &self.c)) }
    }
}

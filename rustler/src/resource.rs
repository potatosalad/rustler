//! Support for storing Rust data in Erlang terms.
//!
//! A NIF resource allows you to safely store Rust structs in a term, and therefore keep it across
//! NIF calls. The struct will be automatically dropped when the BEAM GC decides that there are no
//! more references to the resource.

use std::{marker::PhantomData, mem::MaybeUninit};
use std::mem;
use std::ops::Deref;
use std::ptr;

use crate::codegen_runtime::NIF_TERM;
use crate::sys::{ErlNifEvent, ErlNifMonitor, ErlNifPid, ErlNifSelectFlags, ErlNifSelectReturnFlags, ERL_NIF_SELECT_READ, ERL_NIF_SELECT_WRITE, ERL_NIF_SELECT_ERROR, ERL_NIF_SELECT_CUSTOM_MSG};

use super::{Decoder, Encoder, Env, Error, LocalPid, Monitor, NifResult, Term};
use crate::wrapper::{
    c_int, c_void, NifResourceFlags, NifResourceStop, NifResourceDown, NifResourceDynCall, MUTABLE_NIF_RESOURCE_HANDLE, NIF_ENV, NIF_RESOURCE_TYPE,
};

/// Re-export a type used by the `resource!` macro.
#[doc(hidden)]
pub use crate::wrapper::NIF_RESOURCE_FLAGS;

/// The ResourceType struct contains a  NIF_RESOURCE_TYPE and a phantom reference to the type it
/// is for. It serves as a holder for the information needed to interact with the Erlang VM about
/// the resource type.
///
/// This is usually stored in an implementation of ResourceTypeProvider.
#[doc(hidden)]
pub struct ResourceType<T> {
    pub res: NIF_RESOURCE_TYPE,
    pub struct_type: PhantomData<T>,
}

/// This trait gets implemented for the type we want to put into a resource when
/// resource! is called on it. It provides the ResourceType.
///
/// In most cases the user should not have to worry about this.
#[doc(hidden)]
pub trait ResourceTypeProvider: Sized + Send + Sync + 'static {
    fn get_type() -> &'static ResourceType<Self>;
}

impl<T> Encoder for ResourceArc<T>
where
    T: ResourceTypeProvider,
{
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        self.as_term(env)
    }
}
impl<'a, T> Decoder<'a> for ResourceArc<T>
where
    T: ResourceTypeProvider + 'a,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        ResourceArc::from_term(term)
    }
}

/// Drop a T that lives in an Erlang resource. (erlang_nif-sys requires us to declare this
/// function safe, but it is of course thoroughly unsafe!)
extern "C" fn resource_destructor<T>(_env: NIF_ENV, handle: MUTABLE_NIF_RESOURCE_HANDLE) {
    unsafe {
        let aligned = align_alloced_mem_for_struct::<T>(handle);
        let res = aligned as *mut T;
        ptr::read(res);
    }
}

/// Notity that resource that a select stop event has occured.
extern "C" fn resource_select_stop<T: SelectStopResource>(
    nif_env: NIF_ENV,
    handle: MUTABLE_NIF_RESOURCE_HANDLE,
    event: ErlNifEvent,
    is_direct_call: c_int,
) {
    let lifetime = ();
    let env = unsafe { crate::Env::new(&lifetime, nif_env) };
    let resource = ResourceArc::<T>::from_c_arg(handle);
    let is_direct_call = if is_direct_call == 0 { false } else { true };
    T::resource_select_stop(env, resource, event, is_direct_call);
}

/// Notity that resource that a monitored object is down.
extern "C" fn resource_monitor_down<T: MonitorDownResource>(
    nif_env: NIF_ENV,
    handle: MUTABLE_NIF_RESOURCE_HANDLE,
    pid: *const ErlNifPid,
    mon: *const ErlNifMonitor,
) {
    let lifetime = ();
    let env = unsafe { crate::Env::new(&lifetime, nif_env) };
    let resource = ResourceArc::<T>::from_c_arg(handle);
    let pid = LocalPid::from_c_arg(unsafe { *pid });
    let mon = Monitor::from_c_arg(unsafe { *mon });
    T::resource_monitor_down(env, resource, pid, mon);
}

/// Perform a dynamic call between NIF resources.
extern "C" fn resource_dynamic_call<T: DynamicCallResource>(
    nif_env: NIF_ENV,
    handle: MUTABLE_NIF_RESOURCE_HANDLE,
    call_data: *const c_void,
) {
    let lifetime = ();
    let env = unsafe { crate::Env::new(&lifetime, nif_env) };
    let resource = ResourceArc::<T>::from_c_arg(handle);
    T::resource_dynamic_call(env, resource, call_data);
}

/// This is the function that gets called from resource! in on_load to create a new
/// resource type.
///
/// # Panics
///
/// Panics if `name` isn't null-terminated.
#[doc(hidden)]
pub fn init_struct_resource_type<T: ResourceTypeProvider>(
    env: Env,
    name: &str,
    stop: Option<NifResourceStop>,
    down: Option<NifResourceDown>,
    dyncall: Option<NifResourceDynCall>,
    flags: NifResourceFlags,
) -> Option<ResourceType<T>> {
    let res: Option<NIF_RESOURCE_TYPE> = unsafe {
        crate::wrapper::resource::init_resource_type(
            env.as_c_arg(),
            name.as_bytes(),
            Some(resource_destructor::<T>),
            stop,
            down,
            dyncall,
            flags,
        )
    };

    res.map(|r| ResourceType {
        res: r,
        struct_type: PhantomData,
    })
}

fn get_alloc_size_struct<T>() -> usize {
    mem::size_of::<T>() + mem::align_of::<T>()
}

/// Given a pointer `ptr` to an allocation of `get_alloc_size_struct::<T>()` bytes, return the
/// first aligned pointer within the allocation where a `T` may be stored.
/// Unsafe: `ptr` must point to a large enough allocation and not be null.
unsafe fn align_alloced_mem_for_struct<T>(ptr: *const c_void) -> *const c_void {
    let offset = mem::align_of::<T>() - ((ptr as usize) % mem::align_of::<T>());
    ptr.add(offset)
}

/// A reference to a resource of type `T`.
///
/// This type is like `std::sync::Arc`: it provides thread-safe, reference-counted storage for Rust
/// data that can be shared across threads. Data stored this way is immutable by default. If you
/// need to modify data in a resource, use a `std::sync::Mutex` or `RwLock`.
///
/// Rust code and Erlang code can both have references to the same resource at the same time.  Rust
/// code uses `ResourceArc`; in Erlang, a reference to a resource is a kind of term.  You can
/// convert back and forth between the two using `Encoder` and `Decoder`.
pub struct ResourceArc<T>
where
    T: ResourceTypeProvider,
{
    raw: *const c_void,
    inner: *mut T,
}

// Safe because T is `Sync` and `Send`.
unsafe impl<T> Send for ResourceArc<T> where T: ResourceTypeProvider {}
unsafe impl<T> Sync for ResourceArc<T> where T: ResourceTypeProvider {}

impl<T> ResourceArc<T>
where
    T: ResourceTypeProvider,
{
    /// Makes a new ResourceArc from the given type. Note that the type must have
    /// ResourceTypeProvider implemented for it. See module documentation for info on this.
    pub fn new(data: T) -> Self {
        let alloc_size = get_alloc_size_struct::<T>();
        let mem_raw =
            unsafe { crate::wrapper::resource::alloc_resource(T::get_type().res, alloc_size) };
        let aligned_mem = unsafe { align_alloced_mem_for_struct::<T>(mem_raw) as *mut T };

        unsafe { ptr::write(aligned_mem, data) };

        ResourceArc {
            raw: mem_raw,
            inner: aligned_mem,
        }
    }

    fn from_term(term: Term) -> Result<Self, Error> {
        let res_resource = match unsafe {
            crate::wrapper::resource::get_resource(
                term.get_env().as_c_arg(),
                term.as_c_arg(),
                T::get_type().res,
            )
        } {
            Some(res) => res,
            None => return Err(Error::BadArg),
        };
        Ok(Self::from_c_arg(res_resource))
    }

    fn as_term<'a>(&self, env: Env<'a>) -> Term<'a> {
        unsafe {
            Term::new(
                env,
                crate::wrapper::resource::make_resource(env.as_c_arg(), self.raw),
            )
        }
    }

    pub fn as_c_arg(&mut self) -> *const c_void {
        self.raw
    }

    pub fn from_c_arg(c_resource: *const c_void) -> Self {
        unsafe {
            crate::wrapper::resource::keep_resource(c_resource);
        }
        let casted_ptr = unsafe { align_alloced_mem_for_struct::<T>(c_resource) as *mut T };
        Self {
            raw: c_resource,
            inner: casted_ptr,
        }
    }

    fn inner(&self) -> &T {
        unsafe { &*self.inner }
    }
}

impl<T> Deref for ResourceArc<T>
where
    T: ResourceTypeProvider,
{
    type Target = T;

    fn deref(&self) -> &T {
        self.inner()
    }
}

impl<T> Clone for ResourceArc<T>
where
    T: ResourceTypeProvider,
{
    /// Cloning a `ResourceArc` simply increments the reference count for the
    /// resource. The `T` value is not cloned.
    fn clone(&self) -> Self {
        unsafe {
            crate::wrapper::resource::keep_resource(self.raw);
        }
        ResourceArc {
            raw: self.raw,
            inner: self.inner,
        }
    }
}

impl<T> Drop for ResourceArc<T>
where
    T: ResourceTypeProvider,
{
    /// When a `ResourceArc` is dropped, the reference count is decremented. If
    /// there are no other references to the resource, the `T` value is dropped.
    ///
    /// However, note that in general, the Rust value in a resource is dropped
    /// at an unpredictable time: whenever the VM decides to do garbage
    /// collection.
    fn drop(&mut self) {
        unsafe { crate::sys::enif_release_resource(self.as_c_arg()) };
    }
}

pub trait ResourceArcSelect {
    fn resource_select(&self, caller_env: Option<&Env>, event: ErlNifEvent, mode: ErlNifSelectFlags, pid: &LocalPid, select_ref: NIF_TERM) -> ErlNifSelectReturnFlags;
    fn resource_select_read(&self, caller_env: Option<&Env>, event: ErlNifEvent, pid: &LocalPid, msg: NIF_TERM, msg_env: Option<&Env>) -> ErlNifSelectReturnFlags;
    fn resource_select_write(&self, caller_env: Option<&Env>, event: ErlNifEvent, pid: &LocalPid, msg: NIF_TERM, msg_env: Option<&Env>) -> ErlNifSelectReturnFlags;
    fn resource_select_error(&self, caller_env: Option<&Env>, event: ErlNifEvent, pid: &LocalPid, msg: NIF_TERM, msg_env: Option<&Env>) -> ErlNifSelectReturnFlags;
}

impl<T: SelectStopResource> ResourceArcSelect for ResourceArc<T> {
    fn resource_select(&self, caller_env: Option<&Env>, event: ErlNifEvent, mode: ErlNifSelectFlags, pid: &LocalPid, select_ref: NIF_TERM) -> ErlNifSelectReturnFlags {
        let env = maybe_nif_env(caller_env);
        let res = unsafe {
            crate::sys::enif_select(env, event, mode, self.raw, pid.as_c_arg(), select_ref)
        };
        res
    }

    fn resource_select_read(&self, caller_env: Option<&Env>, event: ErlNifEvent, pid: &LocalPid, msg: NIF_TERM, msg_env: Option<&Env>) -> ErlNifSelectReturnFlags {
        let env = maybe_nif_env(caller_env);
        let msg_env = maybe_nif_env(msg_env);
        let obj = self.raw as *mut c_void;
        let res = unsafe {
            crate::sys::enif_select_read(env, event, obj, pid.as_c_arg(), msg, msg_env)
        };
        res
    }
    
    fn resource_select_write(&self, caller_env: Option<&Env>, event: ErlNifEvent, pid: &LocalPid, msg: NIF_TERM, msg_env: Option<&Env>) -> ErlNifSelectReturnFlags {
        let env = maybe_nif_env(caller_env);
        let msg_env = maybe_nif_env(msg_env);
        let obj = self.raw as *mut c_void;
        let res = unsafe {
            crate::sys::enif_select_write(env, event, obj, pid.as_c_arg(), msg, msg_env)
        };
        res
    }

    fn resource_select_error(&self, caller_env: Option<&Env>, event: ErlNifEvent, pid: &LocalPid, msg: NIF_TERM, msg_env: Option<&Env>) -> ErlNifSelectReturnFlags {
        let env = maybe_nif_env(caller_env);
        let msg_env = maybe_nif_env(msg_env);
        let obj = self.raw as *mut c_void;
        let res = unsafe {
            crate::sys::enif_select_error(env, event, obj, pid.as_c_arg(), msg, msg_env)
        };
        res
    }
}

pub trait ResourceArcProcessMonitor {
    fn monitor_process(&self, caller_env: Option<&Env>, pid: &LocalPid) -> Option<Monitor>;
    fn demonitor_process(&self, caller_env: Option<&Env>, mon: &Monitor) -> bool;
}

impl<T: MonitorDownResource> ResourceArcProcessMonitor for ResourceArc<T> {
    fn monitor_process(&self, caller_env: Option<&Env>, pid: &LocalPid) -> Option<Monitor> {
        let env = maybe_process_bound_nif_env(caller_env);
        let mut mon = MaybeUninit::uninit();
        let res = unsafe {
            crate::sys::enif_monitor_process(env, self.raw, pid.as_c_arg(), mon.as_mut_ptr()) == 0
        };
        if res {
            Some(Monitor::from_c_arg(unsafe { mon.assume_init() }))
        } else {
            None
        }
    }

    fn demonitor_process(&self, caller_env: Option<&Env>, mon: &Monitor) -> bool {
        let env = maybe_process_bound_nif_env(caller_env);
        unsafe { rustler_sys::enif_demonitor_process(env, self.raw, mon.as_c_arg()) == 0 }
    }
}

/// Is the current thread an Erlang scheduler thread?
fn is_scheduler_thread() -> bool {
    // From `enif_thread_type` docs: A positive value indicates a scheduler
    // thread while a negative value or zero indicates another type of thread.
    unsafe { crate::sys::enif_thread_type() > 0 }
}

fn maybe_nif_env(env: Option<&Env>) -> NIF_ENV {
    match env {
        Some(x) => x.as_c_arg(),
        None => ptr::null_mut(),
    }
}

fn maybe_process_bound_nif_env(env: Option<&Env>) -> NIF_ENV {
    if is_scheduler_thread() {
        let env = env.expect("Env required when calling from a scheduler thread");
        // Panic if `env` is not the environment of the calling process.
        env.pid();
        env.as_c_arg()
    } else {
        assert!(
            env.is_none(),
            "Env provided when not calling from a scheduler thread"
        );
        ptr::null_mut()
    }
}

pub trait SelectStopResource: ResourceTypeProvider {
    fn resource_select_stop(env: Env, resource: ResourceArc<Self>, event: ErlNifEvent, is_direct_call: bool);
}

pub trait MonitorDownResource: ResourceTypeProvider {
    fn resource_monitor_down(env: Env, resource: ResourceArc<Self>, pid: LocalPid, mon: Monitor);
}

pub trait DynamicCallResource: ResourceTypeProvider {
    fn resource_dynamic_call(env: Env, resource: ResourceArc<Self>, call_data: *const c_void);
}

/// Used by the resource! macro to pass the unsafe `resource_select_stop` callback in a
/// safe way (because `resource_select_stop` cannot be accessed outside of this module)
#[doc(hidden)]
pub trait ResourceSelectStopProvider {
    fn resource_select_stop_callback() -> Option<NifResourceStop>;
}

impl ResourceSelectStopProvider for () {
    fn resource_select_stop_callback() -> Option<NifResourceStop> {
        None
    }
}

impl<T: SelectStopResource> ResourceSelectStopProvider for T {
    fn resource_select_stop_callback() -> Option<NifResourceStop> {
        Some(resource_select_stop::<T>)
    }
}

/// Used by the resource! macro to pass the unsafe `resource_monitor_down` callback in a
/// safe way (because `resource_monitor_down` cannot be accessed outside of this module)
#[doc(hidden)]
pub trait ResourceMonitorDownProvider {
    fn resource_monitor_down_callback() -> Option<NifResourceDown>;
}

impl ResourceMonitorDownProvider for () {
    fn resource_monitor_down_callback() -> Option<NifResourceDown> {
        None
    }
}

impl<T: MonitorDownResource> ResourceMonitorDownProvider for T {
    fn resource_monitor_down_callback() -> Option<NifResourceDown> {
        Some(resource_monitor_down::<T>)
    }
}

/// Used by the resource! macro to pass the unsafe `resource_monitor_down` callback in a
/// safe way (because `resource_monitor_down` cannot be accessed outside of this module)
#[doc(hidden)]
pub trait ResourceDynamicCallProvider {
    fn resource_dynamic_call_callback() -> Option<NifResourceDynCall>;
}

impl ResourceDynamicCallProvider for () {
    fn resource_dynamic_call_callback() -> Option<NifResourceDynCall> {
        None
    }
}

impl<T: DynamicCallResource> ResourceDynamicCallProvider for T {
    fn resource_dynamic_call_callback() -> Option<NifResourceDynCall> {
        Some(resource_dynamic_call::<T>)
    }
}

#[macro_export]
#[deprecated(since = "0.22.0", note = "Please use `resource!` instead.")]
macro_rules! resource_struct_init {
    ($struct_name:ty, $env: ident) => {
        $crate::resource!($struct_name, $env)
    };
}

#[macro_export]
macro_rules! resource {
    ($struct_name:ty, $env:ident) => {
        $crate::resource!($struct_name, $env, (), (), ())
    };
    ($struct_name:ty, $env:ident, $stop:ty, $down:ty, $dyncall:ty) => {
        {
            use $crate::resource::{ResourceSelectStopProvider, ResourceMonitorDownProvider, ResourceDynamicCallProvider};
            static mut STRUCT_TYPE: Option<$crate::resource::ResourceType<$struct_name>> = None;

            let temp_struct_type =
                match $crate::resource::init_struct_resource_type::<$struct_name>(
                    $env,
                    concat!(stringify!($struct_name), "\x00"),
                    <$stop>::resource_select_stop_callback(),
                    <$down>::resource_monitor_down_callback(),
                    <$dyncall>::resource_dynamic_call_callback(),
                    $crate::resource::NIF_RESOURCE_FLAGS::ERL_NIF_RT_CREATE
                    ) {
                    Some(inner) => inner,
                    None => {
                        println!("Failure in creating resource type");
                        return false;
                    }
                };
            unsafe { STRUCT_TYPE = Some(temp_struct_type) };

            impl $crate::resource::ResourceTypeProvider for $struct_name {
                fn get_type() -> &'static $crate::resource::ResourceType<Self> {
                    unsafe { &STRUCT_TYPE }.as_ref()
                        .expect("The resource type hasn't been initialized. Did you remember to call the function where you used the `resource!` macro?")
                }
            }
        }
    }
}

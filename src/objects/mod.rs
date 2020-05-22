use zerogc_simple::{Gc, SimpleCollectorContext};
use std::ptr::NonNull;
use std::borrow::Cow;
use crate::{PyResult, PyErr, BuiltinError};
use zerogc::GcAllocContext;

#[macro_export]
macro_rules! builtin_type {
    ($target:ident implements $($protocol:ident),*; name = $name:expr) => {{
        let ObjectProtocol;
        let mut NumberProtocol = std::ptr::null_mut();
        let mut SequenceProtocol = std::ptr::null_mut();
        let mut MappingProtocol = std::ptr::null_mut();
        $($protocol = {
            let ptr = std::ptr::null_mut::<$target>() as *mut dyn $protocol<'static>;
            std::mem::transmute::<*mut dyn $protocol<'static>, std::raw::TraitObject>(ptr).vtable
        };)*
        TypeData {
            name: Cow::Borrowed($name),
            obj_protocol_vtable: NonNull::new(ObjectProtocol).unwrap(),
            number_protocol_vtable: NumberProtocol,
            sequence_protocol_vtable: SequenceProtocol,
            mapping_protocol_vtable: MappingProtocol,
            new: $target::new as fn(_, _) -> _
        }
    }};
}

pub type PyType<'gc> = ObjectRef<'gc, TypeData>;
pub struct TypeData {
    name: Cow<'static, str>,
    /// Pointer to the vtable for `ObjectProtocol` (must never be null)
    obj_protocol_vtable: NonNull<()>,
    /// Pointer to the vtable for `NumberProtocol` (optional)
    number_protocol_vtable: *mut (),
    /// Pointer to the vtable for `SequenceProtocol` (optional)
    sequence_protocol_vtable: *mut (),
    /// Pointer to the vtable for `MappingProtocol` (optional)
    mapping_protocol_vtable: *mut (),
    new: for<'gc> fn(&'gc SimpleCollectorContext, &[ObjectRef<'gc>]) -> PyResult<ObjectRef<'gc>>,
}
impl TypeData {
    pub fn new<'gc>(
        _collect: &'gc SimpleCollectorContext,
        _args: &[ObjectRef<'gc>]
    ) -> PyResult<ObjectRef<'gc>> {
        Err(PyErr::with_msg(
            BuiltinError::Unimplemented,
            "can't call type constructor"
        ))
    }
    #[inline]
    pub fn into_object(self, collect: &SimpleCollectorContext) -> PyType<'_> {
        collect.alloc(self.into())
    }
}
impl<'gc> ObjectProtocol<'gc> for TypeData {
    fn object_type(&self) -> PyType {
        builtin_type!(TypeData implements ObjectProtocol; name = "type")
    }
}

pub struct PyObject<'gc, T: 'gc> {
    object_type: PyType<'gc>,
    value: T
}
impl<'gc, T: ObjectProtocol> From<T> for PyObject<'gc, T> {
    #[inline]
    fn from(value: T) -> Self {
        PyObject { object_type: value.object_type(), value }
    }
}

/// Marker for an object of unknown type
pub struct DynObject {
    _private: ()
}
pub type ObjectRef<'gc, T = DynObject> = Gc<'gc, PyObject<'gc, T>>;

pub trait ObjectProtocol<'gc> {
    fn object_type(&self) -> TypeObject<'gc>;
}
impl<'gc, T> ObjectProtocol<'gc> for ObjectRef<'gc, T> {
    #[inline]
    fn object_type(&self) -> TypeObject<'gc> {
        self.object_type
    }
}
pub trait NumberProtocol<'gc>: ObjectProtocol<'gc> {}
pub trait SequenceProtocol<'gc>: ObjectProtocol<'gc> {}
pub trait MappingProtocol<'gc>: ObjectProtocol<'gc> {}
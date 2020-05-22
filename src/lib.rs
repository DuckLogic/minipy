use std::error::Error;
use std::fmt::{self, Display, Formatter};

pub mod bytecode;
pub mod objects;

pub type PyResult<T> = Result<T, PyErr>;

#[derive(Debug)]
pub struct PyErr(Box<PyErrRepr>);
impl PyErr {
    #[cold]
    pub fn new(builtin_type: BuiltinError) -> PyErr {
        PyErr(Box::new(PyErrRepr {
            builtin_type,
            msg: None
        }))
    }
    #[cold]
    pub fn with_msg<T: Display>(builtin_type: BuiltinError, msg: T) -> PyErr {
        PyErr(Box::new(PyErrRepr {
            builtin_type,
            msg: Some(msg.to_string())
        }))
    }
}
impl Error for PyErr {}
impl Display for PyErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let type_name = self.0.builtin_type.type_name();
        match self.0.msg {
            Some(ref msg) => {
                write!(f, "{}: {}", type_name, msg)
            },
            None => {
                f.write_str(type_name)
            }
        }
    }
}
#[derive(Debug)]
struct PyErrRepr {
    builtin_type: BuiltinError,
    msg: Option<String>
}
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BuiltinError {
    Unimplemented
}
impl BuiltinError {
    pub fn type_name(&self) -> &'static str {
        match *self {
            BuiltinError::Unimplemented => "UnimplementedError",
        }
    }
}

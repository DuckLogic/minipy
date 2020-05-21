use indexmap::set::IndexSet;
use rustpython_parser::ast::Location;

pub mod compiler;

#[derive(Debug, PartialEq)]
pub struct FunctionDef {
    name: String,
    args: Vec<String>,
    code: CodeObject
}
#[derive(Debug, PartialEq)]
pub struct CodeObject {
    bytecode: Vec<BytecodeOp>,
    names: IndexSet<String>,
    ints: Vec<i32>,
    floats: Vec<f64>,
    string_consts: Vec<String>,
    /// A sorted list of the locations of each bytecode
    locations: Vec<(usize, Location)>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BytecodeOp {
    Invalid,
    // Errors
    AssertionFail {
        has_msg: bool
    },
    // Stack manipulation
    Pop,
    Dup,
    MoveBack {
        amount: u16
    },
    /// Essentially `MoveBack { amount: 1 }`
    Swap,
    // Boolean ops
    BoolAnd,
    BoolOr,
    // Binary ops
    Add,
    Subtract,
    Multiply,
    MatrixMultiply,
    Divide,
    FloorDivide,
    Modulo,
    Power,
    ShiftLeft,
    ShiftRight,
    BitAnd,
    BitOr,
    BitXor,
    // Subscript/Index
    /// Performs `top[second]`
    Subscript,
    // Unary ops
    UnaryPos,
    UnaryNeg,
    UnaryNot,
    UnaryBinInv,
    // Comparison
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    // Identity testing
    Is,
    IsNot,
    // Container tests
    In,
    NotIn,
    // Loads
    LoadAttr {
        name_index: u16
    },
    LoadLocal {
        index: u16,
    },
    LoadGlobal {
        name_index: u16
    },
    // Calls
    BasicInvoke {
        num_args: u8
    },
    // Consts
    LoadFloat {
        index: u16
    },
    LoadInt {
        index: u16
    },
    LoadString {
        index: u16
    },
    LoadTrue,
    LoadFalse,
    LoadNone,
    // Collections
    BuildList {
        size: u16
    },
    BuildTuple {
        size: u16
    },
    BuildSet {
        size: u16
    },
    BuildDict {
        size: u16
    },
    MakeIter,
    // Jumps
    JumpIf {
        target: u16
    },
    JumpIfNot {
        target: u16
    },
    Jump {
        target: u16
    },
    ReturnVal,
    ReturnNone,
    // Import
    /// Import the specified module, pushing it onto the stack
    Import {
        name_index: u16
    },
    // Stores
    StoreLocal {
        index: u16
    },
    StoreGlobal {
        name_index: u16
    },
    /// Attempt to take the next value from the iterator.
    ///
    /// The iterator is peeked (but not consumed) from the top of the stack.
    ///
    /// If the value is present,
    /// it will store the value into the local var with the specified index
    /// and push `True` onto the stack.
    ///
    /// Otherwise it wont store anything and push `False` onto the stack.
    StoreNextIter {
        index: u16
    },
}
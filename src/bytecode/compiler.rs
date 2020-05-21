use rustpython_parser::ast::{Expression, ExpressionType, BooleanOperator, Operator, UnaryOperator, Comparison, Number, Statement, StatementType, Suite, StringGroup, Parameters, Varargs};
use crate::bytecode::{BytecodeOp, FunctionDef, CodeObject};
use indexmap::IndexSet;
use std::convert::TryFrom;
use rustpython_parser::location::Location;
use num_traits::cast::ToPrimitive;
use indexmap::map::IndexMap;

enum VarContext {
    Function {
        defined_vars: IndexSet<String>,
    },
    Module {
        funcs: IndexMap<String, FunctionDef>
    }
}
struct LoopContext {
    /// The start of the loop, where the conditional is tested
    start: u16,
    /// Jump instructions that need to be fixed to pont to the end of the loop
    end_jumps: Vec<usize>,
}

pub struct BytecodeCompiler {
    context: VarContext,
    result: Vec<BytecodeOp>,
    names: IndexSet<String>,
    ints: Vec<i32>,
    floats: Vec<f64>,
    string_consts: Vec<String>,
    /// A sorted list of the locations of each bytecode
    locations: Vec<(usize, Location)>,
    // State
    current_loop: Option<LoopContext>,
}
impl BytecodeCompiler {
    pub fn into_code(self) -> CodeObject {
        CodeObject {
            bytecode: self.result,
            names: self.names,
            ints: self.ints,
            floats: self.floats,
            string_consts: self.string_consts,
            locations: self.locations
        }
    }
    pub fn define_name(&mut self, name: &str) -> u16 {
        let index = if let Some((index, _)) = self.names.get_full(name) {
            index
        } else {
            self.names.insert_full(name.into()).0
        };
        u16::try_from(index).unwrap()
    }
    pub fn compile_statement(&mut self, target: &Statement) {
        use super::BytecodeOp::*;
        self.locations.push((self.result.len(), target.location.clone()));
        match &target.node {
            StatementType::Break => {
                self.current_loop.as_mut().unwrap()
                    .end_jumps.push(self.result.len());
                self.result.push(Invalid);
            },
            StatementType::Continue => {
                let start = self.current_loop.as_ref().unwrap().start;
                self.result.push(Jump { target: start })
            },
            StatementType::Return { value: Some(val) } => {
                self.compile_expr(val);
                self.result.push(ReturnVal);
            },
            StatementType::Return { value: None } => {
                self.result.push(ReturnNone)
            }
            StatementType::Import { names } => {
                for symbol in names {
                    assert_ne!(symbol.symbol, "*");
                    let name_index = self.define_name(&symbol.symbol);
                    assert_eq!(symbol.alias, None);
                    self.result.push(Import { name_index });
                    let result_index = match symbol.alias {
                        Some(ref alias) => self.define_name(&**alias),
                        None => name_index
                    };
                    self.result.push(StoreGlobal { name_index: result_index });
                }
            },
            StatementType::ImportFrom {
                level, module,
                names
            } => {
                assert_eq!(*level, 0);
                let module_name_index = self.define_name(module.as_ref().unwrap());
                self.result.push(Import {
                    name_index: module_name_index
                });
                let mut names = names.iter();
                while let Some(symbol) = names.next() {
                    if names.len() != 0 {
                        self.result.push(Dup);
                    }
                    assert_eq!(symbol.alias, None);
                    let name_index = self.define_name(&symbol.symbol);
                    self.result.push(LoadAttr { name_index });
                    self.result.push(StoreGlobal { name_index });
                }
            },
            StatementType::Pass => {},
            StatementType::Assert { test, msg } => {
                self.compile_expr(test);
                let jump_end_insn = self.result.len();
                self.result.push(Invalid);
                if let Some(ref msg) = msg {
                    self.compile_expr(msg);
                }
                self.result.push(BytecodeOp::AssertionFail {
                    has_msg: msg.is_some()
                });
                let end_target = u16::try_from(self.result.len()).unwrap();
                self.result[jump_end_insn] = JumpIfNot { target: end_target };
            },
            StatementType::Delete { .. } => unimplemented!("delete"),
            StatementType::Assign { targets, value } => {
                assert_eq!(targets.len(), 1);
                let name = match &targets[0].node {
                    ExpressionType::Identifier { name } => name,
                    _ => panic!("Invalid targets: {:?}", targets)
                };
                self.compile_expr(value);
                match self.context {
                    VarContext::Function { ref mut defined_vars } => {
                        let index = u16::try_from(defined_vars.insert_full(name.clone()).0).unwrap();
                        self.result.push(StoreLocal { index });
                    },
                    VarContext::Module { .. } => {
                        let name_index = self.define_name(name);
                        self.result.push(StoreGlobal { name_index })
                    },
                }
            },
            StatementType::AugAssign { .. } => unimplemented!("assign ops"),
            StatementType::AnnAssign { .. } => unimplemented!("annotated assisgnments"),
            StatementType::Expression { expression } => {
                self.compile_expr(expression);
                self.result.push(Pop);
            },
            StatementType::Global { .. } => unimplemented!("global"),
            StatementType::Nonlocal { .. } => unimplemented!("nonlocal"),
            StatementType::If { test, body, orelse } => {
                self.compile_expr(test);
                let jump_insn = self.result.len();
                self.result.push(Invalid);
                self.compile_suite(body);
                match orelse {
                    Some(orelse) => {
                        let jump_end_insn = self.result.len();
                        self.result.push(Invalid);
                        let else_loc = u16::try_from(self.result.len()).unwrap();
                        self.result[jump_insn] = JumpIfNot { target: else_loc };
                        self.compile_suite(orelse);
                        let end_loc = u16::try_from(self.result.len()).unwrap();
                        self.result[jump_end_insn] = Jump { target: end_loc };
                    },
                    None => {
                        let end_loc = u16::try_from(self.result.len()).unwrap();
                        self.result[jump_insn] = JumpIfNot { target: end_loc };
                    }
                }
            },
            StatementType::While { test, body, orelse } => {
                assert!(orelse.is_none());
                self.with_loop(
                    |compiler| compiler.compile_expr(test),
                    |compiler| compiler.compile_suite(body)
                );
            },
            StatementType::With { .. } => unimplemented!("with (context managers)"),
            StatementType::For {
                is_async, target,
                iter, body,
                orelse
            } => {
                assert!(!is_async);
                assert!(orelse.is_none());
                let name = match &target.node {
                    ExpressionType::Identifier { name } => name,
                    _ => panic!("Invalid for target: {:?}", target)
                };
                let index = match self.context {
                    VarContext::Function { ref mut defined_vars } => {
                        u16::try_from(defined_vars.insert_full(name.clone()).0).unwrap()
                    },
                    VarContext::Module { .. } => unimplemented!("module for loop"),
                };
                self.compile_expr(iter);
                self.result.push(MakeIter);
                self.with_loop(
                    |compiler| {
                        compiler.result.push(StoreNextIter { index })
                    },
                    |compiler| compiler.compile_suite(body)
                );
                self.result.push(Pop); // Drop iterator
            },
            StatementType::Raise { .. } => {
                unimplemented!("raise");
            },
            StatementType::Try { .. } => unimplemented!("try/catch"),
            StatementType::ClassDef { .. } => unimplemented!("classes"),
            StatementType::FunctionDef {
                is_async, name, args, body,
                decorator_list, returns
            } => {
                assert!(!is_async);
                assert!(returns.is_none());
                assert!(decorator_list.is_empty());
                assert!(returns.is_none());
                let Parameters {
                    args,
                    kwonlyargs,
                    vararg, kwarg,
                    defaults,
                    kw_defaults
                } = &**args;
                assert!(kwonlyargs.is_empty());
                assert_eq!(*vararg, Varargs::None);
                assert_eq!(*kwarg, Varargs::None);
                assert!(defaults.is_empty());
                assert!(kw_defaults.is_empty());
                let mut defined_vars = IndexSet::new();
                for arg in args {
                    assert!(defined_vars.insert(arg.arg.clone()));
                }
                match self.context {
                    VarContext::Function { .. } => unimplemented!("nested funcs"),
                    VarContext::Module { ref mut funcs } => {
                        assert!(funcs.contains_key(name), "Already defined {:?}", name);
                        let mut func_compiler = BytecodeCompiler {
                            context: VarContext::Function { defined_vars },
                            result: vec![],
                            names: Default::default(),
                            ints: vec![],
                            floats: vec![],
                            string_consts: vec![],
                            locations: vec![],
                            current_loop: None
                        };
                        func_compiler.compile_suite(body);
                        assert_eq!(funcs.insert(name.clone(), FunctionDef {
                            name: name.clone(),
                            args: args.iter().map(|arg| arg.arg.clone()).collect(),
                            code: func_compiler.into_code()
                        }), None);
                    },
                }
            },
        }
    }
    fn with_loop<C, B>(&mut self, condition: C, body: B)
        where C: FnOnce(&mut Self), B: FnOnce(&mut Self) {
        let start = u16::try_from(self.result.len()).unwrap();
        let old_loop = self.current_loop.replace(LoopContext {
            start, end_jumps: vec![]
        });
        condition(&mut *self);
        let finish_jump = self.result.len();
        self.result.push(BytecodeOp::Invalid);
        body(&mut *self);
        self.result.push(BytecodeOp::Jump { target: start });
        let end_loc = u16::try_from(self.result.len()).unwrap();
        self.result[finish_jump] = BytecodeOp::JumpIfNot { target: end_loc };
        for &end_jump in &self.current_loop.as_mut().unwrap().end_jumps {
            self.result[end_jump] = BytecodeOp::JumpIfNot { target: end_loc };
        }
        // Restore old context
        self.current_loop = old_loop;
    }
    pub fn compile_suite(&mut self, suite: &Suite) {
        for statement in &**suite {
            self.compile_statement(statement);
        }
    }
    /// Compile the specified expression,
    /// pushing its result onto the top of the stack
    pub fn compile_expr(&mut self, target: &Expression) {
        use super::BytecodeOp::*;
        self.locations.push((self.result.len(), target.location.clone()));
        match &target.node {
            ExpressionType::BoolOp { op, values } => {
                self.compile_expr(&values[0]);
                for other in &values[1..] {
                    self.compile_expr(other);
                    match *op {
                        BooleanOperator::And => {
                            self.result.push(BoolAnd);
                        },
                        BooleanOperator::Or => {
                            self.result.push(BoolOr);
                        },
                    }
                }
            },
            ExpressionType::Binop { a, op, b } => {
                self.compile_expr(a);
                self.compile_expr(b);
                match *op {
                    Operator::Add => {
                        self.result.push(Add);
                    },
                    Operator::Sub => {
                        self.result.push(Subtract);
                    },
                    Operator::Mult => {
                        self.result.push(Multiply);
                    },
                    Operator::MatMult => {
                        self.result.push(MatrixMultiply);
                    },
                    Operator::Div => {
                        self.result.push(Divide)
                    },
                    Operator::Mod => {
                        self.result.push(Modulo);
                    },
                    Operator::Pow => {
                        self.result.push(Power)
                    },
                    Operator::LShift => {
                        self.result.push(ShiftLeft)
                    },
                    Operator::RShift => {
                        self.result.push(ShiftRight)
                    },
                    Operator::BitOr => {
                        self.result.push(BitOr)
                    },
                    Operator::BitXor => {
                        self.result.push(BitXor)
                    },
                    Operator::BitAnd => {
                        self.result.push(BitAnd)
                    },
                    Operator::FloorDiv => {
                        self.result.push(FloorDivide)
                    },
                }
            },
            ExpressionType::Subscript { a, b } => {
                self.compile_expr(a);
                self.compile_expr(b);
                self.result.push(Subscript);
            },
            ExpressionType::Unop { op, a } => {
                self.compile_expr(a);
                self.result.push(match *op {
                    UnaryOperator::Pos => UnaryPos,
                    UnaryOperator::Neg => UnaryNeg,
                    UnaryOperator::Not => UnaryNot,
                    UnaryOperator::Inv => UnaryBinInv,
                })
            },
            ExpressionType::Await { .. } => unimplemented!("await"),
            ExpressionType::Yield { .. } |
            ExpressionType::YieldFrom { .. } => unimplemented!("yield"),
            ExpressionType::Compare { vals, ops } => {
                assert_eq!(vals.len(), ops.len() - 1, "vals = {:?}, ops = {:?}", vals, ops);
                let mut vals = vals.iter();
                let mut ops = ops.iter();
                self.compile_expr(vals.next().unwrap());
                let mut had_prev = false;
                while let Some(op) = ops.next() {
                    self.compile_expr(vals.next().unwrap());
                    if ops.len() != 0 {
                        // Duplicate for sake of next comparison
                        self.result.push(Dup);
                        if had_prev {
                            // But make sure we're behind the other op and the boolean
                            self.result.push(MoveBack { amount: 2 });
                        } else {
                            // Make sure we're behind the other op
                            self.result.push(MoveBack { amount: 1 })
                        }
                    }
                    self.result.push(match *op {
                        Comparison::Equal => Equals,
                        Comparison::NotEqual => NotEquals,
                        Comparison::Less => LessThan,
                        Comparison::LessOrEqual => LessThanOrEqual,
                        Comparison::Greater => GreaterThan,
                        Comparison::GreaterOrEqual => GreaterThanOrEqual,
                        Comparison::In => In,
                        Comparison::NotIn => NotIn,
                        Comparison::Is => Is,
                        Comparison::IsNot => IsNot,
                    });
                    if had_prev {
                        // We have to combine our results
                        self.result.push(BoolAnd);
                    }
                    had_prev = true;
                }
            },
            ExpressionType::Attribute { value, name } => {
                let name_index = self.define_name(name);
                self.compile_expr(value);
                self.result.push(LoadAttr { name_index });
            },
            ExpressionType::Call { function, args,
                keywords } => {
                if keywords.is_empty() {
                    self.compile_expr(function);
                    args.iter().for_each(|arg| self.compile_expr(arg));
                    self.result.push(BytecodeOp::BasicInvoke {
                        num_args: u8::try_from(args.len()).unwrap()
                    })
                }
            },
            ExpressionType::Number { value } => {
                match value {
                    Number::Integer { value } => {
                        let index = u16::try_from(self.ints.len()).unwrap();
                        self.ints.push(value.to_i32().expect("Overflow"));
                        self.result.push(BytecodeOp::LoadInt { index })
                    },
                    Number::Float { value } => {
                        let index = u16::try_from(self.floats.len()).unwrap();
                        self.floats.push(*value);
                        self.result.push(BytecodeOp::LoadFloat { index });
                    },
                    Number::Complex { .. } => unimplemented!("complex numbers"),
                }
            },
            ExpressionType::List { elements } => {
                elements.iter().for_each(|e| self.compile_expr(e));
                self.result.push(BuildList { size: u16::try_from(elements.len()).unwrap() })
            },
            ExpressionType::Tuple { elements } => {
                elements.iter().for_each(|e| self.compile_expr(e));
                self.result.push(BuildList {
                    size: u16::try_from(elements.len()).unwrap()
                });
            },
            ExpressionType::Dict { elements } => {
                for (key, element) in elements {
                    let key = key.as_ref()
                        .unwrap_or_else(|| panic!("Invalid dict: {:?}", elements));
                    self.compile_expr(key);
                    self.compile_expr(element);
                }
                self.result.push(BuildDict {
                    size: u16::try_from(elements.len()).unwrap()
                })
            },
            ExpressionType::Set { elements } => {
                elements.iter().for_each(|e| self.compile_expr(e));
                self.result.push(BuildSet {
                    size: u16::try_from(elements.len()).unwrap()
                });
            },
            ExpressionType::Comprehension { .. } => unimplemented!("comprehensions"),
            ExpressionType::Starred { .. } => unimplemented!("starred exprs"),
            ExpressionType::Slice { elements } => {
                assert!(elements.len() <= 3);
                let name_index = self.define_name("slice");
                self.result.push(BytecodeOp::LoadGlobal { name_index });
                self.result.push(BytecodeOp::BasicInvoke {
                    num_args: elements.len() as u8
                })
            },
            ExpressionType::String { value } => {
                match value {
                    StringGroup::Constant { value } => {
                        let index = u16::try_from(self.result.len()).unwrap();
                        self.string_consts.push(value.clone());
                        self.result.push(LoadString { index });
                    },
                    StringGroup::FormattedValue { .. } => unimplemented!("format strings"),
                    StringGroup::Joined { .. } => unimplemented!("joined strings"),
                }
            },
            ExpressionType::Bytes { .. } => unimplemented!("byte literals"),
            ExpressionType::Identifier { name } => {
                let mut loaded = false;
                match self.context {
                    VarContext::Function { ref defined_vars } => {
                        if let Some((index, _)) = defined_vars.get_full(name) {
                            self.result.push(LoadLocal {
                                index: u16::try_from(index).unwrap()
                            });
                            loaded = true;
                        }
                    },
                    VarContext::Module { .. } => {}, // Always load global
                }
                if !loaded {
                    let name_index = self.define_name(name);
                    self.result.push(LoadGlobal { name_index });
                }
            },
            ExpressionType::Lambda { ..  } => {
                unimplemented!("lambda")
            },
            ExpressionType::IfExpression { test, body, orelse } => {
                self.compile_expr(test);
                let jump_if_insn = self.result.len();
                self.result.push(Invalid);
                self.compile_expr(orelse);
                let else_target = u16::try_from(self.result.len()).unwrap();
                self.result[jump_if_insn] = JumpIf { target: else_target };
                let jump_end_insn = self.result.len();
                self.result.push(Invalid);
                self.compile_expr(body);
                let end_target = u16::try_from(self.result.len()).unwrap();
                self.result[jump_end_insn] = Jump { target: end_target };
            },
            ExpressionType::True => {
                self.result.push(LoadTrue);
            },
            ExpressionType::False => {
                self.result.push(LoadFalse);
            },
            ExpressionType::None => {
                self.result.push(LoadNone);
            },
            ExpressionType::Ellipsis => unimplemented!("ellipsis"),
        }
    }
}


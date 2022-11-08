//! This modules contains utilities to implement the Local Value Numbering (LVN)
//! compiler optimization technique on bril programs. LVN is a local optimization
//! technique used to handle multiple classes of local optimizations, such as, dead-code
//! elimination, copy propagation, constant propagation, common sub-expression elimination
//! etc.
//! The above mentioned optimization problems have a common underlying theme in that they
//! conflate VALUES and VARIABLES.
use bril_control_flow::BasicBlock;
use bril_rs::{Code, Function, Instruction, Literal as Briliteral, ValueOps};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Literal {
    /// Integers
    Int(i64),
    /// Booleans
    Bool(bool),
}

impl From<Briliteral> for Literal {
    fn from(literal: Briliteral) -> Self {
        match literal {
            Briliteral::Int(n) => Literal::Int(n),
            Briliteral::Bool(v) => Literal::Bool(v),
            Briliteral::Float(_) => unimplemented!("Floats not supported yet!"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Value {
    Const(Literal),
    NonConst {
        op: ValueOps,
        value_numbers: Vec<u32>,
    },
}

impl Value {
    pub fn new_const(literal: Literal) -> Self {
        Value::Const(literal)
    }

    pub fn new_non_const(op: ValueOps, value_numbers: Vec<u32>) -> Self {
        Value::NonConst { op, value_numbers }
    }
}

/// Provides some utility functions
/// over the `Code` type that represents
/// labels and instructions in bril IR.
trait CodeUtil {
    /// Tries to derive the name of the destination
    /// variable if this is a `Code` that might contain
    /// a destination variable at all.
    fn try_get_dest(&self) -> Option<String>;

    /// Updates the destination variable if this
    /// is a `Code` that might contain a destination
    /// variable at all.
    fn set_dest(&mut self, new_dest: String);
}

impl CodeUtil for Code {
    fn try_get_dest(&self) -> Option<String> {
        match self {
            Code::Instruction(instr) => match instr {
                Instruction::Constant { dest, .. } | Instruction::Value { dest, .. } => {
                    Some(dest.clone())
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn set_dest(&mut self, new_dest: String) {
        if let Code::Instruction(instr) = self {
            match instr {
                Instruction::Constant { dest, .. } | Instruction::Value { dest, .. } => {
                    *dest = new_dest
                }
                _ => {}
            }
        }
    }
}

/// Module containing types used by the `LVNer`
/// type to maintain context when carrying out
/// local value numbering over basic blocks of
/// a bril function.
mod lvn_context {
    use super::Value;
    use std::collections::HashMap;
    use std::ops::{Deref, DerefMut};

    #[derive(Debug, Clone, Eq, PartialEq)]
    pub struct Value2ValueNumber {
        map: HashMap<Value, u32>,
    }

    impl Value2ValueNumber {
        pub fn new() -> Self {
            Self {
                map: HashMap::new(),
            }
        }

        pub fn insert(&mut self, value: Value, value_number: u32) {
            if self.map.contains_key(&value) {
                panic!("How dare you try and assign a new value_number: [{}], to an existing value: [{:?}]!", value_number, value);
            }
            self.map.insert(value, value_number);
        }
    }

    impl Deref for Value2ValueNumber {
        type Target = HashMap<Value, u32>;

        fn deref(&self) -> &Self::Target {
            &self.map
        }
    }

    impl DerefMut for Value2ValueNumber {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.map
        }
    }

    #[derive(Debug, Clone, Eq, PartialEq)]
    pub struct Variable2ValueNumber {
        map: HashMap<String, u32>,
    }

    impl Variable2ValueNumber {
        pub fn new() -> Self {
            Self {
                map: HashMap::new(),
            }
        }
    }

    impl Deref for Variable2ValueNumber {
        type Target = HashMap<String, u32>;

        fn deref(&self) -> &Self::Target {
            &self.map
        }
    }

    impl DerefMut for Variable2ValueNumber {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.map
        }
    }

    #[derive(Debug, Clone, Eq, PartialEq)]
    pub struct ValueNumber2CanonicalVar {
        map: HashMap<u32, String>,
    }

    impl ValueNumber2CanonicalVar {
        pub fn new() -> Self {
            Self {
                map: HashMap::new(),
            }
        }

        pub fn insert(&mut self, value_number: u32, canonical_var: String) {
            if self.map.contains_key(&value_number) {
                panic!("How dare you try and assign a new canonical var: [{}], to an existing value_number: [{}]!", canonical_var, value_number);
            }
            self.map.insert(value_number, canonical_var);
        }
    }

    impl Deref for ValueNumber2CanonicalVar {
        type Target = HashMap<u32, String>;

        fn deref(&self) -> &Self::Target {
            &self.map
        }
    }

    impl DerefMut for ValueNumber2CanonicalVar {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.map
        }
    }
}

/// A type that maintains the context required for
/// carrying out local value numbering on a basic
/// block.
#[derive(Debug, Clone)]
pub struct LVNer {
    /// Number of values existing in the value table.
    num_values: u32,
    /// Used as a suffix when creation temporaries
    /// that need to be set as canonical variables
    /// when the "actual" variable in question is written
    /// to multiple times in a basic block.
    temp_counter: u32,
    /// Mapping from a value to the value number associated to
    /// its canonical variable. Essentially associates a `Value`
    /// to its canonical variable, via the `num2var` table.
    val2num: lvn_context::Value2ValueNumber,
    /// Mapping from a value number to the value's
    /// canonical variable. Together with the `val2num`
    /// table, this table helps us go from a `Value` to
    /// its canonical variable.
    num2var: lvn_context::ValueNumber2CanonicalVar,
    /// Keeps track of the current value of a variable.
    /// Variables in this table can be updated multiply
    /// if they are written to multiple times in a basic
    /// block.
    var2num: lvn_context::Variable2ValueNumber,
}

impl LVNer {
    pub fn new() -> Self {
        Self {
            num_values: 0,
            temp_counter: 0,
            val2num: lvn_context::Value2ValueNumber::new(),
            var2num: lvn_context::Variable2ValueNumber::new(),
            num2var: lvn_context::ValueNumber2CanonicalVar::new(),
        }
    }

    fn get_temporary_var(&mut self) -> String {
        let result = format!("t{}", self.temp_counter);
        self.temp_counter += 1;
        result
    }

    fn get_fresh_local_value_numer(&mut self) -> u32 {
        self.num_values += 1;
        self.num_values
    }

    /// Returns the `Value` representation of a Bril instruction, if possible.
    fn try_into_canonicalized_value(&self, instr: &Code) -> Option<Value> {
        match instr {
            Code::Instruction(instr) => match instr {
                Instruction::Constant { value, .. } => Some(Value::new_const(value.clone().into())),
                Instruction::Value { op, args, .. } => {
                    let value_numbers = args.iter().map(|arg| self.var2num[arg]).collect();

                    let mut value = Value::new_non_const(*op, value_numbers);
                    self.canonicalize_value(&mut value);
                    Some(value)
                }
                _ => None,
            },
            _ => None,
        }
    }

    /// Canonicalizes a `Value`, associated to an instruction using multiple
    /// arguments, in order to have a standard representation of the value.
    /// The method achieves normalization by sorting a `Value`'s `value_numbers`
    /// in increasing order.
    fn canonicalize_value(&self, value: &mut Value) {
        if let Value::NonConst { op, value_numbers } = value {
            // Normalization can only happen for commutative operations
            match op {
                ValueOps::Add
                | ValueOps::Mul
                | ValueOps::Eq
                | ValueOps::And
                | ValueOps::Or
                | ValueOps::Fadd
                | ValueOps::Fmul
                | ValueOps::Feq => value_numbers.sort_unstable(),
                _ => {}
            }
        }
    }

    /// Modifies the `Code` by replacing all its arguments
    /// with the canonical variables associated to their respective
    /// values.
    fn canonicalize(&self, code: &mut Code) {
        if let Code::Instruction(instr) = code {
            match instr {
                Instruction::Value { args, .. } | Instruction::Effect { args, .. } => {
                    for arg in args {
                        // Indexing directly here as -
                        // 1/ Each variable/argument in the current `Code`
                        //    should have been assigned a value number.
                        // 2/ Each value number used in the basic block must
                        //    have been associated to a canonical variable.
                        *arg = self.num2var[&self.var2num[arg]].clone();
                    }
                }
                _ => {}
            }
        }
    }

    fn number_local_values(&mut self, bb: &mut BasicBlock) {
        for (last_write, instr) in bb.last_writes().iter().zip(bb.instrs.iter_mut()) {
            // A `Value` representation is possible for `Constant`
            // and `Value` instructions only.
            if let Some(curr_value) = self.try_into_canonicalized_value(instr) {
                // If this is a `Code` that can be converted into a value
                // then we know for sure that it has a destination.
                let curr_dest = instr.try_get_dest().unwrap();

                let value_number =
                    if let Some(value_number) = self.val2num.get(&curr_value).copied() {
                        // Replace current instruction with id of canonical var.
                        // Without this "copy propagation" is not going to work.
                        // let canonical_var = self.value_num_to_canonical_var.get(&value_number)
                        //     .cloned()
                        //     .expect("A canonical variable must have been present for an existing value number!");

                        // TODO: This is where we will likely take care of "constant propagation"
                        // as well. Check if the value associated to the canonical var is a constant.

                        // TODO: Constant folding???

                        value_number
                    } else {
                        // If the current instruction's destination is overwritten
                        // at a later point in the basic block, replace the
                        // destination with a temporary variable.
                        //
                        // To understand this better, let's assume we have the following
                        // sequence of instructions -
                        // ```
                        // (1) a: int = const 4;
                        // (2) b: int = const 2;
                        // (3) sum1: int = add a b;
                        // (4) sum1: int = const 0;
                        // (5) sum2: int = add a b;
                        // ```
                        // Essentially, the `sum1` cannot be the canonical variable for
                        // the value `(add, a(4), b(2))`, since it will be overwritten on line (4).
                        // Then, on line(5) it will be wrong if `sum2` tries to use `sum1` as the
                        // canonical variable for the value `(add, a(4), b(2))`, as `sum1` is now
                        // actually 0.
                        let maybe_temp = if !last_write {
                            let temp = self.get_temporary_var();
                            instr.set_dest(temp.clone());
                            temp
                        } else {
                            curr_dest.clone()
                        };

                        let value_number = self.get_fresh_local_value_numer();
                        // Else, create a new value for the current instruction
                        self.val2num.insert(curr_value, value_number);
                        self.num2var.insert(value_number, maybe_temp.clone());

                        value_number
                    };

                // Insert the current destination's value number
                self.var2num.insert(curr_dest, value_number);
            }

            // Canonicalize instruction
            self.canonicalize(instr);
        }
    }
}

pub fn run_lvn(func: &mut Function) {
    let mut bbs = BasicBlock::to_basic_blocks(func);

    for bb in &mut bbs {
        let mut lvner = LVNer::new();
        lvner.number_local_values(bb);
    }

    func.instrs = BasicBlock::flatten(bbs);
}

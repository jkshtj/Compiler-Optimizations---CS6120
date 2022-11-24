//! This modules contains utilities to implement the Local Value Numbering (LVN)
//! compiler optimization technique on bril programs. LVN is a local optimization
//! technique used to handle multiple classes of local optimizations, such as, dead-code
//! elimination, copy propagation, constant propagation, common sub-expression elimination
//! etc.
//! The above mentioned optimization problems have a common underlying theme in that they
//! conflate VALUES and VARIABLES.
use bril_control_flow::BasicBlock;
use bril_rs::{Code, ConstOps, Function, Instruction, Literal as Briliteral, ValueOps};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Literal {
    /// Integers
    Int(i64),
    /// Booleans
    Bool(bool),
}

impl Literal {
    pub fn ltype(&self) -> bril_rs::Type {
        match self {
            Literal::Int(_) => bril_rs::Type::Int,
            Literal::Bool(_) => bril_rs::Type::Bool,
        }
    }

    /// Panics if the literal does not contain an integer.
    pub fn into_int(self) -> i64 {
        match self {
            Literal::Int(n) => n,
            _ => panic!("Literal does not contain an int: {:?}", self),
        }
    }

    /// Panics if the literal does not contain a bool.
    pub fn into_bool(self) -> bool {
        match self {
            Literal::Bool(v) => v,
            _ => panic!("Literal does not contain a bool: {:?}", self),
        }
    }
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

impl From<Literal> for Briliteral {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Int(n) => Briliteral::Int(n),
            Literal::Bool(v) => Briliteral::Bool(v),
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
    use bril_rs::ValueOps;
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

        /// Returns the number associated to the `Value`, if it is
        /// present in the table. If the lookup key is a value
        /// representing a [ValueOps::Id](bril_rs::ValueOps::Id)
        /// operation, then instead of relaying the message that the
        /// value does not exist - which in reality, it doesn't, the
        /// value number of the argument to the id operation is returned
        /// instead.
        ///
        /// The above logic prevents us from creating a chain of assignment/ValueOps::Id
        /// operations, that all resolve to a single value. An example can be seen
        /// below.
        /// ```
        ///   x: int = const 4;
        ///   jmp .label;
        /// .label:
        ///   copy1: int = id x;
        ///   copy2: int = id copy1; /* This is redundant. Instead of assigning `copy1` here, we can just assign its value - `x` in this case. */
        ///   copy3: int = id copy2;
        ///   print x;
        /// ```
        ///
        /// It also helps us implement "Copy Propagation" in our LVN
        /// algorithm - by replacing the occurrences of targets of
        /// direct assignments with their values.
        pub fn get(&self, value: &Value) -> Option<u32> {
            match value {
                Value::NonConst { op, value_numbers } if *op == ValueOps::Id => {
                    Some(value_numbers[0])
                }
                _ => self.map.get(value).copied(),
            }
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

    fn get_fresh_local_value_number(&mut self) -> u32 {
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

    fn try_fold(&self, num2const: &HashMap<u32, Literal>, value: &Value) -> Option<Literal> {
        fn is_foldable_op(op: ValueOps) -> bool {
            match op {
                // Int arithmetic
                ValueOps::Add | ValueOps::Sub | ValueOps::Mul | ValueOps::Div => true,

                // Float arithmetic
                // TODO: Not supported yet, but leaving
                // here if we want to extend functionality
                // in future.
                ValueOps::Fadd | ValueOps::Fsub | ValueOps::Fmul | ValueOps::Fdiv => false,

                // Int comparisons
                ValueOps::Eq | ValueOps::Lt | ValueOps::Gt | ValueOps::Le | ValueOps::Ge => true,

                // Float comparisons
                // TODO: Not supported yet, but leaving
                // here if we want to extend functionality
                // in future.
                ValueOps::Feq | ValueOps::Flt | ValueOps::Fgt | ValueOps::Fle | ValueOps::Fge => {
                    false
                }

                // Logic operations - operands are bools
                ValueOps::Not | ValueOps::And | ValueOps::Or => true,

                _ => false,
            }
        }

        if let Value::NonConst { op, value_numbers } = value {
            if is_foldable_op(*op) {
                let const_args: Vec<Literal> = value_numbers
                    .iter()
                    .filter_map(|n| num2const.get(n))
                    .copied()
                    .collect();

                // Folding is possible only if all arguments
                // are constants.
                if const_args.len() == value_numbers.len() {
                    let folded = match op {
                        ValueOps::Add => {
                            Literal::Int(const_args[0].into_int() + const_args[1].into_int())
                        }
                        ValueOps::Sub => {
                            Literal::Int(const_args[0].into_int() - const_args[1].into_int())
                        }
                        ValueOps::Mul => {
                            Literal::Int(const_args[0].into_int() * const_args[1].into_int())
                        }
                        ValueOps::Div => {
                            Literal::Int(const_args[0].into_int() / const_args[1].into_int())
                        }

                        // TODO: Folding of `==` comparison is more complicated.
                        // Not all arguments are required to be constants or known -> (arg1 == arg1) is always true, even if we don't know what `arg1` is.
                        ValueOps::Eq => {
                            Literal::Bool(const_args[0].into_int() == const_args[1].into_int())
                        }

                        ValueOps::Lt => {
                            Literal::Bool(const_args[0].into_int() < const_args[1].into_int())
                        }
                        ValueOps::Gt => {
                            Literal::Bool(const_args[0].into_int() > const_args[1].into_int())
                        }
                        ValueOps::Le => {
                            Literal::Bool(const_args[0].into_int() <= const_args[1].into_int())
                        }
                        ValueOps::Ge => {
                            Literal::Bool(const_args[0].into_int() >= const_args[1].into_int())
                        }

                        // TODO: Folding of logical operations is more complicated.
                        // Not all arguments are required to be constants or known.
                        // For instance, (A && B) will always be false if either A or B is false.
                        ValueOps::Not => Literal::Bool(!const_args[0].into_bool()),
                        ValueOps::And => {
                            Literal::Bool(const_args[0].into_bool() && const_args[1].into_bool())
                        }
                        ValueOps::Or => {
                            Literal::Bool(const_args[0].into_bool() || const_args[1].into_bool())
                        }

                        _ => panic!(
                            "Should not have been here! Operation: {}, is not foldable!",
                            op
                        ),
                    };

                    return Some(folded);
                }
            }
        }

        None
    }

    fn number_local_values(&mut self, bb: &mut BasicBlock) {
        // Stores which value numbers are associated to canonical
        // variables that have constant values (or are `Literals`).
        // This information is useful to propagate constants into
        // expressions.
        let mut num2const: HashMap<u32, Literal> = HashMap::new();

        for var in bb.read_first() {
            let value_number = self.get_fresh_local_value_number();
            self.num2var.insert(value_number, var.clone());
            self.var2num.insert(var, value_number);
        }

        for (last_write, instr) in bb.last_writes().iter().zip(bb.instrs.iter_mut()) {
            // A `Value` representation is possible for `Constant`
            // and `Value` instructions only.
            if let Some(curr_value) = self.try_into_canonicalized_value(instr) {
                // If this is a `Code` that can be converted into a value
                // then we know for sure that it has a destination.
                let curr_dest = instr.try_get_dest().unwrap();

                let value_number = if let Some(value_number) = self.val2num.get(&curr_value) {
                    // Perform constant propagation if possible
                    if let Some(&literal) = num2const.get(&value_number) {
                        *instr = Code::Instruction(Instruction::Constant {
                            dest: curr_dest.clone(),
                            op: ConstOps::Const,
                            pos: None,
                            const_type: literal.ltype(),
                            value: literal.into(),
                        })
                    }

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

                    let value_number = self.get_fresh_local_value_number();

                    // Perform constant folding if possible
                    if let Some(literal) = self.try_fold(&num2const, &curr_value) {
                        num2const.insert(value_number, literal);
                    } else if let Value::Const(literal) = curr_value.clone() {
                        num2const.insert(value_number, literal);
                    }

                    // Perform constant propagation if possible
                    if let Some(&literal) = num2const.get(&value_number) {
                        *instr = Code::Instruction(Instruction::Constant {
                            dest: maybe_temp.clone(),
                            op: ConstOps::Const,
                            pos: None,
                            const_type: literal.ltype(),
                            value: literal.into(),
                        })
                    }

                    // Create a new value for the current instruction
                    self.val2num.insert(curr_value, value_number);
                    self.num2var.insert(value_number, maybe_temp);

                    value_number
                };

                // Updates the CURRENT/LATEST value associated to the
                // variable being used as the destination of the current
                // instruction.
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

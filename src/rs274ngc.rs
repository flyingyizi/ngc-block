use crate::{Box, Error, GCodes, GGroup, MCodes, MGroup, String, Vec};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "rs274ngc.pest"]
pub struct Rs274ngcParser;

pub use line::{set_set_parameter_function, Block};
pub use real::{set_get_parameter_function, Expr, RealValue};

#[derive(Error, Clone, Debug, Eq, PartialEq)]
pub enum Rs274Error {
    #[error("pest error occurred")]
    PestError(#[from] pest::error::Error<Rule>),

    #[error("bad format")]
    NceBadFormat,
    #[error("bad gcode modal group0")]
    NceBugBadGCodeModalGroup0,
    #[error("can not put ana in canned cycle")]
    NceCannotPutAnAInCannedCycle,
    #[error("can not use axis values without a gcode that uses them")]
    NceCannotUseAxisValuesWithoutAGcodeThatUsesThem,
    #[error("")]
    NceCannotUseAxisValuesWithG80,
    #[error("")]
    NceCannotUseG53Incremental,
    #[error("")]
    NceLineWithG10DoesNotHavel2,
    #[error("")]
    NceUnclosedCommentFound,
    #[error("")]
    NcePValueNotAnIntegerWithG10L2,
    #[error("")]
    NcePValueOutOfRangeWithG10L2,
    #[error("")]
    NceSscanfFailed,

    #[error("")]
    NceGCODEOutOfRange,
    #[error("")]
    NceMCODEOutOfRange,
    #[error("")]
    NceMCodeGreaterThan99,
    #[error("")]
    NceLineNumberGreaterThan99999,

    #[error("")]
    NceNegativeOrZeroQValueUsed,
    #[error("")]
    NceNegativeFWordUsed,
    #[error("")]
    NceNegativePWordUsed,
    #[error("")]
    NceNegativeHWordToolLengthOffsetIndexUsed,
    #[error("")]
    NceNegativeLWordUsed,
    #[error("")]
    NceNegativeMCodeUsed,

    #[error("")]
    NceMultipleWordsOnOneLine,
    #[error("")]
    NceMultipleAWordsOnOneLine,
    #[error("")]
    NceTwoMCodesUsedFromSameModalGroup,
    #[error("")]
    NceCannotUseTwoGCodethatBothUseAxisValues,
    #[error("")]
    NceTooManyMCodesOnLine,

    #[error("")]
    NceDWordWithNoG41OrG42,
    #[error("")]
    NceDwellTimeMissingWithG4,
    #[error("")]
    NceHWordWithNoG43,
    #[error("")]
    NceIWordWithNoG2OrG3OrG87ToUseIT,
    #[error("")]
    NceJWordWithNoG2OrG3OrG87ToUseIT,
    #[error("")]
    NceKWordWithNoG2OrG3OrG87ToUseIT,
    #[error("")]
    NceLWordWithNoCannedCycleOrG10,
    #[error("")]
    NcePWordwithNoG4G10G82G86G88G89,
    #[error("qword with no g83")]
    NceQWordWithNoG83,
    #[error("rword with no gcode that use it")]
    NceRwordWithNoGCodeThatUseIT,
    #[error("must use g0 or g1 with g53")]
    NceMustUseG0OrG1WithG53,
    #[error("all axes missing with g92")]
    NceAllAxesMissingWithG92,
    #[error("all axes missing with motioncode")]
    NceAllAxesMissingWithMotionCode,
}

/// distance_mode, include absolute and incremental
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DistanceMode {
    /// value is the destination value
    Absolute,
    /// destination value is current value plus the incremental
    Incremental,
}

/// for "real_value = { real_number | expression | parameter_value | unary_combo }" rule
mod real {
    use super::{Rs274ngcParser, Rule};

    use crate::{Box, String};
    use pest::pratt_parser::PrattParser;
    use pest::{iterators::Pair, Parser};

    lazy_static::lazy_static! {
        static ref PRATT_PARSER: PrattParser<Rule> = {
            use pest::pratt_parser::{Assoc::*, Op};
            // Precedence is defined lowest to highest
            PrattParser::new()
                .op(Op::infix(Rule::and, Left) | Op::infix(Rule::exclusive_or, Left) | Op::infix(Rule::minus, Left) | Op::infix(Rule::non_exclusive_or, Left) | Op::infix(Rule::plus, Left))
                .op(Op::infix(Rule::divided_by, Left) | Op::infix(Rule::modulo, Left) | Op::infix(Rule::times, Left))
                .op(Op::infix(Rule::power, Left))
        };
    }

    #[allow(dead_code)]
    #[doc(hidden)]
    static mut GET_PARAMETER_WEAK_FUNCTION: Option<fn(key: u32) -> Option<f32>> = None;

    #[allow(dead_code)]
    fn default_get_parameter_weak(_key: u32) -> Option<f32> {
        Some(1.0)
        // unimplemented!();
    }

    /// for Parameter Setting in rs274ngc, use it to set the function that query the parameter settings
    #[allow(dead_code)]
    pub fn set_get_parameter_function(func: fn(key: u32) -> Option<f32>) {
        unsafe {
            GET_PARAMETER_WEAK_FUNCTION = Some(func);
        }
    }

    #[allow(dead_code)]
    fn call_get_parameter_weak_function(key: u32) -> Option<f32> {
        unsafe {
            match GET_PARAMETER_WEAK_FUNCTION {
                Some(func) => func(key),
                None => default_get_parameter_weak(key),
            }
        }
    }
    #[derive(Debug, PartialEq)]
    enum BinaryOp {
        Power,
        Divide,
        Modulo,
        Times,
        And,
        ExclusiveOr,
        Minus,
        NoExclusiveOr,
        Plus,
    }
    impl BinaryOp {
        #[cfg(any(feature = "std", test))]
        pub fn dis(&self) -> String {
            match &self {
                &Self::Power => format!("{}", "^^"),
                &Self::Divide => format!("{}", "/"),
                &Self::Modulo => format!("{}", "mod"),
                &Self::Times => format!("{}", "*"),
                &Self::And => format!("{}", "and"),
                &Self::ExclusiveOr => format!("{}", "xor"),
                &Self::Minus => format!("{}", "-"),
                &Self::NoExclusiveOr => format!("{}", "or"),
                &Self::Plus => format!("{}", "+"),
            }
        }
        //refer slient rule binary_operation
        pub fn from_rule(op: &Pair<Rule>) -> Self {
            match op.as_rule() {
                Rule::power => BinaryOp::Power,
                Rule::divided_by => BinaryOp::Divide,
                Rule::modulo => BinaryOp::Modulo,
                Rule::times => BinaryOp::Times,
                Rule::and => BinaryOp::And,
                Rule::exclusive_or => BinaryOp::ExclusiveOr,
                Rule::minus => BinaryOp::Minus,
                Rule::non_exclusive_or => BinaryOp::NoExclusiveOr,
                Rule::plus => BinaryOp::Plus,
                rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
            }
        }

        pub fn eval(&self, lhs: f32, rhs: f32) -> f32 {
            match &self {
                &Self::Power => lhs.powf(rhs),
                &Self::Divide => lhs / rhs,
                &Self::Modulo => {
                    let t = ((lhs % rhs) + rhs) % rhs;

                    if t < 0.0 {
                        /* always calculates a positive answer */
                        t + rhs.abs()
                    } else {
                        t
                    }
                }
                &Self::Times => lhs * rhs,
                &Self::And => {
                    if lhs == 0.0 || rhs == 0.0 {
                        0.0
                    } else {
                        1.0
                    }
                }
                &Self::ExclusiveOr => {
                    if (lhs == 0.0 && rhs != 0.0) || (lhs != 0.0 && rhs == 0.0) {
                        1.0
                    } else {
                        0.0
                    }
                }
                &Self::Minus => lhs - rhs,
                &Self::NoExclusiveOr => {
                    if lhs != 0.0 || rhs != 0.0 {
                        1.0
                    } else {
                        0.0
                    }
                }
                &Self::Plus => lhs + rhs,
            }
        }
    }

    #[derive(Debug, PartialEq)]
    enum Unary {
        Cos(Box<Expr>),
        Sin(Box<Expr>),
        Tan(Box<Expr>),
        Abs(Box<Expr>),
        Acos(Box<Expr>),
        Asin(Box<Expr>),
        Exp(Box<Expr>),
        Fix(Box<Expr>),
        Fup(Box<Expr>),
        Ln(Box<Expr>),
        Round(Box<Expr>),
        Sqrt(Box<Expr>),
        Atan { lhs: Box<Expr>, rhs: Box<Expr> },
    }
    impl Unary {
        #[cfg(any(feature = "std", test))]
        pub fn dis(&self) -> String {
            match &self {
                &Self::Abs(x) => format!("abs{}", x.dis()),
                &Self::Acos(x) => format!("acos{}", x.dis()),
                &Self::Asin(x) => format!("asin{}", x.dis()),
                &Self::Cos(x) => format!("cos{}", x.dis()),
                &Self::Exp(x) => format!("exp{}", x.dis()),
                &Self::Fix(x) => format!("fix{}", x.dis()),
                &Self::Fup(x) => format!("fup{}", x.dis()),
                &Self::Ln(x) => format!("ln{}", x.dis()),
                &Self::Round(x) => format!("round{}", x.dis()),
                &Self::Sin(x) => format!("Sin{}", x.dis()),
                &Self::Sqrt(x) => format!("sqrt{}", x.dis()),
                &Self::Tan(x) => format!("tan{}", x.dis()),
                &Self::Atan { lhs, rhs } => format!("atan {}/{}", lhs.dis(), rhs.dis()),
            }
        }
        // for ordinary_unary_combo and arc_tangent_combo rule
        pub fn from_rule(pair: Pair<Rule>) -> Self {
            match pair.as_rule() {
                Rule::ordinary_unary_combo => {
                    let mut x = pair.into_inner();
                    let o = x.next().unwrap();
                    let rhs = x.next().unwrap();
                    let rhs = Expr::from_rule(rhs);
                    match o.as_rule() {
                        Rule::absolute_value => Self::Abs(Box::new(rhs)),
                        Rule::arc_cosine => Self::Acos(Box::new(rhs)),
                        Rule::arc_sine => Self::Asin(Box::new(rhs)),
                        Rule::cosine => Self::Cos(Box::new(rhs)),
                        Rule::e_raised_to => Self::Exp(Box::new(rhs)),
                        Rule::fix_down => Self::Fix(Box::new(rhs)),
                        Rule::fix_up => Self::Fup(Box::new(rhs)),
                        Rule::natural_log_of => Self::Ln(Box::new(rhs)),
                        Rule::round => Self::Round(Box::new(rhs)),
                        Rule::sine => Self::Sin(Box::new(rhs)),
                        Rule::square_root => Self::Sqrt(Box::new(rhs)),
                        Rule::tangent => Self::Tan(Box::new(rhs)),
                        _ => unreachable!(),
                    }
                }
                Rule::arc_tangent_combo => {
                    let mut x = pair.into_inner();
                    let lhs = x.next().unwrap();
                    let rhs = x.next().unwrap();

                    Self::Atan {
                        lhs: Box::new(Expr::from_rule(lhs)),
                        rhs: Box::new(Expr::from_rule(rhs)),
                    }
                }
                _ => {
                    unreachable!()
                }
            }
        }
        /// Arguments to unary operations which take angle measures (COS, SIN, and TAN) are in degrees.
        /// Values returned by unary operations which return angle measures (ACOS, ASIN, and ATAN) are also in degrees.
        pub fn eval(&self) -> Option<f32> {
            match &self {
                &Self::Abs(x) => x.eval().and_then(|y| Some(y.abs())),
                &Self::Acos(x) => x.eval().and_then(|y| Some(y.acos().to_degrees())),
                &Self::Asin(x) => x.eval().and_then(|y| Some(y.asin().to_degrees())),
                &Self::Cos(x) => x.eval().and_then(|y| Some(y.to_radians().cos())),
                &Self::Exp(x) => x.eval().and_then(|y| Some(y.exp())),
                &Self::Fix(x) => x.eval().and_then(|y| Some(y.floor())),
                &Self::Fup(x) => x.eval().and_then(|y| Some(y.ceil())),
                &Self::Ln(x) => x.eval().and_then(|y| Some(y.ln())),
                &Self::Round(x) => x.eval().and_then(|y| Some(y.round())),
                &Self::Sin(x) => x.eval().and_then(|y| Some(y.to_radians().sin())),
                &Self::Sqrt(x) => x.eval().and_then(|y| Some(y.sqrt())),
                &Self::Tan(x) => x.eval().and_then(|y| Some(y.to_radians().tan())),
                &Self::Atan { lhs, rhs } => {
                    if let (Some(lhs), Some(rhs)) = (lhs.eval(), rhs.eval()) {
                        Some(lhs.atan2(rhs).to_degrees())
                    } else {
                        None
                    }
                }
            }
        }
    }

    #[allow(private_interfaces)]
    #[derive(Debug, PartialEq)]
    pub enum RealValue {
        RealNumber(f32),
        Expression(Box<Expr>),
        ParameterValue(Box<RealValue>),
        UnaryOp(Unary),
    }

    impl RealValue {
        #[cfg(any(feature = "std", test))]
        pub fn dis(&self) -> String {
            match &self {
                &Self::RealNumber(x) => format!("{}", x),
                &Self::ParameterValue(x) => format!("#{}", x.dis()),
                &Self::UnaryOp(x) => format!("{}", x.dis()),
                &Self::Expression(x) => format!("({})", x.dis()),
            }
        }
        pub fn eval(&self) -> Option<f32> {
            match &self {
                &Self::RealNumber(x) => Some(*x),
                &Self::ParameterValue(x) => x
                    .eval()
                    .and_then(|y| call_get_parameter_weak_function(y as u32)),
                &Self::UnaryOp(x) => x.eval(),
                &Self::Expression(x) => x.eval(),
            }
        }
        /// real_value = { real_number | expression | parameter_value | unary_combo }
        pub fn from_rule(pair: Pair<Rule>) -> Self {
            // debug_assert!(pair.as_rule(),Rule::real_value);

            let pair = pair.into_inner().next().unwrap();
            match pair.as_rule() {
                Rule::real_number => Self::RealNumber(pair.as_str().trim().parse::<f32>().unwrap()),
                Rule::parameter_value => Self::ParameterValue(Box::new(Self::from_rule(
                    pair.into_inner().next().unwrap(),
                ))),
                Rule::expression => Self::Expression(Box::new(Expr::from_rule(pair))),
                Rule::ordinary_unary_combo | Rule::arc_tangent_combo => {
                    Self::UnaryOp(Unary::from_rule(pair))
                }
                // Rule::real_value =>
                _ => unreachable!("expected atom, found {:?}", pair),
            }
        }

        #[cfg(any(feature = "std", test))]
        pub fn eval_real_value(str: &str) -> Option<f32> {
            if let Ok(mut op) = Rs274ngcParser::parse(Rule::real_value, str) {
                op.next().and_then(|y| RealValue::from_rule(y).eval())
            } else {
                None
            }
        }
    }

    #[allow(dead_code)]
    #[derive(Debug, PartialEq)]
    pub enum Expr {
        Value(Box<RealValue>),
        BinOp {
            lhs: Box<Expr>,
            op: BinaryOp,
            rhs: Box<Expr>,
        },
    }
    impl Expr {
        #[cfg(any(feature = "std", test))]
        pub fn dis(&self) -> String {
            match &self {
                &Self::Value(x) => format!("({})", x.dis()),
                &Self::BinOp { lhs, op, rhs } => {
                    format!("({}{}{})", lhs.dis(), op.dis(), rhs.dis())
                }
            }
        }

        /// parse expression = { "[" ~ real_value ~ (binary_operation ~ real_value)* ~ "]"}
        pub fn from_rule(pair: Pair<Rule>) -> Self {
            let pairs = pair.into_inner();

            PRATT_PARSER
                .map_primary(|primary| match primary.as_rule() {
                    Rule::real_value => {
                        let rv = RealValue::from_rule(primary);
                        Self::Value(Box::new(rv))
                    }
                    _ => unreachable!("Expr::parse expected atom, found {:?}", primary),
                })
                .map_infix(|lhs, op, rhs| {
                    let op = BinaryOp::from_rule(&op);
                    Self::BinOp {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    }
                })
                // .map_prefix(|op, rhs| match op.as_rule() {
                //     Rule::parameter_sign => Expr::UnaryMinus(Box::new(rhs)),
                //     _ => unreachable!(),
                // })
                .parse(pairs)
        }

        pub fn eval(&self) -> Option<f32> {
            match &self {
                &Self::Value(x) => x.eval(),
                &Self::BinOp { lhs, op, rhs } => {
                    if let (Some(lhs), Some(rhs)) = (lhs.eval(), rhs.eval()) {
                        Some(op.eval(lhs, rhs))
                    } else {
                        None
                    }
                }
            }
        }

        #[cfg(any(feature = "std", test))]
        pub fn eval_expression(str: &str) -> Option<f32> {
            if let Ok(mut op) = Rs274ngcParser::parse(Rule::expression, str) {
                op.next().and_then(|y| Expr::from_rule(y).eval())
            } else {
                None
            }
        }
    }

    #[test]
    fn test_real_value() {
        let e1 = RealValue::from_rule(
            Rs274ngcParser::parse(Rule::real_value, "#2")
                .unwrap()
                .next()
                .unwrap(),
        );
        // real_value = { real_number | expression | parameter_value | unary_combo }

        println!("{:?}", e1);
        // assert_eq!(
        //     e1,
        //     Expr::BinOp {
        //         lhs: Box::new(Expr::Value(Box::new(RealValue::RealNumber(2.0)))),
        //         op: BinaryOp::Plus,
        //         rhs: Box::new(Expr::Value(Box::new(RealValue::RealNumber(3.0))))
        //     }
        // );
    }

    #[test]
    fn test_expression_eval() {
        assert_eq!(Expr::eval_expression("[FIX[2.8]]"), Some(2.));
        assert_eq!(
            Expr::eval_expression("[sin[90]]"),
            Some(90.0_f32.to_radians().sin())
        );
        assert_eq!(
            Expr::eval_expression("[cos[45]]"),
            Some(45.0_f32.to_radians().cos())
        );
        assert_eq!(
            Expr::eval_expression("[atan[2]/[12]]"),
            Some(2.0_f32.atan2(12.0_f32).to_degrees())
        );
        assert_eq!(
            Expr::eval_expression("[1+atan[2]/[12]*cos[1+2]]"),
            Some(1.0 + 2.0_f32.atan2(12.0_f32).to_degrees() * 3.0_f32.to_radians().cos())
        );
    }

    #[test]
    fn test_expression_parse() {
        let e1 = Expr::from_rule(
            Rs274ngcParser::parse(Rule::expression, "[2+3]")
                .unwrap()
                .next()
                .unwrap(),
        );
        assert_eq!(
            e1,
            Expr::BinOp {
                lhs: Box::new(Expr::Value(Box::new(RealValue::RealNumber(2.0)))),
                op: BinaryOp::Plus,
                rhs: Box::new(Expr::Value(Box::new(RealValue::RealNumber(3.0))))
            }
        );
        assert_eq!(e1.eval(), Some(5.));

        let e2 = Expr::from_rule(
            Rs274ngcParser::parse(Rule::expression, "[1.0+atan[2]/[12]*cos[1+2]]")
                .unwrap()
                .next()
                .unwrap(),
        );
        assert_eq!(
            e2,
            Expr::BinOp {
                lhs: Box::new(Expr::Value(Box::new(RealValue::RealNumber(1.0)))),
                op: BinaryOp::Plus,
                rhs: Box::new(Expr::BinOp {
                    lhs: Box::new(Expr::Value(Box::new(RealValue::UnaryOp(Unary::Atan {
                        lhs: Box::new(Expr::Value(Box::new(RealValue::RealNumber(2.0)))),
                        rhs: Box::new(Expr::Value(Box::new(RealValue::RealNumber(12.0)))),
                    })))),
                    op: BinaryOp::Times,
                    rhs: Box::new(Expr::Value(Box::new(RealValue::UnaryOp(Unary::Cos(
                        Box::new(Expr::BinOp {
                            lhs: Box::new(Expr::Value(Box::new(RealValue::RealNumber(1.0)))),
                            op: BinaryOp::Plus,
                            rhs: Box::new(Expr::Value(Box::new(RealValue::RealNumber(2.0)))),
                        })
                    ))))),
                }),
            }
        );

        let e3 = Expr::from_rule(
            Rs274ngcParser::parse(Rule::expression, "[cos[1+2]]")
                .unwrap()
                .next()
                .unwrap(),
        );
        assert_eq!(e3.eval(), Some(3.0_f32.to_radians().cos()));

        assert_eq!(
            Expr::from_rule(
                Rs274ngcParser::parse(Rule::expression, "[1.0+atan[2]/[12]*cos[1+2]]")
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .dis(),
            "((1)+((atan (2)/(12))*(cos((1)+(2)))))"
        );
    }
}

/// for "line ={ block_delete? ~  line_number?  ~ segment* ~ end_of_line}" rule
mod line {
    use std::string::ToString;

    use crate::{BTreeMap, DistanceMode, GCodes, GGroup, MCodes, MGroup, String};
    type GModalMap = BTreeMap<GGroup, GCodes>;
    type MModalMap = BTreeMap<MGroup, MCodes>;
    use super::{RealValue, Rs274Error, Rs274ngcParser, Rule};
    use pest::{iterators::Pair, Parser};

    // #[derive(Debug, PartialEq)]
    // pub struct ParameterSet {
    //     par_index: Box<Expr>,
    //     val: Box<Expr>,
    // }
    #[allow(dead_code)]
    #[doc(hidden)]
    static mut SET_PARAMETER_WEAK_FUNCTION: Option<fn(key: u32, val: f32)> = None;

    #[allow(dead_code)]
    fn default_set_parameter_weak(key: u32, val: f32) {
        println!("do parameter setting: #{:?} = {:?} ", key, val);

        // unimplemented!();
    }

    /// for Parameter Setting in rs274ngc, use it to set the function that query the parameter settings
    #[allow(dead_code)]
    pub fn set_set_parameter_function(func: fn(key: u32, val: f32)) {
        unsafe {
            SET_PARAMETER_WEAK_FUNCTION = Some(func);
        }
    }

    #[allow(dead_code)]
    fn call_set_parameter_weak_function(key: u32, val: f32) {
        unsafe {
            match SET_PARAMETER_WEAK_FUNCTION {
                Some(func) => func(key, val),
                None => default_set_parameter_weak(key, val),
            }
        }
    }

    #[derive(Debug)]
    pub struct Block {
        pub a_number: Option<f32>,
        pub b_number: Option<f32>,
        pub c_number: Option<f32>,
        pub d_number: Option<i32>,
        pub f_number: Option<f32>,
        pub h_number: Option<i32>,

        pub i_number: Option<f32>,
        pub j_number: Option<f32>,
        pub k_number: Option<f32>,

        pub l_number: Option<i32>,
        pub n_number: Option<u32>,
        pub p_number: Option<f32>,
        q_number: Option<f32>,
        pub r_number: Option<f32>,
        pub s_number: Option<f32>,
        pub t_number: Option<i32>,
        pub x_number: Option<f32>,
        pub y_number: Option<f32>,
        pub z_number: Option<f32>,

        comment: Option<String>,
        /// g_modes array in the block keeps track of which G modal groups are used on
        /// a line of code
        pub g_modes: GModalMap,
        /// track of which M Modal groups
        pub m_modes: MModalMap,
        pub motion_to_be: Option<GCodes>,
    }

    impl Block {
        pub fn new() -> Self {
            Self {
                a_number: None,
                b_number: None,
                c_number: None,
                d_number: None,
                f_number: None,
                h_number: None,
                i_number: None,
                j_number: None,
                k_number: None,
                l_number: None,
                n_number: None,
                p_number: None,
                q_number: None,
                r_number: None,
                s_number: None,
                t_number: None,
                x_number: None,
                y_number: None,
                z_number: None,

                comment: None,
                motion_to_be: None,
                g_modes: GModalMap::new(),
                m_modes: MModalMap::new(),
            }
        }

        #[cfg(any(feature = "std", test))]
        pub fn dis(&self) -> String {
            use std::string::ToString;

            let mut ret = "".to_string();

            if let Some(v) = self.n_number {
                ret.push_str(format!("N{:?} ", v).as_str());
            }
            if let Some(v) = self.a_number {
                ret.push_str(format!("A{:?} ", v).as_str());
            }

            if let Some(v) = self.b_number {
                ret.push_str(format!("B{:?} ", v).as_str());
            }
            if let Some(v) = self.c_number {
                ret.push_str(format!("C{:?} ", v).as_str());
            }
            if let Some(v) = self.d_number {
                ret.push_str(format!("D{:?} ", v).as_str());
            }

            if let Some(v) = self.f_number {
                ret.push_str(format!("F{:?} ", v).as_str());
            }
            if let Some(v) = self.h_number {
                ret.push_str(format!("H{:?} ", v).as_str());
            }
            if let Some(v) = self.i_number {
                ret.push_str(format!("I{:?} ", v).as_str());
            }

            if let Some(v) = self.j_number {
                ret.push_str(format!("J{:?} ", v).as_str());
            }
            if let Some(v) = self.k_number {
                ret.push_str(format!("K{:?} ", v).as_str());
            }
            if let Some(v) = self.l_number {
                ret.push_str(format!("L{:?} ", v).as_str());
            }

            if let Some(v) = self.p_number {
                ret.push_str(format!("P{:?} ", v).as_str());
            }
            if let Some(v) = self.q_number {
                ret.push_str(format!("Q{:?} ", v).as_str());
            }
            if let Some(v) = self.r_number {
                ret.push_str(format!("R{:?} ", v).as_str());
            }

            if let Some(v) = self.t_number {
                ret.push_str(format!("T{:?} ", v).as_str());
            }
            if let Some(v) = self.s_number {
                ret.push_str(format!("S{:?} ", v).as_str());
            }
            if let Some(v) = self.x_number {
                ret.push_str(format!("X{:?} ", v).as_str());
            }
            if let Some(v) = self.y_number {
                ret.push_str(format!("Y{:?} ", v).as_str());
            }
            if let Some(v) = self.z_number {
                ret.push_str(format!("Z{:?} ", v).as_str());
            }
            if let Some(v) = self.motion_to_be {
                ret.push_str(format!("motion_to_be{:?} ", v).as_str());
            }

            if self.g_modes.len() > 0 {
                ret.push_str(format!("g_modes:{:?} ", self.g_modes).as_str());
            }
            if self.m_modes.len() > 0 {
                ret.push_str(format!("m_modes:{:?} ", self.m_modes).as_str());
            }
            if let Some(v) = &self.comment {
                ret.push_str(format!("comment:{:?} ", v).as_str());
            }

            ret
        }

        /// parse one statement to fullfill self's content.
        ///
        /// notice: only when it reutrn Ok, the fields value is valid.
        pub fn parse(
            blocktext: &str,

            last_saved_motion: &Option<GCodes>,
            last_saved_distance_mode: &DistanceMode,
        ) -> Result<Self, Rs274Error> {
            if blocktext.len() > 0 {
                let pair = Rs274ngcParser::parse(Rule::line, blocktext)
                    .expect("unsuccessful parse")
                    .next()
                    .unwrap();

                //parse line
                let mut block = Self::from_rule(pair)?;

                // if let Err(t) = block.enhance_block(last_saved_motion) {
                //     return Err(t);
                // }
                // if let Err(t) = block.check_items(last_saved_distance_mode) {
                //     return Err(t);
                // }
                return Ok(block);
            }
            Err(Rs274Error::NceSscanfFailed)
        }

        /// for "line ={ block_delete? ~  line_number?  ~ segment* ~ end_of_line}" rule
        pub fn from_rule(pair: Pair<Rule>) -> Result<Self, Rs274Error> {
            // assert_eq!(pair.as_rule(),Rule::line);

            let mut block = Self::new();
            for field in pair.into_inner() {
                match field.as_rule() {
                    Rule::block_delete => return Ok(block),
                    Rule::line_number => {
                        let mut inner_rules = field.into_inner(); // { "n" ~ number_for_line }
                        let value: &str = inner_rules.next().unwrap().as_str().trim();
                        let val = value.parse::<u32>().unwrap();
                        if val > 99999 {
                            return Err(Rs274Error::NceLineNumberGreaterThan99999);
                        }
                        block.n_number.replace(val);
                    }
                    Rule::segment => {
                        let mut inner_rules = field.into_inner(); // { mid_line_word | comment | parameter_setting }
                        let v = inner_rules.next().unwrap();

                        match v.as_rule() {
                            Rule::mid_line_word => {
                                let mut inner_rules = v.into_inner(); // { mid_line_letter ~ real_value }
                                let mid_l_l =
                                    inner_rules.next().unwrap().into_inner().next().unwrap();
                                let real_val = RealValue::from_rule(inner_rules.next().unwrap())
                                    .eval()
                                    .unwrap();

                                match mid_l_l.as_rule() {
                                    Rule::letter_j => block.j_number = Some(real_val),
                                    Rule::letter_z => block.z_number = Some(real_val),
                                    Rule::letter_s => block.s_number = Some(real_val),
                                    Rule::letter_t => block.t_number = Some(real_val as i32),
                                    Rule::letter_x => block.x_number = Some(real_val),
                                    Rule::letter_y => block.y_number = Some(real_val),
                                    Rule::letter_r => block.r_number = Some(real_val),
                                    Rule::letter_a => block.a_number = Some(real_val),
                                    Rule::letter_i => block.i_number = Some(real_val),
                                    Rule::letter_k => block.k_number = Some(real_val),
                                    Rule::letter_b => block.b_number = Some(real_val),
                                    Rule::letter_c => block.c_number = Some(real_val),
                                    Rule::letter_f => {
                                        if real_val <= 0. {
                                            return Err(Rs274Error::NceNegativeFWordUsed);
                                        }
                                        block.f_number = Some(real_val);
                                    }
                                    Rule::letter_d => {
                                        let val = real_val as i32;
                                        if val <= 0 {
                                            return Err(Rs274Error::NceNegativeFWordUsed);
                                        }
                                        block.d_number.replace(val);
                                    }
                                    Rule::letter_h => {
                                        let val = real_val as i32;
                                        if val <= 0 {
                                            return Err(
                                            Rs274Error::NceNegativeHWordToolLengthOffsetIndexUsed,
                                        );
                                        }
                                        block.h_number.replace(val);
                                    }
                                    Rule::letter_l => {
                                        let val = real_val as i32;
                                        if val <= 0 {
                                            return Err(Rs274Error::NceNegativeLWordUsed);
                                        }
                                        block.l_number.replace(val);
                                    }
                                    Rule::letter_p => {
                                        if real_val <= 0.0 {
                                            return Err(Rs274Error::NceNegativePWordUsed);
                                        }
                                        block.p_number.replace(real_val);
                                    }
                                    Rule::letter_q => {
                                        if real_val <= 0.0 {
                                            return Err(Rs274Error::NceNegativeOrZeroQValueUsed);
                                        }
                                        block.q_number.replace(real_val);
                                    }

                                    Rule::letter_g => {
                                        let value_read = real_val * 10.0;
                                        let mut value = value_read.floor() as i32;
                                        let diff = value_read - (value as f32);

                                        if diff > 0.999 {
                                            value = value_read.ceil() as i32;
                                        } else if diff > 0.001 {
                                            return Err(Rs274Error::NceGCODEOutOfRange);
                                        }

                                        if value > 999 || value < 0 {
                                            return Err(Rs274Error::NceGCODEOutOfRange);
                                        }

                                        let v: Result<GCodes, _> = value.try_into();
                                        match v {
                                            Ok(code) => {
                                                let mode = code.to_modal();
                                                block.g_modes.insert(mode, code);
                                            }
                                            Err(_) => return Err(Rs274Error::NceGCODEOutOfRange),
                                        }
                                    }
                                    Rule::letter_m => {
                                        let val = real_val as i32;
                                        if val < 0 {
                                            return Err(Rs274Error::NceNegativeMCodeUsed);
                                        } else if val > 99 {
                                            return Err(Rs274Error::NceMCodeGreaterThan99);
                                        }

                                        let v: Result<MCodes, _> = val.try_into();
                                        match v {
                                            Ok(code) => {
                                                let mode = code.to_modal();
                                                if let Some(_) = block.m_modes.get(&mode) {
                                                    return Err(
                                                    Rs274Error::NceTwoMCodesUsedFromSameModalGroup,
                                                );
                                                }

                                                block.m_modes.insert(mode, code.clone());
                                            }
                                            Err(_) => return Err(Rs274Error::NceMCODEOutOfRange),
                                        }
                                    }
                                    rule => unreachable!("found {:?}", rule),
                                }
                            }
                            Rule::comment => {
                                let cmt = v.as_str().trim();
                                if let Some(x) = &mut block.comment {
                                    x.extend(cmt.chars());
                                } else {
                                    block.comment.replace(cmt.to_string());
                                }
                            }
                            Rule::parameter_setting => {
                                //parameter_setting = { "#" ~ parameter_index ~ "=" ~ real_value }
                                let mut inner_rules = v.into_inner();
                                let index = inner_rules.next().unwrap();
                                let real = inner_rules.next().unwrap();

                                let index_val = RealValue::from_rule(index).eval().unwrap() as u32;
                                let real_val = RealValue::from_rule(real).eval().unwrap();

                                call_set_parameter_weak_function(index_val, real_val);
                            }
                            _ => unreachable!(),
                        }
                    }
                    Rule::seimcolon_comment => {
                        let cmt = field.as_str().trim();
                        if let Some(x) = &mut block.comment {
                            x.extend(cmt.chars());
                        } else {
                            block.comment.replace(cmt.to_string());
                        }
                    }
                    _ => {}
                }
            }

            Ok(block)
        }

        /// do basic checks and assign motion_to_be field, include:
        /// 1. A g80 is in the block, no modal group 0 code that uses axes
        ///    is in the block, and one or more axis values is given:
        ///    NceCannotUseAxisValuesWithG80
        /// 2. A g92 is in the block and no axis value is given:
        ///    NceAllAxesMissingWithG92
        /// 3. One g-code from group 1 and one from group 0, both of which can use
        ///    axis values, are in the block:
        ///    NCE_CANNOT_USE_TWO_G_CODES_THAT_BOTH_USE_AXIS_VALUES
        /// 4. A g-code from group 1 which can use axis values is in the block,
        ///    but no axis value is given: NCE_ALL_AXES_MISSING_WITH_MOTION_CODE
        /// 5. Axis values are given, but there is neither a g-code in the block
        ///    nor an active previously given modal g-code that uses axis values:
        ///    NCE_CANNOT_USE_AXIS_VALUES_WITHOUT_A_G_CODE_THAT_USES_THEM
        ///
        ///  If there is a g-code for motion in the block (in g_modes\[GCodeMotion\]),
        ///  set motion_to_be to that. Otherwise, if there is an axis value in the
        ///  block and no g-code to use it (any such would be from group 0 in
        ///  g_modes\[GCodeMisc\]), should set motion_to_be as the last motion saved(input motion_mode).
        ///
        fn enhance_block(&mut self, motion_mode: &Option<GCodes>) -> Result<(), Rs274Error> {
            /* pointer to machine settings       */

            let axis_flag: bool = self.x_number.is_some()
                || self.y_number.is_some()
                || self.z_number.is_some()
                || self.a_number.is_some()
                || self.b_number.is_some()
                || self.c_number.is_some();
            let misc_code = self.g_modes.get(&GGroup::GCodeMisc);
            let motion_code = self.g_modes.get(&GGroup::GCodeMotion);

            let mut mode_zero_covets_axes: bool = false;

            if matches!(
                misc_code,
                Some(&GCodes::G10) | Some(&GCodes::G28) | Some(&GCodes::G30) | Some(&GCodes::G92)
            ) {
                mode_zero_covets_axes = true;
            }
            ///////////////////////////////////////////
            if motion_code.is_none() {
                if mode_zero_covets_axes {
                    /* other 3 can get by without axes but not G92
                     */
                    if !axis_flag && misc_code == Some(&GCodes::G92) {
                        return Err(Rs274Error::NceAllAxesMissingWithG92);
                    }
                } else if axis_flag {
                    if motion_mode.is_none() || motion_mode == &Some(GCodes::G80) {
                        return Err(Rs274Error::NceCannotUseAxisValuesWithoutAGcodeThatUsesThem);
                    }

                    self.motion_to_be = motion_mode.clone();
                }
                return Ok(());
            }
            ///////////////////////////////////////////
            if motion_code == Some(&GCodes::G80) {
                if axis_flag && !mode_zero_covets_axes {
                    return Err(Rs274Error::NceCannotUseAxisValuesWithG80);
                }

                if !axis_flag && misc_code == Some(&GCodes::G92) {
                    return Err(Rs274Error::NceAllAxesMissingWithG92);
                }
            } else {
                if mode_zero_covets_axes {
                    return Err(Rs274Error::NceCannotUseTwoGCodethatBothUseAxisValues);
                }

                if !axis_flag {
                    return Err(Rs274Error::NceAllAxesMissingWithMotionCode);
                }
            }

            self.motion_to_be = Some(motion_code.unwrap().clone());

            return Ok(());
        }

        fn check_items(&mut self, distance_mode: &DistanceMode) -> Result<(), Rs274Error> {
            self._check_g_codes(distance_mode)?;
            self._check_m_codes()?;
            self._check_other_codes()
        }

        fn _check_m_codes(&mut self) -> Result<(), Rs274Error> {
            // max number of m codes on one line
            const MAX_EMS: usize = 4;

            if self.m_modes.len() > MAX_EMS {
                return Err(Rs274Error::NceTooManyMCodesOnLine);
            }
            Ok(())
        }
        fn _check_g_codes(&self, distance_mode: &DistanceMode) -> Result<(), Rs274Error> {
            match self.g_modes.get(&GGroup::GCodeMisc) {
                Some(&GCodes::G4) => {
                    if self.p_number.is_none() {
                        return Err(Rs274Error::NceDwellTimeMissingWithG4);
                    }
                }
                Some(&GCodes::G10) => {
                    if let Some(p) = self.p_number {
                        let p_int = (p + 0.0001) as i32;
                        if (p + 0.0001) - p_int as f32 > 0.0002 {
                            return Err(Rs274Error::NcePValueNotAnIntegerWithG10L2);
                        }
                        if p_int < 1 || p_int > 9 {
                            return Err(Rs274Error::NcePValueOutOfRangeWithG10L2);
                        }
                    }
                    if self.l_number != Some(2) {
                        return Err(Rs274Error::NceLineWithG10DoesNotHavel2);
                    };
                }
                Some(&GCodes::G28) | Some(&GCodes::G30) | Some(&GCodes::G92)
                | Some(&GCodes::G92_1) | Some(&GCodes::G92_2) | Some(&GCodes::G92_3) => {}
                Some(&GCodes::G53) => {
                    if self.motion_to_be != Some(GCodes::G0)
                        && self.motion_to_be != Some(GCodes::G1)
                    {
                        return Err(Rs274Error::NceMustUseG0OrG1WithG53);
                    }

                    let dis_code = self.g_modes.get(&GGroup::GCodeDistance);
                    if dis_code == Some(&GCodes::G91)
                        || (dis_code == Some(&GCodes::G90)
                            && distance_mode == &DistanceMode::Incremental)
                    {
                        return Err(Rs274Error::NceCannotUseG53Incremental);
                    }
                }
                None => return Ok(()),
                _ => {
                    return Err(Rs274Error::NceBugBadGCodeModalGroup0);
                }
            }

            Ok(())
        }
        fn _check_other_codes(&self) -> Result<(), Rs274Error> {
            if self.a_number.is_some() || self.b_number.is_some() || self.c_number.is_some() {
                if let Some(gcode) = self.g_modes.get(&GGroup::GCodeMotion) {
                    if gcode > &GCodes::G80 && gcode < &GCodes::G90 {
                        return Err(Rs274Error::NceCannotPutAnAInCannedCycle);
                    }
                }
            }

            if self.d_number.is_some() {
                if let Some(gcode) = self.g_modes.get(&GGroup::GCodeCutterRadiusCompensation) {
                    if gcode != &GCodes::G41 && gcode != &GCodes::G42 {
                        return Err(Rs274Error::NceDWordWithNoG41OrG42);
                    }
                }
            }

            if self.h_number.is_some() {
                if let Some(gcode) = self.g_modes.get(&GGroup::GCodeToolLengthOffset) {
                    if gcode != &GCodes::G43 {
                        return Err(Rs274Error::NceHWordWithNoG43);
                    }
                }
            }

            /* could still be useless if yz_plane arc */
            if self.i_number.is_some() {
                if self.motion_to_be != Some(GCodes::G2)
                    && self.motion_to_be != Some(GCodes::G3)
                    && self.motion_to_be != Some(GCodes::G87)
                {
                    return Err(Rs274Error::NceIWordWithNoG2OrG3OrG87ToUseIT);
                }
            }

            /* could still be useless if xz_plane arc */
            if self.j_number.is_some() {
                if self.motion_to_be != Some(GCodes::G2)
                    && self.motion_to_be != Some(GCodes::G3)
                    && self.motion_to_be != Some(GCodes::G87)
                {
                    return Err(Rs274Error::NceJWordWithNoG2OrG3OrG87ToUseIT);
                }
            }

            /* could still be useless if xy_plane arc */
            if self.k_number.is_some() {
                if self.motion_to_be != Some(GCodes::G2)
                    && self.motion_to_be != Some(GCodes::G3)
                    && self.motion_to_be != Some(GCodes::G87)
                {
                    return Err(Rs274Error::NceKWordWithNoG2OrG3OrG87ToUseIT);
                }
            }

            if self.l_number.is_some() {
                if let Some(motion) = self.motion_to_be {
                    if let Some(gcode) = self.g_modes.get(&GGroup::GCodeMisc) {
                        if gcode != &GCodes::G10 && (motion < GCodes::G81 || motion > GCodes::G89) {
                            return Err(Rs274Error::NceLWordWithNoCannedCycleOrG10);
                        }
                    }
                }
            }

            if self.p_number.is_some() {
                if let Some(motion) = self.motion_to_be {
                    if let Some(gcode) = self.g_modes.get(&GGroup::GCodeMisc) {
                        if gcode != &GCodes::G10
                            && gcode != &GCodes::G4
                            && motion != GCodes::G86
                            && motion != GCodes::G88
                            && motion != GCodes::G89
                        {
                            return Err(Rs274Error::NcePWordwithNoG4G10G82G86G88G89);
                        }
                    }
                }
            }

            if self.q_number.is_some() {
                if let Some(motion) = self.motion_to_be {
                    if motion != GCodes::G83 {
                        return Err(Rs274Error::NceQWordWithNoG83);
                    }
                }
            }

            if self.r_number.is_some() {
                if let Some(motion) = self.motion_to_be {
                    if (motion != GCodes::G2 && motion != GCodes::G3)
                        && (motion == GCodes::G82
                            || motion == GCodes::G83
                            || motion == GCodes::G84
                            || motion == GCodes::G85
                            || motion == GCodes::G86
                            || motion == GCodes::G87
                            || motion == GCodes::G88)
                    {
                        return Err(Rs274Error::NceRwordWithNoGCodeThatUseIT);
                    }
                }
            }
            Ok(())
        }
    }
}

pub fn parse_lines(lines: &str) -> Result<Vec<Block>, Rs274Error> {
    let mut ret = Vec::<Block>::new();
    for l in lines.lines() {
        let mut x = Rs274ngcParser::parse(Rule::line, l).map_err(Rs274Error::from)?;

        if let Some(pair) = x.next() {
            let block = Block::from_rule(pair)?;
            ret.push(block);
        }
        // println!("{:?}", block.unwrap().dis());
    }
    Ok(ret)
}

#[test]
fn test_parse_lines() {
    let lines = r#"
        ; n0 semicolon comment
        n1 O1000
        n2 T1 M6
        n3 (Linear / Feed - Absolute)
        n4 G0 G90 G40 G21 G17 G94 G80
        n5 G54 ;X-75 Y-75 S500 M3  (Position 6)
        n6 G43 Z100 H1
        n7 G01 Z5
        n8 G01 Z-20 F100
        n9 G01 X-40                   (Position 1)
        n10 G01 Y40 M8                 (Position 2)
        n11 G01 X40                    (Position 3)
        n12 G01 Y-40                   (Position 4)
        n13 G01 X-75                   (Position 5)
        n14 G01 Y-75                   (Position 6)
        n15 G0 Z100
        n16 M30        
    "#;

    for l in lines.lines() {
        let pair = Rs274ngcParser::parse(Rule::line, l)
            .expect("unsuccessful parse")
            .next()
            .unwrap();
        let block = Block::from_rule(pair);
        println!("{:?}", block.unwrap().dis());
    }
}

pub fn parse_line(str: &str) -> Result<Block, Rs274Error> {
    let pair = Rs274ngcParser::parse(Rule::line, str)
        .expect("unsuccessful parse")
        .next()
        .unwrap();

    let block = Block::from_rule(pair);

    block
}

#[test]
fn test_line_parse() {
    let input ="n111 g38.2 #5226=14 #5222=3 #5221=1 X[#5226 - #5221] Y#5222 (move above nominal hole center)";
    let last_saved_motion = Some(GCodes::G80);
    let last_saved_distance_mode = crate::DistanceMode::Absolute;

    // let input = "n12 g92.1 a3 B3 #[cos[1.2]]=1";
    println!(
        "{} parse result: block:{:#?}",
        input,
        Block::parse(input, &last_saved_motion, &last_saved_distance_mode)
    );
}

// pub fn get_pairs_from_line(input: &str) -> Vec<Pair<Rule>> {
//     let o = Rs274ngcParser::parse(Rule::line, input)
//         .unwrap()
//         .next()
//         .unwrap();

//     let inner_rules = o.into_inner();

//     let ret = inner_rules.collect::<Vec<_>>();
//     println!("{:?}", ret);
//     ret
// }

// #[test]
// fn main() {
//     get_pairs_from_line("/n12a3 B3 #[cos[1.2]]=1");
//     // let successful_parse = Rs274ngcParser::parse(Rule::num, "-273.15");

//     // for t in successful_parse.expect("") .tokens(){
//     // println!("{:?}", t);
//     // }

//     // let unsuccessful_parse = CSVParser::parse(Rule::num, "this is not a number");
//     // println!("{:?}", unsuccessful_parse);
// }

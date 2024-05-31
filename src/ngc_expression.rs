use crate::{BTreeMap, BlockError, Box, GCodes, GGroup, MCodes, MGroup, String, Vec};
use pest::Parser;
use pest_derive::Parser;

type GModalMap = BTreeMap<GGroup, GCodes>;
type MModalMap = BTreeMap<MGroup, MCodes>;

#[derive(Parser)]
#[grammar = "rs274ngc.pest"]
pub struct Rs274ngcParser;

pub use real::{set_get_parameter_function, Expr, RealValue};

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
        unimplemented!();
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
                Rule::parameter_value => Self::ParameterValue(Box::new(Self::from_rule(pair))),
                Rule::expression => Self::Expression(Box::new(Expr::from_rule(pair))),
                Rule::ordinary_unary_combo => {
                    let mut x = pair.into_inner();
                    let o = x.next().unwrap();
                    let rhs = x.next().unwrap();
                    let rhs = Expr::from_rule(rhs);
                    match o.as_rule() {
                        Rule::absolute_value => Self::UnaryOp(Unary::Abs(Box::new(rhs))),
                        Rule::arc_cosine => Self::UnaryOp(Unary::Acos(Box::new(rhs))),
                        Rule::arc_sine => Self::UnaryOp(Unary::Asin(Box::new(rhs))),
                        Rule::cosine => Self::UnaryOp(Unary::Cos(Box::new(rhs))),
                        Rule::e_raised_to => Self::UnaryOp(Unary::Exp(Box::new(rhs))),
                        Rule::fix_down => Self::UnaryOp(Unary::Fix(Box::new(rhs))),
                        Rule::fix_up => Self::UnaryOp(Unary::Fup(Box::new(rhs))),
                        Rule::natural_log_of => Self::UnaryOp(Unary::Ln(Box::new(rhs))),
                        Rule::round => Self::UnaryOp(Unary::Round(Box::new(rhs))),
                        Rule::sine => Self::UnaryOp(Unary::Sin(Box::new(rhs))),
                        Rule::square_root => Self::UnaryOp(Unary::Sqrt(Box::new(rhs))),
                        Rule::tangent => Self::UnaryOp(Unary::Tan(Box::new(rhs))),
                        _ => unreachable!(),
                    }
                }
                Rule::arc_tangent_combo => {
                    let mut x = pair.into_inner();
                    let lhs = x.next().unwrap();
                    let rhs = x.next().unwrap();

                    Self::UnaryOp(Unary::Atan {
                        lhs: Box::new(Expr::from_rule(lhs)),
                        rhs: Box::new(Expr::from_rule(rhs)),
                    })
                }
                _ => unreachable!("Expr::parse expected atom, found {:?}", pair),
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

        pub fn eval_expression(str: &str) -> Option<f32> {
            if let Ok(mut op) = Rs274ngcParser::parse(Rule::expression, str) {
                op.next().and_then(|y| Expr::from_rule(y).eval())
            } else {
                None
            }
        }
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

#[derive(Debug, PartialEq)]
pub struct ParameterSet {
    par_index: Box<Expr>,
    val: Box<Expr>,
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
    // pub motion_to_be: Option<GCodes>,
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
            // motion_to_be: None,
            g_modes: GModalMap::new(),
            m_modes: MModalMap::new(),
        }
    }
}

pub fn parse_line(str: &str) -> Result<Block, BlockError> {
    let o = Rs274ngcParser::parse(Rule::line, str)
        .expect("unsuccessful parse")
        .next()
        .unwrap();

    let mut block = Block::new();
    for field in o.into_inner() {
        match field.as_rule() {
            Rule::block_delete => return Ok(block),
            Rule::line_number => {
                let mut inner_rules = field.into_inner(); // { "n" ~ number_for_line }
                let value: &str = inner_rules.next().unwrap().as_str().trim();
                let val = value.parse::<u32>().unwrap();
                if val > 99999 {
                    return Err(BlockError::NceLineNumberGreaterThan99999);
                }
                block.n_number.replace(val);
            }
            Rule::segment => {
                let mut inner_rules = field.into_inner(); // { mid_line_word | comment | parameter_setting }
                let v = inner_rules.next().unwrap();

                match v.as_rule() {
                    Rule::mid_line_word => {
                        let mut inner_rules = v.into_inner(); // { mid_line_letter ~ real_value }
                        let mid_l_l = inner_rules.next().unwrap().into_inner().next().unwrap();
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
                                    return Err(BlockError::NceNegativeFWordUsed);
                                }
                                block.f_number = Some(real_val);
                            }
                            Rule::letter_d => {
                                let val = real_val as i32;
                                if val <= 0 {
                                    return Err(BlockError::NceNegativeFWordUsed);
                                }
                                block.d_number.replace(val);
                            }
                            Rule::letter_h => {
                                let val = real_val as i32;
                                if val <= 0 {
                                    return Err(
                                        BlockError::NceNegativeHWordToolLengthOffsetIndexUsed,
                                    );
                                }
                                block.h_number.replace(val);
                            }
                            Rule::letter_l => {
                                let val = real_val as i32;
                                if val <= 0 {
                                    return Err(BlockError::NceNegativeLWordUsed);
                                }
                                block.l_number.replace(val);
                            }
                            Rule::letter_p => {
                                if real_val <= 0.0 {
                                    return Err(BlockError::NceNegativePWordUsed);
                                }
                                block.p_number.replace(real_val);
                            }
                            Rule::letter_q => {
                                if real_val <= 0.0 {
                                    return Err(BlockError::NceNegativeOrZeroQValueUsed);
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
                                    return Err(BlockError::NceGCODEOutOfRange);
                                }

                                if value > 999 || value < 0 {
                                    return Err(BlockError::NceGCODEOutOfRange);
                                }

                                let v: Result<GCodes, _> = value.try_into();
                                match v {
                                    Ok(code) => {
                                        let mode = code.to_modal();
                                        block.g_modes.insert(mode, code);
                                    }
                                    Err(_) => return Err(BlockError::NceGCODEOutOfRange),
                                }
                            }
                            Rule::letter_m => {
                                let val = real_val as i32;
                                if val < 0 {
                                    return Err(BlockError::NceNegativeMCodeUsed);
                                } else if val > 99 {
                                    return Err(BlockError::NceMCodeGreaterThan99);
                                }

                                let v: Result<MCodes, _> = val.try_into();
                                match v {
                                    Ok(code) => {
                                        let mode = code.to_modal();
                                        if let Some(_) = block.m_modes.get(&mode) {
                                            return Err(
                                                BlockError::NceTwoMCodesUsedFromSameModalGroup,
                                            );
                                        }

                                        block.m_modes.insert(mode, code.clone());
                                    }
                                    Err(_) => return Err(BlockError::NceMCODEOutOfRange),
                                }
                            }
                            rule => unreachable!("found {:?}", rule),
                        }
                    }
                    Rule::comment => {}
                    Rule::parameter_setting => {
                        //parameter_setting = { "#" ~ parameter_index ~ "=" ~ real_value }
                        let mut inner_rules = v.into_inner();
                        let index = inner_rules.next().unwrap();
                        let real = inner_rules.next().unwrap();

                        let index_val = RealValue::from_rule(index);
                        let real_val = RealValue::from_rule(real);

                        println!("do parameter setting: #{:?} = {:?} ", index_val, real_val);
                    }
                    _ => unreachable!(),
                }
            }
            _ => {}
        }
    }

    Ok(block)
}

#[test]
fn test_line_parse() {
    let input = "n12 g92.1 a3 B3 #[cos[1.2]]=1";
    println!("{} parse result: block:{:#?}", input, parse_line(input));
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

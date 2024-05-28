use crate::{Box, String, Vec};
use pest::pratt_parser::PrattParser;
use pest::{
    iterators::{Pair, Pairs},
    Parser,
};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "rs274ngc.pest"]
pub struct Rs274ngcParser;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        // use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            .op(Op::infix(Rule::and, Left) | Op::infix(Rule::exclusive_or, Left) | Op::infix(Rule::minus, Left) | Op::infix(Rule::non_exclusive_or, Left) | Op::infix(Rule::plus, Left))
            .op(Op::infix(Rule::divided_by, Left) | Op::infix(Rule::modulo, Left) | Op::infix(Rule::times, Left))
            .op(Op::infix(Rule::power, Left))
    };
}
#[derive(Debug, PartialEq)]
pub enum Op {
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
impl Op {
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
}

#[derive(Debug, PartialEq)]
pub enum Unary {
    Abs(Box<Expr>),
    Acos(Box<Expr>),
    Asin(Box<Expr>),
    Cos(Box<Expr>),
    Exp(Box<Expr>),
    Fix(Box<Expr>),
    Fup(Box<Expr>),
    Ln(Box<Expr>),
    Round(Box<Expr>),
    Sin(Box<Expr>),
    Sqrt(Box<Expr>),
    Tan(Box<Expr>),
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
            _ => {
                unreachable!()
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryArcTangent {
    BinOp { lhs: Box<Expr>, rhs: Box<Expr> },
}
impl UnaryArcTangent {
    #[cfg(any(feature = "std", test))]
    pub fn dis(&self) -> String {
        match &self {
            &Self::BinOp { lhs, rhs } => {
                format!("atan {}/{}", lhs.dis(), rhs.dis())
            }
            _ => {
                unreachable!()
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    RealNumber(f32),
    ParameterValue(Box<Expr>),
    UnaryOp(Unary),
    UnaryArcTangentOp(UnaryArcTangent),
    // UnaryMinus(Box<Expr>),
    BinOp {
        lhs: Box<Expr>,
        op: Op,
        rhs: Box<Expr>,
    },
}
impl Expr {
    #[cfg(any(feature = "std", test))]
    pub fn dis(&self) -> String {
        match &self {
            &Self::RealNumber(x) => format!("({})", x),
            &Self::ParameterValue(x) => format!("(#{})", x.dis()),
            &Self::UnaryOp(x) => format!("({})", x.dis()),
            &Self::UnaryArcTangentOp(x) => format!("({})", x.dis()),
            &Self::BinOp { lhs, op, rhs } => format!("({}{}{})", lhs.dis(), op.dis(), rhs.dis()),
        }
    }
}

pub fn parse_expr(pairs: Pairs<Rule>) -> Expr {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::real_number => Expr::RealNumber(primary.as_str().parse::<f32>().unwrap()),
            Rule::parameter_value => {
                Expr::ParameterValue(Box::new(parse_expr(primary.into_inner())))
            }
            Rule::expression => parse_expr(primary.into_inner()),
            Rule::arc_tangent_combo => {
                let mut x = primary.into_inner();
                let lhs = x.next().unwrap();
                let rhs = x.next().unwrap();
                Expr::UnaryArcTangentOp(UnaryArcTangent::BinOp {
                    lhs: Box::new(parse_expr(lhs.into_inner())),
                    rhs: Box::new(parse_expr(rhs.into_inner())),
                })
            }

            Rule::ordinary_unary_combo => {
                let mut x = primary.into_inner();
                let o = x.next().unwrap();
                let rhs = x.next().unwrap();
                let rhs = parse_expr(rhs.into_inner());
                match o.as_rule() {
                    Rule::absolute_value => Expr::UnaryOp(Unary::Abs(Box::new(rhs))),
                    Rule::arc_cosine => Expr::UnaryOp(Unary::Acos(Box::new(rhs))),
                    Rule::arc_sine => Expr::UnaryOp(Unary::Asin(Box::new(rhs))),
                    Rule::cosine => Expr::UnaryOp(Unary::Cos(Box::new(rhs))),
                    Rule::e_raised_to => Expr::UnaryOp(Unary::Exp(Box::new(rhs))),
                    Rule::fix_down => Expr::UnaryOp(Unary::Fix(Box::new(rhs))),
                    Rule::fix_up => Expr::UnaryOp(Unary::Fup(Box::new(rhs))),
                    Rule::natural_log_of => Expr::UnaryOp(Unary::Ln(Box::new(rhs))),
                    Rule::round => Expr::UnaryOp(Unary::Round(Box::new(rhs))),
                    Rule::sine => Expr::UnaryOp(Unary::Sin(Box::new(rhs))),
                    Rule::square_root => Expr::UnaryOp(Unary::Sqrt(Box::new(rhs))),
                    Rule::tangent => Expr::UnaryOp(Unary::Tan(Box::new(rhs))),
                    _ => unreachable!(),
                }
            }

            _ => unreachable!("Expr::parse expected atom, found {:?}", primary),
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::power => Op::Power,
                Rule::divided_by => Op::Divide,
                Rule::modulo => Op::Modulo,
                Rule::times => Op::Times,
                Rule::and => Op::And,
                Rule::exclusive_or => Op::ExclusiveOr,
                Rule::minus => Op::Minus,
                Rule::non_exclusive_or => Op::NoExclusiveOr,
                Rule::plus => Op::Plus,
                _ => unreachable!("Expr::parse expected infix operation, found {:?}", op),
            };
            Expr::BinOp {
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
    // /// g_modes array in the block keeps track of which G modal groups are used on
    // /// a line of code
    // pub g_modes: GModalMap,
    // /// track of which M Modal groups
    // pub m_modes: MModalMap,

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

            // g_modes: GModalMap::new(),
            // m_modes: MModalMap::new(),
        }
    }
}

pub fn parse_line(str: &str) -> Option<Block> {
    let o = Rs274ngcParser::parse(Rule::line, str)
        .expect("unsuccessful parse")
        .next()
        .unwrap();

    let mut block = Block::new();
    for field in o.into_inner() {
        match field.as_rule() {
            Rule::block_delete => return None,
            Rule::line_number => {
                let mut inner_rules = field.into_inner(); // { "n" ~ ASCII_DIGIT{1,5} }
                let value: &str = inner_rules.next().unwrap().as_str();
                block.n_number.replace(value.parse::<u32>().unwrap());
            }
            Rule::segment => {
                let mut inner_rules = field.into_inner(); // { mid_line_word | comment | parameter_setting }
                let v = inner_rules.next().unwrap();

                match v.as_rule() {
                    Rule::mid_line_word => {
                        let mut inner_rules = v.into_inner(); // { mid_line_letter ~ real_value }
                        let mid_l_l = inner_rules.next().unwrap();
                        let real_val = inner_rules.next().unwrap();

                        match mid_l_l.as_rule() {
                            Rule::letter_a => {}
                            Rule::letter_b => {}
                            Rule::letter_c => {}
                            Rule::letter_d => {}
                            Rule::letter_f => {}
                            Rule::letter_g => {}
                            Rule::letter_h => {}
                            Rule::letter_i => {}
                            Rule::letter_j => {}
                            Rule::letter_k => {}
                            Rule::letter_l => {}
                            Rule::letter_m => {}
                            Rule::letter_p => {}
                            Rule::letter_q => {}
                            Rule::letter_r => {}
                            Rule::letter_s => {}
                            Rule::letter_t => {}
                            Rule::letter_x => {}
                            Rule::letter_y => {}
                            Rule::letter_z => {}
                            _ => unreachable!(),
                        }
                    }
                    Rule::comment => {}
                    Rule::parameter_setting => {}
                    _ => unreachable!(),
                }
            }
            _ => {}
        }
    }

    None
}

#[test]
fn test_expression() {
    assert_eq!(
        parse_expr(
            Rs274ngcParser::parse(Rule::expression, "[2+3]")
                .unwrap()
                .next()
                .unwrap()
                .into_inner()
        ),
        Expr::BinOp {
            lhs: Box::new(Expr::RealNumber(2.0)),
            op: Op::Plus,
            rhs: Box::new(Expr::RealNumber(3.0))
        }
    );
    assert_eq!(
        parse_expr(
            Rs274ngcParser::parse(Rule::expression, "[cos[2]+3]")
                .unwrap()
                .next()
                .unwrap()
                .into_inner()
        ),
        Expr::BinOp {
            lhs: Box::new(Expr::UnaryOp(Unary::Cos(Box::new(Expr::RealNumber(2.0))))),
            op: Op::Plus,
            rhs: Box::new(Expr::RealNumber(3.0))
        }
    );
    assert_eq!(
        parse_expr(
            Rs274ngcParser::parse(Rule::expression, "[#cos[2]+#3*4**-2]")
                .unwrap()
                .next()
                .unwrap()
                .into_inner()
        ),
        Expr::BinOp {
            lhs: Box::new(Expr::ParameterValue(Box::new(Expr::UnaryOp(Unary::Cos(
                Box::new(Expr::RealNumber(2.0))
            ))))),
            op: Op::Plus,
            rhs: Box::new(Expr::BinOp {
                lhs: Box::new(Expr::ParameterValue(Box::new(Expr::RealNumber(3.0)))),
                op: Op::Times,
                rhs: Box::new(Expr::BinOp {
                    lhs: Box::new(Expr::RealNumber(4.0)),
                    op: Op::Power,
                    rhs: Box::new(Expr::RealNumber(-2.0))
                })
            })
        }
    );

    assert_eq!(
        parse_expr(
            Rs274ngcParser::parse(Rule::expression, "[1.0+atan[2]/[12]*cos[1+2]]")
                .unwrap()
                .next()
                .unwrap()
                .into_inner()
        )
        .dis(),
        "((1)+((atan (2)/(12))*(cos((1)+(2)))))"
    );
    // let o = Rs274ngcParser::parse(Rule::expression, "[1.0+atan[2]/[12]*cos[1+2]]") //""
    //     .unwrap()
    //     .next()
    //     .unwrap();

    // let inner_rules = o.into_inner();
    // println!("{:?}", inner_rules);
    // println!("---{:?}", parse_expr(inner_rules).dis());
}

pub fn get_pairs_from_line(input: &str) -> Vec<Pair<Rule>> {
    let o = Rs274ngcParser::parse(Rule::line, input)
        .unwrap()
        .next()
        .unwrap();

    let inner_rules = o.into_inner();

    let ret = inner_rules.collect::<Vec<_>>();
    println!("{:?}", ret);
    ret
}

#[test]
fn main() {
    get_pairs_from_line("/n12a3 B3 #[cos[1.2]]=1");
    // let successful_parse = Rs274ngcParser::parse(Rule::num, "-273.15");

    // for t in successful_parse.expect("") .tokens(){
    // println!("{:?}", t);
    // }

    // let unsuccessful_parse = CSVParser::parse(Rule::num, "this is not a number");
    // println!("{:?}", unsuccessful_parse);
}

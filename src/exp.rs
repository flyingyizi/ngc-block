pub use ngc::{
    elements::{parse_to_l0vals, L0Val, NgcWord},
    evalpart::{eval_exp_string, eval_l0val},
    helper_par_assign, remove_ngc_comment, RefParsUtilTrait,
};
mod ngc {
    use alloc::string::String;

    use elements::L0Val;

    #[derive(Debug, PartialEq, Clone)]
    pub enum NgcErr<'a> {
        NgcComment,
        /// when nom parse fail, the remainder string
        Nom(&'a str),
    }

    /// util to write/read ngc paramenters. the paramenter key use defined key
    /// in `[reference\RS274NGC_3.pdf 3.2.1 Parameters]`. e.g. 5161 is "G28 home X"
    pub trait RefParsUtilTrait {
        /// get paramenter by key
        fn get_params(&self, key: i32) -> Option<f32>;
        /// set paramenter by key
        fn set_params(&mut self, _key: i32, _val: f32) {}
    }

    /// remove ngc comment form orig str. if ngc comment is invalid. then return None
    pub fn remove_ngc_comment(i: &mut String) -> Result<Option<String>, NgcErr> {
        if i.len() == 0 {
            return Ok(None);
        }
        let mut u = 0;
        for c in i.chars() {
            if c == '(' {
                break;
            }
            u += 1;
        }

        let start = u;
        let t = &i[u + 1..];

        let mut left_parentheses = 1;
        let mut index = 1;
        for c in t.chars() {
            if c == ')' {
                left_parentheses -= 1;
            } else if c == '(' {
                left_parentheses += 1;
            }
            index += 1;
            if left_parentheses == 0 {
                break;
            }
        }

        if left_parentheses != 0 {
            return Err(NgcErr::NgcComment);
        }

        let output = String::from(&i[start..(start + index)]);
        //remove comment part
        i.drain(start..(start + index));
        // remove whitespace
        i.retain(|c| !c.is_whitespace());

        Ok(Some(output))
    }

    pub fn helper_par_assign(
        v: &(L0Val /*left*/, L0Val /*right*/),
        pars: &impl RefParsUtilTrait,
    ) -> Option<(i32 /*left*/, f32 /*right*/)> {
        use evalpart::{eval_exp_string, eval_l0val};

        let ll = match v.0.clone() {
            L0Val::ExpIndex(s) => {
                if let Some(val) = eval_exp_string(s.as_str(), pars) {
                    val as i32
                } else {
                    return None;
                }
            }
            L0Val::PureIndex(v) => v as i32,
            _ => {
                unreachable!()
            }
        };
        let rr = if let Some(v) = eval_l0val(v.1.clone(), pars) {
            v
        } else {
            return None;
        };
        return Some((ll, rr));
    }
    /// parse word value and comment. word value storedd by enum [L0Val].if the value is a expresstion or paramenter index,
    /// it directly output them, dont do calculating.
    ///
    /// notes: it dont deal with ngc comment. suggest you firstly use [remove_ngc_comment] to remove and get comment part, then
    /// use \[parse_to_l0vals\] deal the remainder str .
    pub mod elements {
        use super::NgcErr;
        use alloc::{
            string::{String, ToString},
            vec::Vec,
        };

        use nom::Err;
        use nom::{
            branch::alt,
            bytes::complete::tag,
            character::complete::{multispace0, one_of},
            // combinator::cut,
            combinator::map,
            error::ErrorKind,
            multi::fold_many0,
            // number::complete,
            number::complete::float,
            sequence::preceded,
            sequence::tuple,
            IResult,
            Needed,
        };

        #[derive(Debug, PartialEq, Clone)]
        pub enum L0Val {
            /// e.g 12.3
            Raw(f32),
            /// e.g #12345
            PureIndex(f32),
            /// e.g #[...], comply with nested rule
            ExpIndex(String),
            /// e.g [...], comply with nested rule
            Exp(String),
        }
        #[derive(Debug, PartialEq, Clone)]
        pub enum NgcWord {
            // Comment(String),
            Word((char /*word*/, L0Val /*value*/)),
            /// left must be par index, right is a normal exp.
            ParAssign((L0Val /*left*/, L0Val /*right*/)),
        }

        ///
        pub fn parse_to_l0vals(i: &str) -> Result<Vec<NgcWord>, NgcErr> {
            if let Ok((i, j)) =
                fold_many0(p_single_ngc_element, Vec::new, |mut acc: Vec<_>, item| {
                    acc.push(item);
                    acc
                })(i)
            {
                if i.len() == 0 {
                    return Ok(j);
                }
            }
            return Err(NgcErr::Nom(i));
        }

        fn p_single_ngc_element<'a>(i: &'a str) -> IResult<&'a str, NgcWord> {
            preceded(
                multispace0,
                alt((
                    p_par_assign,
                    p_key_and_value,
                    // p_comment
                )),
            )(i)
        }

        /// notes: export to evalpart mod use
        pub fn real_value<'a>(i: &'a str) -> IResult<&'a str, L0Val> {
            preceded(
                multispace0,
                alt((
                    map(float, |v| L0Val::Raw(v)),
                    map(bracket_part, |s| L0Val::Exp(s.to_string())),
                    map(exp_index, |s| L0Val::ExpIndex(s.to_string())),
                    map(pure_index, |v| L0Val::PureIndex(v)),
                )),
            )(i)
        }

        // e.g "#1234 = [1+2]"
        fn p_par_assign<'a>(i: &'a str) -> IResult<&'a str, NgcWord> {
            let (i, (left, _k, right)) = tuple((
                preceded(
                    multispace0,
                    alt((
                        map(exp_index, |s| L0Val::ExpIndex(s.to_string())),
                        map(pure_index, |v| L0Val::PureIndex(v)),
                    )),
                ),
                preceded(multispace0, tag("=")),
                preceded(multispace0, real_value),
            ))(i)?;

            let t = NgcWord::ParAssign((left, right));
            Ok((i, t))

            // unimplemented!()
        }
        // fn p_comment<'a>(i: &'a str) -> IResult<&'a str, NgcWord> {
        //     if i.chars().nth(0) != Some('(') {
        //         return Err(Err::Error(nom::error::Error::new(i, ErrorKind::Char)));
        //     }
        //     let t = &i[1..];
        //     let mut left_parentheses = 1;
        //     let mut index = 1;
        //     for c in t.chars() {
        //         if c == ')' {
        //             left_parentheses -= 1;
        //         } else if c == '(' {
        //             left_parentheses += 1;
        //         }
        //         index += 1;
        //         if left_parentheses == 0 {
        //             break;
        //         }
        //     }
        //     if left_parentheses != 0 {
        //         return Err(Err::Incomplete(Needed::Unknown));
        //     }
        //     Ok((&i[index..], NgcWord::Comment(i[0..index].to_string())))
        // }

        fn p_key_and_value<'a>(i: &'a str) -> IResult<&'a str, NgcWord> {
            let (i, (key, value)) = tuple((
                preceded(
                    multispace0,
                    one_of("GABCDFHIJKLNMPQRSTXYZgabcdfhijklnmpqrstxyz"),
                ),
                preceded(multispace0, real_value),
            ))(i)?;

            let o = NgcWord::Word((key, value));
            return Ok((i, o));
        }

        /// notes: export to evalpart mod use
        pub fn bracket_part<'a>(i: &'a str) -> IResult<&'a str, &'a str> {
            // skip prefix white space
            let mut u = 0;
            for c in i.chars() {
                if char::is_whitespace(c) == false {
                    break;
                }
                u += 1;
            }
            let i = &i[u..];

            if i.chars().nth(0) != Some('[') {
                return Err(Err::Error(nom::error::Error::new(i, ErrorKind::Char)));
            }

            let t = &i[1..];
            let mut left_backet = 1;
            let mut index = 1;
            for c in t.chars() {
                if c == ']' {
                    left_backet -= 1;
                } else if c == '[' {
                    left_backet += 1;
                }
                index += 1;
                if left_backet == 0 {
                    break;
                }
            }

            if left_backet != 0 {
                return Err(Err::Incomplete(Needed::Unknown));
            }

            Ok((&i[index..], &i[0..index]))
        }

        fn exp_index<'a>(i: &'a str) -> IResult<&'a str, &'a str> {
            let (i, (_key, value)) = tuple((
                preceded(multispace0, tag("#")),
                preceded(multispace0, bracket_part),
            ))(i)?;
            return Ok((i, value));
        }

        fn pure_index<'a>(i: &'a str) -> IResult<&'a str, f32> {
            let (i, (_key, value)) = tuple((
                preceded(multispace0, tag("#")),
                preceded(multispace0, float),
            ))(i)?;
            return Ok((i, value));
        }
    }

    pub mod evalpart {
        use super::{
            elements::{bracket_part, real_value, L0Val},
            RefParsUtilTrait,
        };
        use alloc::{
            // boxed::Box,
            string::{String, ToString},
            vec::Vec,
        };
        use nom::{
            branch::alt,
            bytes::complete::{tag, tag_no_case},
            character::complete::multispace0,
            combinator::map,
            // error::ErrorKind,
            multi::fold_many0,
            sequence::preceded,
            sequence::tuple,
            // Err,
            IResult,
        };
        #[allow(unused_imports)]
        use num_traits::Float;

        #[derive(Clone, Debug, Eq, PartialEq)]
        enum OPUnary {
            // unary operations
            ABS(String),
            ACOS(String),
            ASIN(String),
            ATAN((String /*left*/, String /*right*/)),
            COS(String),
            EXP(String),
            FIX(String),
            FUP(String),
            LN(String),
            ROUND(String),
            SIN(String),
            SQRT(String),
            TAN(String),
        }
        impl OPUnary {
            pub fn eval(self, pars: &impl RefParsUtilTrait) -> Option<f32> {
                match self {
                    Self::ABS(s) => {
                        if let Some(t) = eval_exp_string(s.as_str(), pars) {
                            return Some(t.abs());
                        }
                    }
                    Self::ACOS(s) => {
                        if let Some(t) = eval_exp_string(s.as_str(), pars) {
                            return Some(t.acos().to_degrees());
                        }
                    }
                    Self::ASIN(s) => {
                        if let Some(t) = eval_exp_string(s.as_str(), pars) {
                            return Some(t.asin().to_degrees());
                        }
                    }
                    Self::ATAN(s) => {
                        if let Some(l) = eval_exp_string(s.0.as_str(), pars) {
                            if let Some(r) = eval_exp_string(s.1.as_str(), pars) {
                                return Some(l.atan2(r).to_degrees());
                            }
                        }
                    }
                    Self::COS(s) => {
                        if let Some(t) = eval_exp_string(s.as_str(), pars) {
                            return Some(t.to_radians().cos());
                        }
                    }
                    Self::SIN(s) => {
                        if let Some(t) = eval_exp_string(s.as_str(), pars) {
                            return Some(t.to_radians().sin());
                        }
                    }
                    Self::TAN(s) => {
                        if let Some(t) = eval_exp_string(s.as_str(), pars) {
                            return Some(t.to_radians().tan());
                        }
                    }
                    Self::EXP(s) => {
                        if let Some(t) = eval_exp_string(s.as_str(), pars) {
                            return Some(t.exp());
                        }
                    }
                    Self::FIX(s) => {
                        if let Some(t) = eval_exp_string(s.as_str(), pars) {
                            return Some(t.floor());
                        }
                    }
                    Self::FUP(s) => {
                        if let Some(t) = eval_exp_string(s.as_str(), pars) {
                            return Some(t.ceil());
                        }
                    }
                    Self::LN(s) => {
                        if let Some(t) = eval_exp_string(s.as_str(), pars) {
                            return Some(t.ln());
                        }
                    }
                    Self::ROUND(s) => {
                        if let Some(t) = eval_exp_string(s.as_str(), pars) {
                            return Some(t.round());
                        }
                    }
                    Self::SQRT(s) => {
                        if let Some(t) = eval_exp_string(s.as_str(), pars) {
                            return Some(t.sqrt());
                        }
                    }
                }
                None
            }
        }

        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        enum OPBinary1 {
            DividedBy,
            MODULO,
            POWER,
            TIMES,
        }
        impl OPBinary1 {
            pub fn eval(&self, left: f32, right: f32) -> f32 {
                match &self {
                    &Self::DividedBy => left / right,
                    &Self::MODULO => {
                        let t = ((left % right) + right) % right;

                        return if t < 0.0 {
                            /* always calculates a positive answer */
                            t + right.abs()
                        } else {
                            t
                        };
                    }
                    &Self::POWER => left.powf(right),
                    &Self::TIMES => left * right,
                }
            }
        }
        #[derive(Clone, Debug, Eq, PartialEq)]
        enum OPBinary2 {
            // AND2, ExclusiveOR, MINUS,   NONExclusiveOR, PLUS.
            AND2,
            ExclusiveOR,
            MINUS,
            NONExclusiveOR,
            PLUS,
        }
        impl OPBinary2 {
            pub fn eval(&self, left: f32, right: f32) -> f32 {
                match &self {
                    &Self::AND2 => {
                        if left == 0.0 || right == 0.0 {
                            0.0
                        } else {
                            1.0
                        }
                    }
                    &Self::ExclusiveOR => {
                        if (left == 0.0 && right != 0.0) || (left != 0.0 && right == 0.0) {
                            1.0
                        } else {
                            0.0
                        }
                    }
                    &Self::NONExclusiveOR => {
                        if left != 0.0 || right != 0.0 {
                            1.0
                        } else {
                            0.0
                        }
                    }
                    &Self::MINUS => left - right,
                    &Self::PLUS => left + right,
                }
            }
        }

        #[derive(Clone, Debug, Eq, PartialEq)]
        enum OPBinary {
            B1(OPBinary1),
            B2(OPBinary2),
        }
        impl OPBinary {
            /// priority
            pub fn pri(&self) -> i32 {
                match &self {
                    // OPBinary::RightBracket => return 1,
                    OPBinary::B1(b1) => match b1 {
                        OPBinary1::POWER => return 4,
                        _ => return 3,
                    },
                    OPBinary::B2(_) => return 2,
                }
            }
        }

        /// ExpElement inner used to be stored in stack to calculating
        #[derive(Clone, Debug)]
        enum ExpElement {
            Exp(L0Val),
            OPB(OPBinary),
            ExpUnary(OPUnary),
        }
        impl ExpElement {
            pub fn is_op_binary(&self) -> Option<OPBinary> {
                match &self {
                    &Self::OPB(v) => return Some(v.clone()),
                    _ => return None,
                }
            }
            /// if self is Exp**, not operator, return None
            pub fn pri(&self) -> Option<i32> {
                match &self {
                    &Self::OPB(v) => return Some(v.pri()),
                    _ => return None,
                }
            }
        }

        fn p_element<'a>(i: &'a str) -> IResult<&'a str, ExpElement> {
            alt((
                map(preceded(multispace0, p_opunary), |v| {
                    ExpElement::ExpUnary(v)
                }),
                map(preceded(multispace0, real_value), |v| ExpElement::Exp(v)),
                map(preceded(multispace0, p_opbinary), |v| ExpElement::OPB(v)),
            ))(i)
        }

        fn p_opbinary<'a>(i: &'a str) -> IResult<&'a str, OPBinary> {
            alt((
                map(preceded(multispace0, p_opbinary1), |v| OPBinary::B1(v)),
                map(preceded(multispace0, p_opbinary2), |v| OPBinary::B2(v)),
            ))(i)
        }

        // e.g. “ATAN[2]/[1+3]”
        fn p_atan_exp<'a>(i: &'a str) -> IResult<&'a str, OPUnary> {
            let (i, (_k, left, _a, right)) = tuple((
                preceded(multispace0, tag_no_case("ATAN")),
                preceded(multispace0, bracket_part),
                preceded(multispace0, tag("/")),
                preceded(multispace0, bracket_part),
            ))(i)?;
            let left: String = left.chars().filter(|c| !c.is_whitespace()).collect();
            let right: String = right.chars().filter(|c| !c.is_whitespace()).collect();

            let t = OPUnary::ATAN((left, right));
            Ok((i, t))

            // unimplemented!()
        }

        fn p_opunary<'a>(i: &'a str) -> IResult<&'a str, OPUnary> {
            alt((
                map(preceded(tag_no_case("abs"), bracket_part), |s| {
                    OPUnary::ABS(s.to_string())
                }),
                map(preceded(tag_no_case("ACOS"), bracket_part), |s| {
                    OPUnary::ACOS(s.to_string())
                }),
                map(preceded(tag_no_case("ASIN"), bracket_part), |s| {
                    OPUnary::ASIN(s.to_string())
                }),
                map(p_atan_exp, |s| s), //ATAN
                map(preceded(tag_no_case("COS"), bracket_part), |s| {
                    OPUnary::COS(s.to_string())
                }),
                map(preceded(tag_no_case("EXP"), bracket_part), |s| {
                    OPUnary::EXP(s.to_string())
                }),
                map(preceded(tag_no_case("FIX"), bracket_part), |s| {
                    OPUnary::FIX(s.to_string())
                }),
                map(preceded(tag_no_case("FUP"), bracket_part), |s| {
                    OPUnary::FUP(s.to_string())
                }),
                map(preceded(tag_no_case("LN"), bracket_part), |s| {
                    OPUnary::LN(s.to_string())
                }),
                map(preceded(tag_no_case("ROUND"), bracket_part), |s| {
                    OPUnary::ROUND(s.to_string())
                }),
                map(preceded(tag_no_case("SIN"), bracket_part), |s| {
                    OPUnary::SIN(s.to_string())
                }),
                map(preceded(tag_no_case("SQRT"), bracket_part), |s| {
                    OPUnary::SQRT(s.to_string())
                }),
                map(preceded(tag_no_case("TAN"), bracket_part), |s| {
                    OPUnary::TAN(s.to_string())
                }),
            ))(i)
        }
        fn p_opbinary1<'a>(i: &'a str) -> IResult<&'a str, OPBinary1> {
            alt((
                map(tag_no_case("/"), |_| OPBinary1::DividedBy),
                map(tag_no_case("MOD"), |_| OPBinary1::MODULO),
                map(tag_no_case("**"), |_| OPBinary1::POWER), //priority to *
                map(tag_no_case("*"), |_| OPBinary1::TIMES),
            ))(i)
        }

        fn p_opbinary2<'a>(i: &'a str) -> IResult<&'a str, OPBinary2> {
            alt((
                map(tag_no_case("AND"), |_| OPBinary2::AND2),
                map(tag_no_case("XOR"), |_| OPBinary2::ExclusiveOR),
                map(tag_no_case("OR"), |_| OPBinary2::NONExclusiveOR),
                map(tag_no_case("+"), |_| OPBinary2::PLUS),
                map(tag_no_case("-"), |_| OPBinary2::MINUS),
            ))(i)
        }

        /// OUTSIDE not directly call it, you should call eval_exp_string to skip "[1 -- 3]" simular problem
        /// [Exp(Raw(1.0)), OPB(B2(MINUS)), OPB(B2(MINUS)), Exp(Raw(2.0))]
        fn exp_to_stack(i: &str) -> IResult<&str, Vec<ExpElement>> {
            let i = if let Some('[') = i.chars().nth(0) {
                &i[1..i.len() - 1]
            } else {
                i
            };

            fold_many0(
                p_element,
                Vec::<ExpElement>::new,
                |mut acc: Vec<_>, item| {
                    //例外 +/-, 会被pure number吃掉。 例如" 1+2"会被解析为“1”与“+2 即2”,这不是我们期望的。
                    if acc.len() > 0 {
                        let last: &_ = &acc[acc.len() - 1];
                        if item.is_op_binary().is_none() {
                            if last.is_op_binary().is_none() {
                                acc.push(ExpElement::OPB(OPBinary::B2(OPBinary2::PLUS)));
                            }
                        }
                    }
                    acc.push(item);
                    acc
                },
            )(i)
        }

        pub fn eval_exp_string(i: &str, pars: &impl RefParsUtilTrait) -> Option<f32> {
            let s: String = i.chars().filter(|c| !c.is_whitespace()).collect();
            let i = s.as_str();
            if let Ok((_i, mut y)) = exp_to_stack(i) {
                return eval(&mut y, pars);
            } else {
                return None;
            }
        }

        pub fn eval_l0val(val: L0Val, pars: &impl RefParsUtilTrait) -> Option<f32> {
            match val {
                L0Val::Raw(v) => return Some(v),
                L0Val::ExpIndex(s) => {
                    if let Some(key) = eval_exp_string(s.as_str(), pars) {
                        if let Some(v) = pars.get_params(key as i32) {
                            return Some(v);
                        }
                    }
                    return None;
                }
                L0Val::PureIndex(key) => {
                    if let Some(v) = pars.get_params(key as i32) {
                        return Some(v);
                    }

                    return None;
                }
                L0Val::Exp(s) => {
                    return eval_exp_string(s.as_str(), pars);
                }
            }
        }
        fn eval_expelement(val: &ExpElement, pars: &impl RefParsUtilTrait) -> Option<f32> {
            match val.clone() {
                ExpElement::Exp(v) => return eval_l0val(v, pars),
                ExpElement::ExpUnary(v) => return v.eval(pars),
                ExpElement::OPB(_) => return None,
            }
        }

        fn eval(vecs: &mut Vec<ExpElement>, pars: &impl RefParsUtilTrait) -> Option<f32> {
            /// when val is not exp, or eval fail, return None
            fn do_binary_calc(
                left: &ExpElement,
                op: &ExpElement,
                right: &ExpElement,
                pars: &impl RefParsUtilTrait,
            ) -> Option<f32> {
                if let Some(op) = op.is_op_binary() {
                    let (left, right) = (eval_expelement(left, pars), eval_expelement(right, pars));
                    if left.is_none() || right.is_none() {
                        return None;
                    }
                    let (left, right) = (left.unwrap(), right.unwrap());
                    match op {
                        OPBinary::B1(v) => {
                            let l = v.eval(left, right);
                            return Some(l);
                        }
                        OPBinary::B2(v) => {
                            let l = v.eval(left, right);
                            return Some(l);
                        }
                    }
                }
                None
            }

            if vecs.len() == 0 {
                return None;
            }

            if vecs.len() == 1 {
                let x = vecs.pop().unwrap();
                return eval_expelement(&x, pars);
            }

            //
            if vecs.len() == 3 {
                let (left, op, right) = (&vecs[0], &vecs[1], &vecs[2]);
                let ret = do_binary_calc(left, op, right, pars);
                vecs.clear();
                return ret;
            }

            if vecs.len() < 5 {
                return None;
            }

            let last_i = vecs.len() - 1;
            let (left1, op1, left, op2, right) = (
                &vecs[last_i - 4],
                &vecs[last_i - 3],
                &vecs[last_i - 2],
                &vecs[last_i - 1],
                &vecs[last_i],
            );
            let (op1_pri, op2_pri) = (op1.pri(), op2.pri());
            if op1_pri.is_none() || op2_pri.is_none() {
                return None;
            }
            let (op1_pri, op2_pri) = (op1_pri.unwrap(), op2_pri.unwrap());

            if op1_pri > op2_pri {
                if let Some(ll) = do_binary_calc(left1, op1, left, pars) {
                    let (right, op2, _, _, _) =
                        (vecs.pop(), vecs.pop(), vecs.pop(), vecs.pop(), vecs.pop());
                    let t = ExpElement::Exp(L0Val::Raw(ll));
                    vecs.push(t);
                    vecs.push(op2.unwrap());
                    vecs.push(right.unwrap());
                    return eval(vecs, pars);
                }
            } else {
                if let Some(ll) = do_binary_calc(left, op2, right, pars) {
                    let (_, _, _) = (vecs.pop(), vecs.pop(), vecs.pop());
                    let t = ExpElement::Exp(L0Val::Raw(ll));
                    vecs.push(t);
                    return eval(vecs, pars);
                }
            }

            None
        }

        #[cfg(test)]
        mod tests {
            use super::*;

            struct TestSettings {}

            impl RefParsUtilTrait for TestSettings {
                fn get_params(&self, key: i32) -> Option<f32> {
                    if key == 12345 {
                        return Some(100.);
                    }
                    return Some(101.);
                }
            }

            #[test]
            fn test_eval() {
                let pars = TestSettings {};
                // case 0
                let t = eval_exp_string("[1+[2**[4.0/2]]]", &pars);
                assert_eq!(t, Some(5.));

                // case 1
                let t = eval_exp_string("[acos[0]]", &pars);
                assert_eq!(t, Some(0.0_f32.acos().to_degrees()));
                //case 2
                let t = eval_exp_string("[1+acos[0.4]-[10.0**[4.0/2]]]", &pars);
                let to_be = 1. + 0.4_f32.acos().to_degrees() - 10.0_f32.powf(4.0 / 2.);
                assert_eq!(t, Some(to_be));

                // case 3
                let t = eval_exp_string("[1+acos[0.4]-[#12345* 10.0**[4.0/2]]]", &pars);
                let to_be =
                    1. + 0.4_f32.acos().to_degrees() - 100.0_f32 * (10.0_f32.powf(4.0 / 2.));
                assert_eq!(t, Some(to_be));

                // case 4
                let t = eval_exp_string("#[12345.0+1.0]", &pars);
                let to_be = 101.0_f32;
                assert_eq!(t, Some(to_be));
            }

            /// test case is from rs274ngc
            #[test]
            fn test_eval2() {
                let pars = TestSettings {};
                assert_eq!(eval_exp_string("[1 + 2]", &pars), Some(3.));
                assert_eq!(eval_exp_string("[1 - 2]", &pars), Some(-1.));
                println!("{:?}", exp_to_stack("[1 -- 2]").unwrap());
                assert_eq!(eval_exp_string("[1 -- 2]", &pars), Some(3.));
                assert_eq!(eval_exp_string("[2/5]", &pars), Some(0.4));
                assert_eq!(eval_exp_string("[3.0 * 5]", &pars), Some(15.));
                assert_eq!(eval_exp_string("[0 OR 0]", &pars), Some(0.));
                assert_eq!(eval_exp_string("[0 OR 1]", &pars), Some(1.));
                assert_eq!(eval_exp_string("[2 OR 2]", &pars), Some(1.));
                assert_eq!(eval_exp_string("[0 AND 0]", &pars), Some(0.));

                assert_eq!(eval_exp_string("[2 AND 2]", &pars), Some(1.));
                assert_eq!(eval_exp_string("[0 XOR 0]", &pars), Some(0.));
                assert_eq!(eval_exp_string("[0 XOR 1]", &pars), Some(1.));
                assert_eq!(eval_exp_string("[2 XOR 2]", &pars), Some(0.));
                assert_eq!(eval_exp_string("[15 MOD 4.0]", &pars), Some(3.));
                assert_eq!(eval_exp_string("[1 + 2 * 3 - 4 / 5]", &pars), Some(6.2));
                assert_eq!(eval_exp_string("[sin[30]]", &pars), Some(0.5));
                assert_eq!(eval_exp_string("[cos[0.0]]", &pars), Some(1.));
                // n0200 x tan[60.0] (x should be 1.7321)
                assert_eq!(
                    eval_exp_string("[tan[60.0]]", &pars),
                    Some(60.0_f32.to_radians().tan())
                );
                assert_eq!(eval_exp_string("[sqrt[3]]", &pars), Some(3.0_f32.sqrt()));

                // n0220 x atan[1.7321]/[1.0] (x should be 60.0)
                assert_eq!(
                    eval_exp_string("atan[1.7321]/[1.0]", &pars),
                    Some(1.7321_f32.atan2(1.0).to_degrees())
                );

                // n0230 x asin[1.0] (x should be 90.0)
                assert_eq!(eval_exp_string("[asin[1.0]]", &pars), Some(90.));
                // n0240 x acos[0.707107] (x should be 45.0000)
                assert_eq!(
                    eval_exp_string("[acos[0.707107]]", &pars),
                    Some(0.707107_f32.acos().to_degrees())
                );

                assert_eq!(eval_exp_string("fup[-0.499]", &pars), Some(0.0));
                assert_eq!(eval_exp_string("fup[-0.5001]", &pars), Some(0.0));
                assert_eq!(eval_exp_string("fup[2.444]", &pars), Some(3.0));
                assert_eq!(eval_exp_string("fup[9.975]", &pars), Some(10.0));
                assert_eq!(eval_exp_string("fIX[-0.499]", &pars), Some(-1.0));
                assert_eq!(eval_exp_string("fIX[-0.5001]", &pars), Some(-1.0));
                assert_eq!(eval_exp_string("fIX[2.444]", &pars), Some(2.0));
                assert_eq!(eval_exp_string("fIX[9.975]", &pars), Some(9.0));

                assert_eq!(eval_exp_string("ROUND[-0.499]", &pars), Some(0.0));
                assert_eq!(eval_exp_string("ROUND[-0.5001]", &pars), Some(-1.0));
                assert_eq!(eval_exp_string("ROUND[2.444]", &pars), Some(2.0));
                assert_eq!(eval_exp_string("round[9.975]", &pars), Some(10.0));

                // n0390 x exp[2.3026] (x should be 10)
                assert_eq!(
                    eval_exp_string("exp[2.3026]", &pars),
                    Some(2.3026_f32.exp())
                );
                // n0400 x ln[10.0] (x should be 2.3026)
                assert_eq!(eval_exp_string("ln[10.]", &pars), Some(10.0_f32.ln()));

                assert_eq!(eval_exp_string("sqrt[3**2 + 4**2]", &pars), Some(5.));
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::{
            elements::{parse_to_l0vals, L0Val, NgcWord},
            remove_ngc_comment,
        };

        #[test]
        fn test_remove() {
            let mut i = "A #12345 A # [x[xx]x] A [cos[ab]*c] (this is comment) yyyyyyy".to_string();
            // let mut i = "".to_string();
            let o = remove_ngc_comment(&mut i).unwrap_or(None);

            let str = i.as_mut_str();
            str.make_ascii_lowercase();

            println! {"c:{:?}  ;r:{:?}  ;", str, o}
        }
        #[test]
        fn test_parse_ngc_elements() {
            let mut i = "A #12345 A # [x[xx]x] A [cos[ab]*c] (this is comment) ".to_string();
            println!("orig:{:?}", i);
            // let mut i = "".to_string();
            let comment = remove_ngc_comment(&mut i).unwrap_or(None);

            let str = i.as_mut_str();
            str.make_ascii_lowercase();

            println!("comment:{:?}, to parse{:?}", comment, str);
            let x = parse_to_l0vals(str);
            assert_eq!(x.is_ok(), true);
            let vecs = x.unwrap();
            assert_eq!(vecs[0], NgcWord::Word(('a', L0Val::PureIndex(12345.0))));
            assert_eq!(
                vecs[1],
                NgcWord::Word(('a', L0Val::ExpIndex("[x[xx]x]".to_string())))
            );
            assert_eq!(
                vecs[2],
                NgcWord::Word(('a', L0Val::Exp("[cos[ab]*c]".to_string())))
            );
        }
    }
}

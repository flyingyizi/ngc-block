use super::exp::RefParsUtilTrait;

// use num_traits::Float;
#[derive(Clone, Copy, Debug, PartialEq /* , Eq, */)]
pub struct Position {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub aa: f32,
    pub bb: f32,
    pub cc: f32,
}
impl Position {
    pub fn new(x: f32, y: f32, z: f32, aa: f32, bb: f32, cc: f32) -> Self {
        Self {
            x,
            y,
            z,
            aa,
            bb,
            cc,
        }
    }
}

impl core::ops::Sub for Position {
    type Output = Self;
    #[inline]
    fn sub(self, rhs: Self::Output) -> Self {
        Position::new(
            self.x - rhs.x,
            self.y - rhs.y,
            self.z - rhs.z,
            self.aa - rhs.aa,
            self.bb - rhs.bb,
            self.cc - rhs.cc,
        )
    }
}

impl core::ops::Add for Position {
    type Output = Self;
    #[inline]
    fn add(self, rhs: Self::Output) -> Self {
        Position::new(
            self.x + rhs.x,
            self.y + rhs.y,
            self.z + rhs.z,
            self.aa + rhs.aa,
            self.bb + rhs.bb,
            self.cc + rhs.cc,
        )
    }
}

impl core::ops::MulAssign<f32> for Position {
    #[inline]
    fn mul_assign(&mut self, rhs: f32) {
        self.x *= rhs;
        self.y *= rhs;
        self.z *= rhs;
        self.aa *= rhs;
        self.bb *= rhs;
        self.cc *= rhs;
    }
}

impl core::ops::Div<f32> for Position {
    type Output = Self;
    fn div(self, rhs: f32) -> Self::Output {
        Self {
            x: self.x / rhs,
            y: self.y / rhs,
            z: self.z / rhs,
            aa: self.aa / rhs,
            bb: self.bb / rhs,
            cc: self.cc / rhs,
        }
    }
}

impl core::default::Default for Position {
    fn default() -> Self {
        Self {
            x: 0.0,
            y: 0.0,
            z: 0.0,
            aa: 0.0,
            bb: 0.0,
            cc: 0.0,
        }
    }
}
/// helper enum, used to simplify setting/getting [Paramenters]
///
/// example
/// ```text
/// let mut store: Paramters = Paramters::new();
/// let p = Position::new(1., 0., 0., 0., 0., 0.);
/// let v = Argument::CoordSys1(p);
/// store.update_item(&v);
/// ```
#[derive(Clone, Debug /* , Eq, PartialEq*/)]
pub enum Argument {
    /// page13 Table 2. Default Parameter File. after success probe, 5061~5066 store the last controlled point
    ProbePos(Position),

    /// PROBE_ANALOG_VALUE = 5067,
    ProbeAnalogValue(f32),

    G28Home(Position),
    G30Home(Position),
    /// when execute G92 store in below parameter 5211~5216
    G92Offset(Position),
    /// COORD_SYS_SELECTED = 5220, //coord. system number
    CoordSysSelected(i32),
    /// coord. system 1
    CoordSys1(Position),
    /// coord. system 2
    CoordSys2(Position),
    /// coord. system 3
    CoordSys3(Position),
    /// coord. system 4
    CoordSys4(Position),
    /// coord. system 5
    CoordSys5(Position),
    /// coord. system 6
    CoordSys6(Position),
    /// coord. system 7
    CoordSys7(Position),
    /// coord. system 8
    CoordSys8(Position),
    /// coord. system 9
    CoordSys9(Position),
}

impl Argument {
    pub fn to_para_index(&self) -> ParaIndex {
        let r = match self {
            &Argument::CoordSys1(_) => ParaIndex::CoordSys1,
            &Argument::CoordSys2(_) => ParaIndex::CoordSys2,
            &Argument::CoordSys3(_) => ParaIndex::CoordSys3,
            &Argument::CoordSys4(_) => ParaIndex::CoordSys4,
            &Argument::CoordSys5(_) => ParaIndex::CoordSys5,
            &Argument::CoordSys6(_) => ParaIndex::CoordSys6,
            &Argument::CoordSys7(_) => ParaIndex::CoordSys7,
            &Argument::CoordSys8(_) => ParaIndex::CoordSys8,
            &Argument::CoordSys9(_) => ParaIndex::CoordSys9,

            &Argument::ProbePos(_) => ParaIndex::ProbePos,
            &Argument::ProbeAnalogValue(_) => ParaIndex::ProbeAnalogValue,
            &Argument::G28Home(_) => ParaIndex::G28Home,
            &Argument::G30Home(_) => ParaIndex::G30Home,
            &Argument::G92Offset(_) => ParaIndex::G92Offset,
            &Argument::CoordSysSelected(_) => ParaIndex::CoordSysSelected,
        };
        r
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum ParaIndex {
    ProbePos = 5061,
    ProbeAnalogValue = 5067,

    G28Home = 5161, //G28 home
    G30Home = 5181, //G30 home

    //when execute G92 store in below parameter 5211~5216
    G92Offset = 5211, //G92 offset

    CoordSysSelected = 5220, //coord. system number
    CoordSys1 = 5221,        //coord. system 1
    CoordSys2 = 5241,        //coord. system 2
    CoordSys3 = 5261,        //coord. system 3
    CoordSys4 = 5281,        //coord. system 4
    CoordSys5 = 5301,        //coord. system 5
    CoordSys6 = 5321,        //coord. system 6
    CoordSys7 = 5341,        //coord. system 7
    CoordSys8 = 5361,        //coord. system 8
    CoordSys9 = 5381,        //coord. system 9
}

use crate::BTreeMap;
type TParamtersMap = BTreeMap<ParaIndex, Argument>;

/// COMPLAY with `[reference\RS274NGC_3.pdf 3.2.1 Parameters]`
#[derive(Clone)]
pub struct Paramters(TParamtersMap);

impl Paramters {
    pub fn new() -> Self {
        Self {
            0: TParamtersMap::new(),
        }
    }
    #[allow(dead_code)]
    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn get_coord_sys_selected(&self) -> Option<i32> {
        if let Some(v) = self.0.get(&ParaIndex::CoordSysSelected) {
            match v {
                &Argument::CoordSysSelected(v) => return Some(v.clone()),
                _ => return None,
            }
        }
        None
    }

    pub fn get_g92_offset(&self) -> Option<Position> {
        if let Some(v) = self.0.get(&ParaIndex::G92Offset) {
            match v {
                &Argument::G92Offset(x) => return Some(x.clone()),
                _ => {}
            }
        }
        None
    }
    /// orig is 1,2,3..9
    pub fn get_sys_coord(&self, orig: i32) -> Option<Position> {
        let index: ParaIndex;
        match orig {
            1 => index = ParaIndex::CoordSys1,
            2 => index = ParaIndex::CoordSys2,
            3 => index = ParaIndex::CoordSys3,
            4 => index = ParaIndex::CoordSys4,
            5 => index = ParaIndex::CoordSys5,
            6 => index = ParaIndex::CoordSys6,
            7 => index = ParaIndex::CoordSys7,
            8 => index = ParaIndex::CoordSys8,
            9 => index = ParaIndex::CoordSys9,
            _ => return None,
        }

        if let Some(v) = self.0.get(&index) {
            match v {
                &Argument::CoordSys1(x) => return Some(x.clone()),
                &Argument::CoordSys2(x) => return Some(x.clone()),
                &Argument::CoordSys3(x) => return Some(x.clone()),
                &Argument::CoordSys4(x) => return Some(x.clone()),
                &Argument::CoordSys5(x) => return Some(x.clone()),
                &Argument::CoordSys6(x) => return Some(x.clone()),
                &Argument::CoordSys7(x) => return Some(x.clone()),
                &Argument::CoordSys8(x) => return Some(x.clone()),
                &Argument::CoordSys9(x) => return Some(x.clone()),
                _ => {}
            }
        }
        None
    }
    /// orig is 1,2,3..9

    pub fn set_sys_coord(&mut self, orig: i32, pos: &Position) -> Result<(), ()> {
        // let mut index: ParaIndex ;
        match orig {
            1 => self.update_item(&Argument::CoordSys1(pos.clone())),
            2 => self.update_item(&Argument::CoordSys2(pos.clone())),
            3 => self.update_item(&Argument::CoordSys3(pos.clone())),
            4 => self.update_item(&Argument::CoordSys4(pos.clone())),
            5 => self.update_item(&Argument::CoordSys5(pos.clone())),
            6 => self.update_item(&Argument::CoordSys6(pos.clone())),
            7 => self.update_item(&Argument::CoordSys7(pos.clone())),
            8 => self.update_item(&Argument::CoordSys8(pos.clone())),
            9 => self.update_item(&Argument::CoordSys9(pos.clone())),

            _ => return Err(()),
        }
        Ok(())
    }

    pub fn get_field(&self, field_index: FieldIndex) -> Option<f32> {
        // i should be 0,1,2..5
        let get_by_index = |pos: &Position, offset: i32| -> Option<f32> {
            match offset {
                0 => return Some(pos.x),
                1 => return Some(pos.y),
                2 => return Some(pos.z),
                3 => return Some(pos.aa),
                4 => return Some(pos.bb),
                5 => return Some(pos.cc),
                _ => return None,
            }
        };

        let (par_index, ofst) = field_index.info();

        if let Some(p) = self.0.get(&par_index) {
            match p {
                &Argument::ProbePos(v) => return get_by_index(&v, ofst),
                &Argument::ProbeAnalogValue(v) => return Some(v),
                &Argument::G28Home(v) => return get_by_index(&v, ofst),
                &Argument::G30Home(v) => return get_by_index(&v, ofst),
                &Argument::G92Offset(v) => return get_by_index(&v, ofst),
                &Argument::CoordSysSelected(v) => return Some(v as f32),
                &Argument::CoordSys1(v) => return get_by_index(&v, ofst),
                &Argument::CoordSys2(v) => return get_by_index(&v, ofst),
                &Argument::CoordSys3(v) => return get_by_index(&v, ofst),
                &Argument::CoordSys4(v) => return get_by_index(&v, ofst),
                &Argument::CoordSys5(v) => return get_by_index(&v, ofst),
                &Argument::CoordSys6(v) => return get_by_index(&v, ofst),
                &Argument::CoordSys7(v) => return get_by_index(&v, ofst),
                &Argument::CoordSys8(v) => return get_by_index(&v, ofst),
                &Argument::CoordSys9(v) => return get_by_index(&v, ofst),
            }
        }

        None
    }

    /// update by item
    ///
    pub fn update_item(&mut self, value: &Argument) {
        let index = value.to_para_index();
        let _ = self.0.insert(index, value.clone());
    }
    /// update by field
    ///
    pub fn update_field(&mut self, field_index: FieldIndex, value: f32) {
        // return a new instance with replaced field identified by input
        // i should be 0,1,2..5
        let replace_by_index = |pos: &Position, i: i32, value: f32| -> Position {
            match i {
                0 => Position { x: value, ..*pos },
                1 => Position { y: value, ..*pos },
                2 => Position { z: value, ..*pos },
                3 => Position { aa: value, ..*pos },
                4 => Position { bb: value, ..*pos },
                5 => Position { cc: value, ..*pos },
                _ => *pos,
            }
        };

        let (par_index, ofst) = field_index.info();

        if let Some(p) = self.0.get(&par_index) {
            match p {
                &Argument::ProbeAnalogValue(v) => {
                    let _ = self.0.insert(par_index, Argument::ProbeAnalogValue(v));
                }
                &Argument::CoordSysSelected(v) => {
                    let _ = self.0.insert(par_index, Argument::CoordSysSelected(v));
                }

                &Argument::ProbePos(v) => {
                    let xx = replace_by_index(&v, ofst, value);
                    let _ = self.0.insert(par_index, Argument::ProbePos(xx));
                }
                &Argument::G28Home(v) => {
                    let xx = replace_by_index(&v, ofst, value);
                    let _ = self.0.insert(par_index, Argument::G28Home(xx));
                }
                &Argument::G30Home(v) => {
                    let xx = replace_by_index(&v, ofst, value);
                    let _ = self.0.insert(par_index, Argument::G30Home(xx));
                }
                &Argument::G92Offset(v) => {
                    let xx = replace_by_index(&v, ofst, value);
                    let _ = self.0.insert(par_index, Argument::G92Offset(xx));
                }
                &Argument::CoordSys1(v) => {
                    let xx = replace_by_index(&v, ofst, value);
                    let _ = self.0.insert(par_index, Argument::CoordSys1(xx));
                }
                &Argument::CoordSys2(v) => {
                    let xx = replace_by_index(&v, ofst, value);
                    let _ = self.0.insert(par_index, Argument::CoordSys2(xx));
                }
                &Argument::CoordSys3(v) => {
                    let xx = replace_by_index(&v, ofst, value);
                    let _ = self.0.insert(par_index, Argument::CoordSys3(xx));
                }
                &Argument::CoordSys4(v) => {
                    let xx = replace_by_index(&v, ofst, value);
                    let _ = self.0.insert(par_index, Argument::CoordSys4(xx));
                }
                &Argument::CoordSys5(v) => {
                    let xx = replace_by_index(&v, ofst, value);
                    let _ = self.0.insert(par_index, Argument::CoordSys5(xx));
                }
                &Argument::CoordSys6(v) => {
                    let xx = replace_by_index(&v, ofst, value);
                    let _ = self.0.insert(par_index, Argument::CoordSys6(xx));
                }
                &Argument::CoordSys7(v) => {
                    let xx = replace_by_index(&v, ofst, value);
                    let _ = self.0.insert(par_index, Argument::CoordSys7(xx));
                }
                &Argument::CoordSys8(v) => {
                    let xx = replace_by_index(&v, ofst, value);
                    let _ = self.0.insert(par_index, Argument::CoordSys8(xx));
                }
                &Argument::CoordSys9(v) => {
                    let xx = replace_by_index(&v, ofst, value);
                    let _ = self.0.insert(par_index, Argument::CoordSys9(xx));
                }
            }
        }
    }
}

impl RefParsUtilTrait for Paramters {
    fn get_params(&self, key: i32) -> Option<f32> {
        //convert i32 to FieldIndex
        let ret: Result<FieldIndex, _> = key.try_into();
        if let Ok(key) = ret {
            return self.get_field(key);
        }
        return None;
    }
    /// set paramenter by key
    fn set_params(&mut self, key: i32, val: f32) {
        let ret: Result<FieldIndex, _> = key.try_into();
        if let Ok(key) = ret {
            self.update_field(key, val);
        }
    }
}

/// marco used for enum class, you can convert i32 to enum
///
/// # example
/// ```text
/// i32_to_enum! {
///     #[derive(Clone, Copy, Debug, Eq, PartialEq)]
///     pub enum SampleEnum {
///         A = 1,
///         B = 2,   
///     }
/// }
/// let value = 1;
/// let v: Result<SampleEnum, _> = value.try_into();
/// match v {
///     Ok(i) => {
///         assert_eq!(i, 1);
///     }
///     Err(_) => {},
/// }
/// ```
///
macro_rules! i32_to_enum {
    ($(#[$meta:meta])* $vis:vis enum $name:ident {
        $($(#[$vmeta:meta])* $vname:ident $(= $val:expr)?,)*
    }) => {
        $(#[$meta])*
        $vis enum $name {
            $($(#[$vmeta])* $vname $(= $val)?,)*
        }

        impl core::convert::TryFrom<i32> for $name {
            type Error = ();

            fn try_from(v: i32) -> Result<Self, Self::Error> {
                match v {
                    $(x if x == $name::$vname as i32 => Ok($name::$vname),)*
                    _ => Err(()),
                }
            }
        }
    }
}

i32_to_enum! {

/// paramenter field index. COMPLAY `[reference\RS274NGC_3.pdf 3.2.1 Parameters]`. e.g. 5161 is "G28 home X"
#[allow(non_camel_case_types)]
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub enum FieldIndex {
        //page13 Table 2. Default Parameter File
        ProbePosX = 5061, //after success probe, 5061~5066 store the last controlled point
        ProbePosY = 5062,
        ProbePosZ = 5063,
        ProbePosA = 5064,
        ProbePosB = 5065,
        ProbePosC = 5066,
        PROBE_ANALOG_VALUE = 5067,

        G28HX = 5161, //G28 home X
        G28HY = 5162, //G28 home Y
        G28HZ = 5163, //G28 home Z
        G28HA = 5164, //G28 home A
        G28HB = 5165, //G28 home B
        G28HC = 5166, //G28 home C

        G30HX = 5181, //G30 home X
        G30HY = 5182, //G30 home Y
        G30HZ = 5183, //G30 home Z
        G30HA = 5184, //G30 home A
        G30HB = 5185, //G30 home B
        G30HC = 5186, //G30 home C

        //when execute G92 store in below parameter 5211~5216
        G92_OFST_X = 5211, //G92 offset X
        G92_OFST_Y = 5212, //G92 offset Y
        G92_OFST_Z = 5213, //G92 offset Z
        G92_OFST_A = 5214, //G92 offset A
        G92_OFST_B = 5215, //G92 offset B
        G92_OFST_C = 5216, //G92 offset C
        /// coord. system number 5220
        COORD_SYS_SELECTED = 5220,
        /// coord. system 1 X 5221
        CoordSys1X = 5221,
        /// coord. system 1 Y 5222
        CoordSys1Y = 5222,
        /// coord. system 1 Z 5223
        CoordSys1Z = 5223,
        /// coord. system 1 A 5224
        CoordSys1A = 5224,
        /// coord. system 1 B 5225
        CoordSys1B = 5225,
        /// coord. system 1 C 5226
        CoordSys1C = 5226,

        CoordSys2X = 5241,         //coord. system 2 X
        CoordSys2Y = 5242,         //coord. system 2 Y
        CoordSys2Z = 5243,         //coord. system 2 Z
        CoordSys2A = 5244,         //coord. system 2 A
        CoordSys2B = 5245,         //coord. system 2 B
        CoordSys2C = 5246,         //coord. system 2 C

        CoordSys3X = 5261,         //coord. system 3 X
        CoordSys3Y = 5262,         //coord. system 3 Y
        CoordSys3Z = 5263,         //coord. system 3 Z
        CoordSys3A = 5264,         //coord. system 3 A
        CoordSys3B = 5265,         //coord. system 3 B
        CoordSys3C = 5266,         //coord. system 3 C

        CoordSys4X = 5281, //coord. system 4 X
        CoordSys4Y = 5282, //coord. system 4 Y
        CoordSys4Z = 5283, //coord. system 4 Z
        CoordSys4A = 5284, //coord. system 4 A
        CoordSys4B = 5285, //coord. system 4 B
        CoordSys4C = 5286, //coord. system 4 C
        CoordSys5X = 5301, //coord. system 5 X
        CoordSys5Y = 5302, //coord. system 5 Y
        CoordSys5Z = 5303, //coord. system 5 Z
        CoordSys5A = 5304, //coord. system 5 A
        CoordSys5B = 5305, //coord. system 5 B
        CoordSys5C = 5306, //coord. system 5 C
        CoordSys6X = 5321, //coord. system 6 X
        CoordSys6Y = 5322, //coord. system 6 Y
        CoordSys6Z = 5323, //coord. system 6 Z
        CoordSys6A = 5324, //coord. system 6 A
        CoordSys6B = 5325, //coord. system 6 B
        CoordSys6C = 5326, //coord. system 6 C
        CoordSys7X = 5341, //coord. system 7 X
        CoordSys7Y = 5342, //coord. system 7 Y
        CoordSys7Z = 5343, //coord. system 7 Z
        CoordSys7A = 5344, //coord. system 7 A
        CoordSys7B = 5345, //coord. system 7 B
        CoordSys7C = 5346, //coord. system 7 C
        CoordSys8X = 5361, //coord. system 8 X
        CoordSys8Y = 5362, //coord. system 8 Y
        CoordSys8Z = 5363, //coord. system 8 Z
        CoordSys8A = 5364, //coord. system 8 A
        CoordSys8B = 5365, //coord. system 8 B
        CoordSys8C = 5366, //coord. system 8 C
        CoordSys9X = 5381, //coord. system 9 X
        CoordSys9Y = 5382, //coord. system 9 Y
        CoordSys9Z = 5383, //coord. system 9 Z
        CoordSys9A = 5384, //coord. system 9 A
        CoordSys9B = 5385, //coord. system 9 B
        CoordSys9C = 5386, //coord. system 9 C
    }
}

impl FieldIndex {
    /// get related paraindex and base-index info
    ///
    pub fn info(&self) -> (ParaIndex, i32) {
        match self {
            &FieldIndex::ProbePosA
            | &FieldIndex::ProbePosB
            | &FieldIndex::ProbePosC
            | &FieldIndex::ProbePosX
            | &FieldIndex::ProbePosY
            | &FieldIndex::ProbePosZ => {
                return (
                    ParaIndex::ProbePos,
                    *self as i32 - FieldIndex::ProbePosX as i32,
                )
            }

            &FieldIndex::PROBE_ANALOG_VALUE => return (ParaIndex::ProbeAnalogValue, 0),
            &FieldIndex::COORD_SYS_SELECTED => return (ParaIndex::CoordSysSelected, 0),

            &FieldIndex::G28HA
            | &FieldIndex::G28HB
            | &FieldIndex::G28HC
            | &FieldIndex::G28HX
            | &FieldIndex::G28HY
            | &FieldIndex::G28HZ => {
                return (ParaIndex::G28Home, *self as i32 - FieldIndex::G28HX as i32)
            }

            &FieldIndex::G30HA
            | &FieldIndex::G30HB
            | &FieldIndex::G30HC
            | &FieldIndex::G30HX
            | &FieldIndex::G30HY
            | &FieldIndex::G30HZ => {
                return (ParaIndex::G30Home, *self as i32 - FieldIndex::G30HX as i32)
            }

            &FieldIndex::G92_OFST_A
            | &FieldIndex::G92_OFST_B
            | &FieldIndex::G92_OFST_C
            | &FieldIndex::G92_OFST_X
            | &FieldIndex::G92_OFST_Y
            | &FieldIndex::G92_OFST_Z => {
                return (
                    ParaIndex::G92Offset,
                    *self as i32 - FieldIndex::G92_OFST_X as i32,
                )
            }

            &FieldIndex::CoordSys1A
            | &FieldIndex::CoordSys1B
            | &FieldIndex::CoordSys1C
            | &FieldIndex::CoordSys1X
            | &FieldIndex::CoordSys1Y
            | &FieldIndex::CoordSys1Z => {
                return (
                    ParaIndex::CoordSys1,
                    *self as i32 - FieldIndex::CoordSys1X as i32,
                )
            }

            &FieldIndex::CoordSys2A
            | &FieldIndex::CoordSys2B
            | &FieldIndex::CoordSys2C
            | &FieldIndex::CoordSys2X
            | &FieldIndex::CoordSys2Y
            | &FieldIndex::CoordSys2Z => {
                return (
                    ParaIndex::CoordSys2,
                    *self as i32 - FieldIndex::CoordSys2X as i32,
                )
            }

            &FieldIndex::CoordSys3A
            | &FieldIndex::CoordSys3B
            | &FieldIndex::CoordSys3C
            | &FieldIndex::CoordSys3X
            | &FieldIndex::CoordSys3Y
            | &FieldIndex::CoordSys3Z => {
                return (
                    ParaIndex::CoordSys3,
                    *self as i32 - FieldIndex::CoordSys3X as i32,
                )
            }

            &FieldIndex::CoordSys4A
            | &FieldIndex::CoordSys4B
            | &FieldIndex::CoordSys4C
            | &FieldIndex::CoordSys4X
            | &FieldIndex::CoordSys4Y
            | &FieldIndex::CoordSys4Z => {
                return (
                    ParaIndex::CoordSys4,
                    *self as i32 - FieldIndex::CoordSys4X as i32,
                )
            }

            &FieldIndex::CoordSys5A
            | &FieldIndex::CoordSys5B
            | &FieldIndex::CoordSys5C
            | &FieldIndex::CoordSys5X
            | &FieldIndex::CoordSys5Y
            | &FieldIndex::CoordSys5Z => {
                return (
                    ParaIndex::CoordSys5,
                    *self as i32 - FieldIndex::CoordSys5X as i32,
                )
            }

            &FieldIndex::CoordSys6A
            | &FieldIndex::CoordSys6B
            | &FieldIndex::CoordSys6C
            | &FieldIndex::CoordSys6X
            | &FieldIndex::CoordSys6Y
            | &FieldIndex::CoordSys6Z => {
                return (
                    ParaIndex::CoordSys6,
                    *self as i32 - FieldIndex::CoordSys6X as i32,
                )
            }

            &FieldIndex::CoordSys7A
            | &FieldIndex::CoordSys7B
            | &FieldIndex::CoordSys7C
            | &FieldIndex::CoordSys7X
            | &FieldIndex::CoordSys7Y
            | &FieldIndex::CoordSys7Z => {
                return (
                    ParaIndex::CoordSys7,
                    *self as i32 - FieldIndex::CoordSys7X as i32,
                )
            }

            &FieldIndex::CoordSys8A
            | &FieldIndex::CoordSys8B
            | &FieldIndex::CoordSys8C
            | &FieldIndex::CoordSys8X
            | &FieldIndex::CoordSys8Y
            | &FieldIndex::CoordSys8Z => {
                return (
                    ParaIndex::CoordSys8,
                    *self as i32 - FieldIndex::CoordSys8X as i32,
                )
            }
            &FieldIndex::CoordSys9A
            | &FieldIndex::CoordSys9B
            | &FieldIndex::CoordSys9C
            | &FieldIndex::CoordSys9X
            | &FieldIndex::CoordSys9Y
            | &FieldIndex::CoordSys9Z => {
                return (
                    ParaIndex::CoordSys9,
                    *self as i32 - FieldIndex::CoordSys9X as i32,
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Argument, FieldIndex, Paramters, Position};

    #[test]
    fn stack_new() {
        let mut _v: Paramters = Paramters::new();
    }

    #[test]
    fn test_paramers_update() {
        let mut store: Paramters = Paramters::new();
        assert_eq!(true, store.0.is_empty());

        let p = Position::new(1., 0., 0., 0., 0., 0.);
        let v = Argument::CoordSys1(p);
        store.update_item(&v);

        assert_eq!(false, store.0.is_empty());
        let r = store.get_field(FieldIndex::CoordSys1X);
        assert_eq!(r, Some(1.));

        let p = Position { y: 10., ..p };
        let v = Argument::CoordSys1(p);
        store.update_item(&v);
        let r = store.get_field(FieldIndex::CoordSys1Y);
        assert_eq!(r, Some(10.));
    }
    #[test]
    fn test_paramers_get() {
        let mut store: Paramters = Paramters::new();

        let p = Position::new(1., 0., 0., 0., 0., 0.);
        let v = Argument::CoordSys1(p);

        store.update_item(&v);

        let r = store.get_field(FieldIndex::CoordSys1X);
        assert_eq!(r, Some(1.));
    }

    // #[test]
    // fn test_line2d_example() {

    // }
}

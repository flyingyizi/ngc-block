use super::exp;

use crate::{BTreeMap,String,
    // vec::Vec
};
use exp::{
    eval_l0val,
    helper_par_assign,
    parse_to_l0vals,
    remove_ngc_comment,
    NgcWord,
    RefParsUtilTrait,
};

use super::codes::{GCodes, GGroup, MCodes, MGroup};
use crate::error::*;
#[allow(unused_imports)]
use num_traits::Float;

type GModalMap = BTreeMap<GGroup, GCodes>;
type MModalMap = BTreeMap<MGroup, MCodes>;

/// distance_mode, include absolute and incremental
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DistanceMode {
    /// value is the destination value
    Absolute,
    /// destination value is current value plus the incremental
    Incremental,
}

/// it represent & store analyzed information from one statement line.
///
/// specification: `[reference\RS274NGC_3.pdf]`
///
/// # example
/// ```text
/// use ngc_block::{Block,GCodes,GGroup};
/// let mut pars = TestSettings::new();
/// pars.0.insert(5221, 2.0);
/// pars.0.insert(5222, 3.0);
/// // case 1
/// let mut cmd = ("N110 G0 X#5221 Y#5222 (move above nominal hole center)").to_string();
/// let mut block = Block::new();
/// assert_eq!(block.read_items(&mut pars, &mut cmd), Ok(()));
/// assert_eq!(
///     block.get_comment(),
///     Some("(move above nominal hole center)")
/// );
/// assert_eq!(Some(110), block.n_number);
/// assert_eq!(Some(2.), block.x_number);
/// assert_eq!(Some(3.), block.y_number);
/// assert_eq!(block.g_modes.get(&GGroup::GCodeMotion), Some(&GCodes::G0));
/// ```
///
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

    /// parse one statement to fullfill self's content.
    ///
    /// notice: only when it reutrn Ok, the fields value is valid.
    pub fn parse(
        &mut self,
        blocktext: &mut String,

        pars: &mut impl RefParsUtilTrait,
        last_saved_motion: &Option<GCodes>,
        last_saved_distance_mode: &DistanceMode,
    ) -> Result<(), BlockError> {
        if blocktext.len() > 0 {
            //parse line
            if let Err(t) = self.read_items(pars, blocktext) {
                return Err(t);
            }
            if let Err(t) = self.enhance_block(last_saved_motion) {
                return Err(t);
            }
            if let Err(t) = self.check_items(last_saved_distance_mode) {
                return Err(t);
            }
            return Ok(());
        }
        Err(BlockError::NceSscanfFailed)
    }

    /// read one(only one) statement line, parsed result stored in self
    pub fn read_items(
        &mut self,
        pars: &mut impl RefParsUtilTrait,
        blocktext: &mut String,
    ) -> Result<(), BlockError> {
        if let Ok(comment) = remove_ngc_comment(blocktext) {
            self.comment = comment;
        } else {
            return Err(BlockError::NceUnclosedCommentFound);
        }

        let str = blocktext.as_mut_str();
        str.make_ascii_lowercase();

        let input = if let Ok(raw) = parse_to_l0vals(str) {
            raw
        } else {
            return Err(BlockError::NceBadFormat);
        };

        //check duplicate
        let mut dups = BTreeMap::<char, f32>::new();

        for v in input {
            match v {
                NgcWord::Word((w, v)) => {
                    if let Some(val) = eval_l0val(v, pars) {
                        if dups.insert(w, val).is_some() {
                            return Err(BlockError::NceMultipleWordsOnOneLine);
                        }
                    } else {
                        return Err(BlockError::NceSscanfFailed);
                    }
                }
                NgcWord::ParAssign(v) => {
                    if let Some((key, val)) = helper_par_assign(&v, pars) {
                        pars.set_params(key, val);
                    } else {
                        return Err(BlockError::NceSscanfFailed);
                    }
                }
            }
        }

        for (key, val) in dups {
            match key {
                'j' => self.j_number = Some(val),
                'z' => self.z_number = Some(val),
                'y' => self.y_number = Some(val),
                'x' => self.x_number = Some(val),
                't' => self.t_number = Some(val as i32),
                's' => self.s_number = Some(val),
                'r' => self.r_number = Some(val),
                'q' => {
                    if val <= 0.0 {
                        return Err(BlockError::NceNegativeOrZeroQValueUsed);
                    }
                    self.q_number = Some(val);
                }
                'p' => {
                    if val <= 0.0 {
                        return Err(BlockError::NceNegativePWordUsed);
                    }
                    self.p_number = Some(val);
                }
                'n' => {
                    let val = val as u32;
                    if val > 99999 {
                        return Err(BlockError::NceLineNumberGreaterThan99999);
                    }
                    self.n_number = Some(val);
                }
                'm' => {
                    let val = val as i32;
                    if val < 0 {
                        return Err(BlockError::NceNegativeMCodeUsed);
                    } else if val > 99 {
                        return Err(BlockError::NceMCodeGreaterThan99);
                    }

                    let v: Result<MCodes, _> = val.try_into();
                    match v {
                        Ok(code) => {
                            let mode = code.to_modal();
                            if let Some(_) = self.m_modes.get(&mode) {
                                return Err(BlockError::NceTwoMCodesUsedFromSameModalGroup);
                            }

                            self.m_modes.insert(mode, code.clone());
                        }
                        Err(_) => return Err(BlockError::NceMCODEOutOfRange),
                    }
                }
                'l' => {
                    let val = val as i32;
                    if val <= 0 {
                        return Err(BlockError::NceNegativeLWordUsed);
                    }
                    self.l_number = Some(val);
                }
                'k' => self.k_number = Some(val),
                'i' => self.i_number = Some(val),
                'h' => {
                    let val = val as i32;
                    if val <= 0 {
                        return Err(BlockError::NceNegativeHWordToolLengthOffsetIndexUsed);
                    }
                    self.h_number = Some(val);
                }
                'g' => {
                    let value_read = val * 10.0;
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
                            self.g_modes.insert(mode, code);
                        }
                        Err(_) => return Err(BlockError::NceGCODEOutOfRange),
                    }
                }
                'f' => {
                    if val <= 0. {
                        return Err(BlockError::NceNegativeFWordUsed);
                    }
                    self.f_number = Some(val);
                }
                'd' => {
                    let val = val as i32;
                    if val <= 0 {
                        return Err(BlockError::NceNegativeFWordUsed);
                    }
                    self.d_number = Some(val);
                }
                'c' => self.c_number = Some(val),
                'b' => self.b_number = Some(val),
                'a' => self.a_number = Some(val),
                _ => unreachable!(),
            }
        }

        return Ok(());
    }

    pub fn get_comment(&self) -> Option<&str> {
        if let Some(s) = &self.comment {
            return Some(s.as_str());
        }

        return None;
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
    pub fn enhance_block(&mut self, motion_mode: &Option<GCodes>) -> Result<(), BlockError> {
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
                    return Err(BlockError::NceAllAxesMissingWithG92);
                }
            } else if axis_flag {
                if motion_mode.is_none() || motion_mode == &Some(GCodes::G80) {
                    return Err(BlockError::NceCannotUseAxisValuesWithoutAGcodeThatUsesThem);
                }

                self.motion_to_be = motion_mode.clone();
            }
            return Ok(());
        }
        ///////////////////////////////////////////
        if motion_code == Some(&GCodes::G80) {
            if axis_flag && !mode_zero_covets_axes {
                return Err(BlockError::NceCannotUseAxisValuesWithG80);
            }

            if !axis_flag && misc_code == Some(&GCodes::G92) {
                return Err(BlockError::NceAllAxesMissingWithG92);
            }
        } else {
            if mode_zero_covets_axes {
                return Err(BlockError::NceCannotUseTwoGCodethatBothUseAxisValues);
            }

            if !axis_flag {
                return Err(BlockError::NceAllAxesMissingWithMotionCode);
            }
        }

        self.motion_to_be = Some(motion_code.unwrap().clone());

        return Ok(());
    }

    pub fn check_items(&mut self, distance_mode: &DistanceMode) -> Result<(), BlockError> {
        let err = self._check_g_codes(distance_mode);
        if err.is_err() {
            return err;
        }
        let err = self._check_m_codes();
        if err.is_err() {
            return err;
        }
        let err = self._check_other_codes();
        if err.is_err() {
            return err;
        }
        return Ok(());
    }

    fn _check_m_codes(&mut self) -> Result<(), BlockError> {
        // max number of m codes on one line
        const MAX_EMS: usize = 4;

        if self.m_modes.len() > MAX_EMS {
            return Err(BlockError::NceTooManyMCodesOnLine);
        }
        Ok(())
    }
    fn _check_g_codes(&self, distance_mode: &DistanceMode) -> Result<(), BlockError> {
        match self.g_modes.get(&GGroup::GCodeMisc) {
            Some(&GCodes::G4) => {
                if self.p_number.is_none() {
                    return Err(BlockError::NceDwellTimeMissingWithG4);
                }
            }
            Some(&GCodes::G10) => {
                if let Some(p) = self.p_number {
                    let p_int = (p + 0.0001) as i32;
                    if (p + 0.0001) - p_int as f32 > 0.0002 {
                        return Err(BlockError::NcePValueNotAnIntegerWithG10L2);
                    }
                    if p_int < 1 || p_int > 9 {
                        return Err(BlockError::NcePValueOutOfRangeWithG10L2);
                    }
                }
                if self.l_number != Some(2) {
                    return Err(BlockError::NceLineWithG10DoesNotHavel2);
                };
            }
            Some(&GCodes::G28) | Some(&GCodes::G30) | Some(&GCodes::G92) | Some(&GCodes::G92_1)
            | Some(&GCodes::G92_2) | Some(&GCodes::G92_3) => {}
            Some(&GCodes::G53) => {
                if self.motion_to_be != Some(GCodes::G0) && self.motion_to_be != Some(GCodes::G1) {
                    return Err(BlockError::NceMustUseG0OrG1WithG53);
                }

                let dis_code = self.g_modes.get(&GGroup::GCodeDistance);
                if dis_code == Some(&GCodes::G91)
                    || (dis_code == Some(&GCodes::G90)
                        && distance_mode == &DistanceMode::Incremental)
                {
                    return Err(BlockError::NceCannotUseG53Incremental);
                }
            }
            None => return Ok(()),
            _ => {
                return Err(BlockError::NceBugBadGCodeModalGroup0);
            }
        }

        Ok(())
    }
    fn _check_other_codes(&self) -> Result<(), BlockError> {
        if self.a_number.is_some() || self.b_number.is_some() || self.c_number.is_some() {
            if let Some(gcode) = self.g_modes.get(&GGroup::GCodeMotion) {
                if gcode > &GCodes::G80 && gcode < &GCodes::G90 {
                    return Err(BlockError::NceCannotPutAnAInCannedCycle);
                }
            }
        }

        if self.d_number.is_some() {
            if let Some(gcode) = self.g_modes.get(&GGroup::GCodeCutterRadiusCompensation) {
                if gcode != &GCodes::G41 && gcode != &GCodes::G42 {
                    return Err(BlockError::NceDWordWithNoG41OrG42);
                }
            }
        }

        if self.h_number.is_some() {
            if let Some(gcode) = self.g_modes.get(&GGroup::GCodeToolLengthOffset) {
                if gcode != &GCodes::G43 {
                    return Err(BlockError::NceHWordWithNoG43);
                }
            }
        }

        /* could still be useless if yz_plane arc */
        if self.i_number.is_some() {
            if self.motion_to_be != Some(GCodes::G2)
                && self.motion_to_be != Some(GCodes::G3)
                && self.motion_to_be != Some(GCodes::G87)
            {
                return Err(BlockError::NceIWordWithNoG2OrG3OrG87ToUseIT);
            }
        }

        /* could still be useless if xz_plane arc */
        if self.j_number.is_some() {
            if self.motion_to_be != Some(GCodes::G2)
                && self.motion_to_be != Some(GCodes::G3)
                && self.motion_to_be != Some(GCodes::G87)
            {
                return Err(BlockError::NceJWordWithNoG2OrG3OrG87ToUseIT);
            }
        }

        /* could still be useless if xy_plane arc */
        if self.k_number.is_some() {
            if self.motion_to_be != Some(GCodes::G2)
                && self.motion_to_be != Some(GCodes::G3)
                && self.motion_to_be != Some(GCodes::G87)
            {
                return Err(BlockError::NceKWordWithNoG2OrG3OrG87ToUseIT);
            }
        }

        if self.l_number.is_some() {
            if let Some(motion) = self.motion_to_be {
                if let Some(gcode) = self.g_modes.get(&GGroup::GCodeMisc) {
                    if gcode != &GCodes::G10 && (motion < GCodes::G81 || motion > GCodes::G89) {
                        return Err(BlockError::NceLWordWithNoCannedCycleOrG10);
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
                        return Err(BlockError::NcePWordwithNoG4G10G82G86G88G89);
                    }
                }
            }
        }

        if self.q_number.is_some() {
            if let Some(motion) = self.motion_to_be {
                if motion != GCodes::G83 {
                    return Err(BlockError::NceQWordWithNoG83);
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
                    return Err(BlockError::NceRwordWithNoGCodeThatUseIT);
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ToString;

    struct TestSettings(BTreeMap<i32, f32>);
    impl TestSettings {
        pub fn new() -> Self {
            Self(BTreeMap::<i32, f32>::new())
        }
    }

    impl RefParsUtilTrait for TestSettings {
        fn get_params(&self, key: i32) -> Option<f32> {
            if let Some(v) = self.0.get(&key) {
                Some(v.clone())
            } else {
                None
            }
        }
        fn set_params(&mut self, key: i32, val: f32) {
            self.0.insert(key, val);
        }
    }

    #[test]
    fn check_read_items() {
        let mut pars = TestSettings::new();
        pars.0.insert(5221, 2.0);
        pars.0.insert(5222, 3.0);

        // case 1
        let mut cmd = ("N110 G0 X#5221 Y#5222 (move above nominal hole center)").to_string();
        let mut block = Block::new();

        assert_eq!(block.read_items(&mut pars, &mut cmd), Ok(()));

        assert_eq!(
            block.get_comment(),
            Some("(move above nominal hole center)")
        );
        assert_eq!(Some(110), block.n_number);
        assert_eq!(Some(2.), block.x_number);
        assert_eq!(Some(3.), block.y_number);
        assert_eq!(block.g_modes.get(&GGroup::GCodeMotion), Some(&GCodes::G0));
    }

    #[test]
    fn check_parameters_set_in_parallel() {
        let mut pars = TestSettings::new();
        pars.0.insert(5221, 2.0);
        pars.0.insert(5222, 3.0);

        // case 2 check parameters set in parallel
        let mut cmd =
                String::from("N111 G38.2 #5226=14 #5222=3 #5221=1 X[#5226 - #5221] Y#5222 (move above nominal hole center)").to_string();
        let mut block = Block::new();
        //parse line
        assert_eq!(block.read_items(&mut pars, &mut cmd), Ok(()));
        assert_eq!(
            block.get_comment(),
            Some("(move above nominal hole center)")
        );
        assert_eq!(Some(111), block.n_number);
        assert_eq!(Some(13.), block.x_number);
        assert_eq!(Some(3.), block.y_number);
        assert_eq!(
            block.g_modes.get(&GGroup::GCodeMotion),
            Some(&GCodes::G38_2)
        );
    }
}

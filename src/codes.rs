macro_rules! back_to_enum {
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

back_to_enum! {
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
pub enum GCodes {
    ///G0--rapid positioning
    G0 = 0,
    ///G01---直线插补 Linear interpolation at programmed feedrate
    ///
    /// ```cnc
    /// e.g. P1(40,20,30), P2(60,60,40), P3(120,80,90)
    /// ;Absolute dimensional input:
    /// N10 G01 G90 X60 Y60 U40 F1000 ;move from P1 to P2 feedrate 1000 mm/min)
    /// N20 X120 Y80 U90 ;move from P2 to P3 feedrate 1000 mm/min)
    /// ```
    G1 = 10,

    ///G02---顺时针方向圆弧插补 G2 circular/helical interpolation (clockwise)
    /// When G02 or G03 is selected, the programmed path is travelled to the target point in circular
    /// motion at the feedrate specified in the F word. Circular motion can be travelled in the three main
    /// planes of the spatial coordinate system (X-Y, Z-X, Y-Z). The main plane is selected using the functions G17, G18, G19
    ///
    /// The circle is defined by taking the starting point, the target point of the circle and the centre point of the circle
    /// . The centre point of the circle is specified by the interpolation parameters I, J, K
    /// relative to the starting point of the circle when G91_1 is active or absolute when G90_1 is active.
    ///
    /// Syntax according to selected interpolation plane:
    /// ```text
    /// |Plane|Interpolation type|Target point in plane| Centre point/radius|
    /// |-----|------------------|---------------------|-----------|
    /// |G17  |G02/G03           |X..Y..               |I..J../R   |
    /// |G18  |G02/G03           |Z..X..               |K..I../R   |
    /// |G19  |G02/G03           |Y..Z..               |J..K../R   |
    /// ```
    G2 = 20,
    ///G03---逆时针方向圆弧插补 G3 circular/helical interpolation (counterclockwise)
    G3 = 30,
    ///G04---定时暂停  G4 dwell.
    /// G4 P_  p为暂停时间，单位秒
    G4 = 40,

    /// G10 coordinate system origin setting
    /// If a g10 is encountered,should to reset the offsets of the program
    /// coordinate system indicated by the P number given in the same block.
    /// Example: G10 L2 P1 x 3.5 y 17.2 sets the origin of the first coordinate system (the one selected
    /// by G54) to a point where X is 3.5 and Y is 17.2 (in absolute coordinates). The Z coordinate of the
    /// origin (and the coordinates for any rotational axes) are whatever those coordinates of the origin
    /// were before the line was executed.
    G10 = 100,
    /// G17 (Zaxis, XY-plane).XY-plane selection
    G17 = 170,
    ///G18 (Y-axis, XZ-plane) XZ-plane selection
    G18 = 180,
    ///G19 (X-axis, YZ-plane) YZ-plane selection
    G19 = 190,
    /// G20 to use inches for length units
    G20 = 200,
    /// G21 to use millimeters
    G21 = 210,
    /// G28: return to home, home positon defined by parameters 5161-5166
    G28 = 280,
    /// G30: return to secondary home, home positon defined by parameters 5181-5186
    G30 = 300,

    ///
    /// Program G38.2 X… Y… Z… A… B… C… to perform a straight probe operation. The rotational
    /// axis words are allowed, but it is better to omit them. If rotational axis words are used, the numbers
    /// must be the same as the current position numbers so that the rotational axes do not move. The
    /// linear axis words are optional, except that at least one of them must be used. The tool in the
    /// spindle must be a probe.
    ///
    G38_2 = 382,
    /// G40-cancel cutter radius compensation, 取消刀具补偿
    G40 = 400,
    /// G41-start cutter radius compensation left. -刀具补偿——左,即相对刀具前进方向的左侧补偿。
    G41 = 410,
    /// G42-start cutter radius compensation right. -刀具补偿——右
    G42 = 420,
    /// G43--tool length offset (plus)
    G43 = 430,
    /// G49--cancel tool length offset
    G49 = 490,

    /// G53 motion in machine coordinate system
    ///
    /// For linear motion to a point expressed in absolute coordinates, program G1 G53 X… Y… Z…
    /// A… B… C… (or use G0 instead of G1), where all the axis words are optional, except that at least
    /// one must be used. The G0 or G1 is optional if it is the current motion mode. G53 is not modal and
    /// must be programmed on each line on which it is intended to be active. This will produce
    /// coordinated linear motion to the programmed point. If G1 is active, the speed of motion is the
    /// current feed rate (or slower if the machine will not go that fast). If G0 is active, the speed of
    /// motion is the current traverse rate (or slower if the machine will not go that fast).
    /// It is an error if:
    /// • G53 is used without G0 or G1 being active,
    /// • G53 is used while cutter radius compensation is on.
    G53 = 530,

    ///G54------直线偏移x G54 use preset work coordinate system 1
    G54 = 540,
    ///G55------直线偏移y G55 use preset work coordinate system 2
    G55 = 550,
    ///G56------直线偏移z G56 use preset work coordinate system 3
    G56 = 560,
    ///G57------直线偏移xy G57 use preset work coordinate system 4
    G57 = 570,
    ///G58------直线偏移xz G58 use preset work coordinate system 5
    G58 = 580,
    ///G59------直线偏移yz G59 use preset work coordinate system 6
    G59 = 590,
    ///G59.1 use preset work coordinate system 7
    G59_1 = 591,
    ///G59.2 use preset work coordinate system 8
    G59_2 = 592,
    ///G59.3 use preset work coordinate system 9
    G59_3 = 593,
    ///准确路径方式（中）G61 set path control mode:exact path
    G61 = 610,
    /// G61.1 set path control     mode: exact stop
    G61_1 = 611,
    ///G64 set path control mode: continuous
    G64 = 640,
    /// 自动切削循环取消 cancel motion mode (including any canned cycle)
    G80 = 800,
    /// 钻孔循环 drilling cycle
    G81 = 810,
    /// 钻孔循环（带暂停）G82 canned cycle: drilling with dwell
    G82 = 820,
    ///G83 canned cycle: peck drilling
    G83 = 830,
    ///G84 canned cycle: right hand tapping 攻螺纹循环指令
    G84 = 840,
    ///G85 canned cycle: boring, no dwell, feed out
    G85 = 850,
    ///G86 canned cycle: boring, spindle stop, rapid out
    G86 = 860,
    ///G87 canned cycle: back boring
    G87 = 870,
    ///G88 canned cycle: boring, spindle stop, manual out
    G88 = 880,
    ///G89 canned cycle: boring, dwell, feed out
    G89 = 890,

    ///G90------绝对尺寸 G90 absolute distance mode.
    /// With an absolute dimensional input (G90), all coordinate specifications are based on the coordinate origin
    /// ```cnc
    /// e.g. P1(40,20,30), P2(120,80,90)
    /// ;Absolute dimensional input:
    /// N10 G00 G90 X120 Y80 U90      ;move from P1 to P2
    /// ```
    G90 = 900,

    ///G90.1------arc ijk使用绝对尺寸
    G90_1 = 901,
    ///G91------相对尺寸G91 incremental distance mode.
    /// With incremental programming (G91), the coordinate values are based on the target point of the preceding motion block,
    /// ```cnc
    /// e.g. P1(40,20,30), P2(120,80,90)
    /// ;Incremental dimensional input:
    /// N10 G00 G91 X80 Y60 U60       ;move from P1 to P2
    /// ```
    G91 = 910,
    ///G91.1------arc ijk使用相对尺寸
    G91_1 = 911,

    ///G92------预制坐标G92 offset coordinate systems and set parameters
    /// To make the current point have the coordinates you want (without motion),
    ///
    /// program G92 X… Y… Z… A… B… C… , where the axis words contain the axis numbers
    /// you want. All axis words are optional,
    G92 = 920,

    ///G92.1 cancel offset coordinate systems and set parameters to zero
    G92_1 = 921,
    ///G92.2 cancel offset coordinate systems but do not reset parameters
    G92_2 = 922,
    ///G92.3 apply parameters to offset coordinate systems
    G92_3 = 923,
    ///G93------时间倒数，进给率G93 inverse time feed rate mode
    G93 = 930,
    ///G94------进给率，每分钟进给G94 units per minute feed rate mode
    G94 = 940,
    G98 = 980,
    G99 = 990,

    /// Constant Surface Speed。
    /// G96 S_ s为 切削的恒线速度m/min.
    G96 = 960,

    /// RPM Mode (default)
    /// G97 S_ s为取消恒线速度后指定的主轴转速r/min.
    G97 = 970,
    //G98 返回初始平面
    //G99 返回R点平面
}
}

impl GCodes {
    pub fn to_modal(&self) -> GGroup {
        match self {
            &GCodes::G4
            | &GCodes::G10
            | &GCodes::G28
            | &GCodes::G30
            | &GCodes::G53
            | &GCodes::G92
            | &GCodes::G92_1
            | &GCodes::G92_2
            | &GCodes::G92_3 => GGroup::GCodeMisc,

            &GCodes::G0
            | &GCodes::G1
            | &GCodes::G2
            | &GCodes::G3
            | &GCodes::G38_2
            | &GCodes::G80
            | &GCodes::G81
            | &GCodes::G82
            | &GCodes::G83
            | &GCodes::G84
            | &GCodes::G85
            | &GCodes::G86
            | &GCodes::G87
            | &GCodes::G88
            | &GCodes::G89 => GGroup::GCodeMotion,

            &GCodes::G17 | &GCodes::G18 | &GCodes::G19 => GGroup::GCodePlaneSelection,
            &GCodes::G90 | &GCodes::G91 => GGroup::GCodeDistance,
            &GCodes::G90_1 | &GCodes::G91_1 => GGroup::GCodeArcDistance,
            &GCodes::G93 | &GCodes::G94 => GGroup::GCodeFeedRateMode,
            &GCodes::G20 | &GCodes::G21 => GGroup::GCodeUnit,
            &GCodes::G40 | &GCodes::G41 | &GCodes::G42 => GGroup::GCodeCutterRadiusCompensation,
            &GCodes::G43 | &GCodes::G49 => GGroup::GCodeToolLengthOffset,
            &GCodes::G98 | &GCodes::G99 => GGroup::GCodeReturnModeInCannedCycle,
            &GCodes::G54
            | &GCodes::G55
            | &GCodes::G56
            | &GCodes::G57
            | &GCodes::G58
            | &GCodes::G59
            | &GCodes::G59_1
            | &GCodes::G59_2
            | &GCodes::G59_3 => GGroup::GCodeCoordinateSystem,
            // group 13 - gez[11] g61, g61.1, g64 - control mode
            &GCodes::G61 | &GCodes::G61_1 | &GCodes::G64 => GGroup::GCodeControlMode,

            &GCodes::G96 | &GCodes::G97 => GGroup::GcodeSpindleSpeedMode,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]

pub enum GGroup {
    /// {g4, g10, g28, g30, g53, g92 g92.1, g92.2, g92.3} - misc
    GCodeMisc = 0,
    /// {g0, g1, g2, g3, g38.2, g80, g81, g82, g83, g84, g85,g86, g87, g88, g89} - motion
    GCodeMotion = 1,
    /// {g17, g18, g19} - plane selection
    GCodePlaneSelection = 2,
    /// {g90, g91} - distance mode
    GCodeDistance = 3,
    /// {g90.1,g91.1}   - arc distance mode
    GCodeArcDistance = 4,
    /// {g93, g94} - feed rate mode
    GCodeFeedRateMode = 5,
    /// {g20, g21} - units
    GCodeUnit = 6,
    /// {g40, g41, g42} - cutter radius compensation
    GCodeCutterRadiusCompensation = 7,
    /// {g43, g49} - tool length offset
    GCodeToolLengthOffset = 8,
    /// {g98, g99} - return mode in canned cycles
    GCodeReturnModeInCannedCycle = 10,
    /// {g54, g55, g56, g57, g58, g59, g59.1, g59.2, g59.3} - coordinate system
    GCodeCoordinateSystem = 12,
    /// {g61, g61.1, g64} - control mode
    GCodeControlMode = 13,

    /// {G96, G97} Spindle Speed Mode
    GcodeSpindleSpeedMode = 14,
    // // num of gcode modal group
    // GModalGroupLen = 15,
}

back_to_enum! {
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MCodes {
    /// M0 program stop
    M0 = 0,
    /// M1 optional program stop
    M1 = 1,
    ///M2 program end
    M2 = 2,
    ///M3 turn spindle clockwise(cw)
    M3 = 3,
    ///M4 turn spindle counterclockwise(ccw)
    M4 = 4,
    ///M5 stop spindle turning
    M5 = 5,
    ///M6 tool change
    M6 = 6,
    ///M7 mist coolant on
    M7 = 7,
    ///M8 flood coolant on
    M8 = 8,
    ///M9 mist and flood coolant off
    M9 = 9,
    /// M26	Tool unclamp
    M26 = 26,
    /// M27	Clutch neutral ON
    M27 = 27,
    ///M30 program end, pallet shuttle, and reset
    M30 = 30,
    ///M48 enable speed and feed overrides
    M48 = 48,
    ///M49 disable speed and feed overrides
    M49 = 49,
    ///M60 pallet shuttle and program stop
    M60 = 60,
    // M19	Spindle orientation ON
    // M21	Tool
    // M22	Tool
    // M23	Tool
    // M24	Tool
    // M25	Tool clamp
    // M28	Clutch neutral OFF
    // M98	Call sub-program
    // M99	End sub-program
}
}

impl MCodes {
    pub fn to_modal(&self) -> MGroup {
        match self {
            &MCodes::M0 | &MCodes::M1 | &MCodes::M2 | &MCodes::M30 | &MCodes::M60 => {
                MGroup::MCodeStoping
            }
            &MCodes::M3 | &MCodes::M4 | &MCodes::M5 => MGroup::MCodeSpindleTurning,
            &MCodes::M7 | &MCodes::M8 | &MCodes::M9 => MGroup::MCodeCoolant,
            &MCodes::M48 | &MCodes::M49 => MGroup::MCodeSpeedOverrideSwitch,
            &MCodes::M6 | &MCodes::M26 | &MCodes::M27 => MGroup::MCodeToolChange,
        }
    }
}
// impl hash32::Hash for MCodes {
//     fn hash<H>(&self, state: &mut H)
//     where
//         H: hash32::Hasher,
//     {
//         (*self as i32).hash(state)
//     }
// }

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum MGroup {
    /// {m0,m1,m2,m30,m60} - stopping
    MCodeStoping = 4,
    /// {m6}               - tool change
    MCodeToolChange = 6,
    /// {m3,m4,m5}         - spindle turning
    MCodeSpindleTurning = 7,
    /// {m7,m8,m9}         - coolant
    MCodeCoolant = 8,
    /// {m48,m49}          - feed and speed override switch bypass
    MCodeSpeedOverrideSwitch = 9,
    // MModalGroupM_max,
}

use ngc_block::{Block, DistanceMode, GCodes, GGroup};

fn main() {
    if let Some(mut pars) = helper::Pars::import_from_file(".\\examples\\testdata\\rs274ngc.var") {
        pars.0.insert(5221, 2.0);
        pars.0.insert(5222, 3.0);

        // case 2 check parameters set in parallel
        let mut cmd =
                String::from("N111 G38.2 #5226=14 #5222=3 #5221=1 X[#5226 - #5221] Y#5222 (move above nominal hole center)").to_string();
        let mut block = Block::new();

        let last_saved_motion = Some(GCodes::G80);
        let last_saved_distance_mode = DistanceMode::Absolute;
        if let Ok(_) = block.parse(
            &mut cmd,
            &mut pars,
            &last_saved_motion,
            &last_saved_distance_mode,
        ) {
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

    // unimplemented!()
}

mod helper {
    use ngc_block::RefParsUtilTrait;

    use nom::{
        bytes::complete::tag,
        character::complete::multispace0,
        combinator::map,
        number::complete::float,
        sequence::tuple,
        IResult,
        // branch::alt,
        // combinator::cut,
        // Needed,
        // sequence::preceded,
        // error::ErrorKind,
        // multi::fold_many0,
        // number::complete,
    };
    use std::{
        collections::BTreeMap,
        fs::File,
        io::{BufRead, BufReader},
        string::String,
    };

    pub struct Pars(pub BTreeMap<i32, f32>);
    impl Pars {
        // pub fn new() -> Self {
        //     Self(BTreeMap::<i32, f32>::new())
        // }
        pub fn import_from_file(p: &str) -> Option<Self> {
            let path = std::path::Path::new(p);

            let mut line = String::new();
            if let Ok(file) = File::open(path) {
                let mut pars = BTreeMap::<i32, f32>::new();
                let mut bufferReader = BufReader::new(file);
                while let Ok(i) = bufferReader.read_line(&mut line) {
                    if i == 0 {
                        break; //EOF
                    }

                    if let Ok((_, (k, v))) = key_value(line.as_str()) {
                        pars.insert(k, v);
                    }
                }
                return Some(Self(pars));
            } else {
                println!("Error in reading file");
                return None;
            }
        }
    }

    fn key_value<'a>(i: &'a str) -> IResult<&'a str, (i32, f32)> {
        let (i, (k, _, v, _)) =
            tuple((map(float, |x| x), multispace0, map(float, |x| x), tag("\n")))(i)?;

        Ok((i, (k as i32, v)))
    }
    impl RefParsUtilTrait for Pars {
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

    pub struct Tools {
        id: i32,
        length: f32,
        diameter: f32,
    }
}

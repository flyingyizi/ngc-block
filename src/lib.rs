// tell rustc only compile no-std when disable test feature
#![cfg_attr(not(test), no_std)]
#![cfg_attr(test, feature(test))]

extern crate alloc;

mod block;
mod exp;
mod codes;
mod error;
mod params;

pub use {
    block::{Block, DistanceMode},
    exp::RefParsUtilTrait,
    codes::{GCodes, GGroup, MCodes, MGroup},
    params::{FieldIndex, Parameter, Paramters, Position},
};

// pub fn add(left: usize, right: usize) -> usize {
//     left + right
// }

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn it_works() {
//         let result = add(2, 2);
//         assert_eq!(result, 4);
//     }
// }

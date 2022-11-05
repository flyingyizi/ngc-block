//! COMPLAY with [reference\RS274NGC_3.pdf], include statement line parsing, and ngc paramenters visiting.
//! line parsing result is [Block]

#![cfg_attr(not(test), no_std)]
#![cfg_attr(test, feature(test))]

extern crate alloc;

mod block;
mod codes;
mod error;
mod exp;
mod params;

pub use {
    block::{Block, DistanceMode},
    codes::{GCodes, GGroup, MCodes, MGroup},
    error::BlockError,
    exp::RefParsUtilTrait,
    params::{Argument, FieldIndex, Paramters, Position},
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

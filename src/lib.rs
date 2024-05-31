//! COMPLAY with [reference\RS274NGC_3.pdf], include statement line parsing, and ngc paramenters visiting.
//! line parsing result is [Block]

// #![cfg_attr(not(test), no_std)]
// #![cfg_attr(test, feature(test))]

#![no_std]

#[cfg(all(feature = "std", feature = "no_std"))] 
compile_error!("alterative std or no_std, not select at the same time");
cfg_if::cfg_if! {
    if #[cfg(all(feature = "std",not(feature = "no_std")))] {
        use std::{fmt,format,string::{String,ToString},vec,vec::Vec,collections::BTreeMap,boxed::Box};
    }
    else if #[cfg(all(feature = "no_std",not(feature = "std")))] {
        extern crate alloc;
        use alloc::{fmt,format,string::{String,ToString},vec,vec::Vec,collections::BTreeMap,boxed::Box};
    }
}

// always pull in `std` during tests
#[cfg(any(feature = "std", test))]
#[macro_use]
extern crate std;


mod block;
mod codes;
mod error;
mod exp;
mod params;
mod ngc_expression;
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

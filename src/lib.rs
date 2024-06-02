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
        use thiserror::Error;

    }
    else if #[cfg(all(feature = "no_std",not(feature = "std")))] {
        extern crate alloc;
        use alloc::{fmt,format,string::{String,ToString},vec,vec::Vec,collections::BTreeMap,boxed::Box};
        use thiserror_no_std::Error;

    }
}

// always pull in `std` during tests
#[cfg(any(feature = "std", test))]
#[macro_use]
extern crate std;

mod codes;
mod rs274ngc;
mod params;
pub use {
    codes::{GCodes, GGroup, MCodes, MGroup},
    params::{Argument, FieldIndex, Paramters, Position},
    rs274ngc::{parse_lines,Block,DistanceMode},
};

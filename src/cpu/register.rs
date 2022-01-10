use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use super::Mode;

#[derive(Debug, FromPrimitive, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum RegisterType {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    SP,
    LR,
    PC,
}

impl From<u32> for RegisterType {
    fn from(n: u32) -> Self {
        FromPrimitive::from_u32(n).unwrap_or(RegisterType::R0)
    }
}

impl From<u16> for RegisterType {
    fn from(n: u16) -> Self {
        FromPrimitive::from_u16(n).unwrap_or(RegisterType::R0)
    }
}

impl From<u8> for RegisterType {
    fn from(n: u8) -> Self {
        FromPrimitive::from_u8(n).unwrap_or(RegisterType::R0)
    }
}

pub trait Register<T> {
    fn get(&self, mode: Mode) -> T;
    fn set(&mut self, mode: Mode, val: T);
}

#[derive(Default)]
pub struct CommonRegister<T = u32>
where
    T: Copy,
{
    r: T,
}

impl<T> Register<T> for CommonRegister<T>
where
    T: Copy,
{
    fn get(&self, _: Mode) -> T {
        self.r
    }

    fn set(&mut self, _: Mode, val: T) {
        self.r = val;
    }
}

#[derive(Default)]
pub struct FiqRegister<T = u32>
where
    T: Copy,
{
    r: T,
    fiq_r: T,
}

impl<T> Register<T> for FiqRegister<T>
where
    T: Copy,
{
    fn get(&self, mode: Mode) -> T {
        if mode == Mode::Fiq {
            self.fiq_r
        } else {
            self.r
        }
    }

    fn set(&mut self, mode: Mode, val: T) {
        if mode == Mode::Fiq {
            self.fiq_r = val;
        } else {
            self.r = val;
        }
    }
}

#[derive(Default)]
pub struct ModeRegister<T = u32>
where
    T: Copy,
{
    r: [T; 6],
}

impl<T> Register<T> for ModeRegister<T>
where
    T: Copy,
{
    fn get(&self, mode: Mode) -> T {
        if mode == Mode::System {
            return self.r[Mode::User as usize];
        }
        self.r[mode as usize]
    }

    fn set(&mut self, mode: Mode, val: T) {
        if mode == Mode::System {
            self.r[Mode::User as usize] = val;
            return;
        }
        self.r[mode as usize] = val;
    }
}

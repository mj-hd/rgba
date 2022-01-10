use bitmatch::bitmatch;
use num_derive::FromPrimitive;

#[derive(Debug, PartialEq, FromPrimitive)]
pub enum Mode {
    User,
    Fiq,
    Supervisor,
    Abort,
    Irq,
    Undefined,
    System,
}

impl Mode {
    fn has_spsr(&self) -> bool {
        match self {
            Mode::User | Mode::System => false,
            _ => true,
        }
    }
}

impl From<u32> for Mode {
    #[bitmatch]
    fn from(n: u32) -> Mode {
        #[bitmatch]
        match n {
            "0??00" => Mode::User,
            "0??01" => Mode::Fiq,
            "0??10" => Mode::Irq,
            "0??11" => Mode::Supervisor,
            "10000" => Mode::User,
            "10001" => Mode::Fiq,
            "10010" => Mode::Irq,
            "10011" => Mode::Supervisor,
            "10111" => Mode::Abort,
            "11011" => Mode::Undefined,
            "11111" => Mode::System,
            _ => {
                panic!("unsupported mode")
            }
        }
    }
}

impl Into<u32> for Mode {
    fn into(self) -> u32 {
        match self {
            Mode::User => 0b10000,
            Mode::Fiq => 0b10001,
            Mode::Irq => 0b10010,
            Mode::Supervisor => 0b10011,
            Mode::Abort => 0b10111,
            Mode::Undefined => 0b11011,
            Mode::System => 0b11111,
        }
    }
}

mod arm;
pub mod cpu_gba;
mod psr;
mod register;
mod thumb;

use std::ops::{BitAnd, Not, Shr};

use crate::{
    arit::IntoI10,
    arit::IsOverflowAdd,
    arit::{IntoI24, IsOverflowSub},
    arit::{Shift, ShiftResult},
    bus::Bus,
};
use anyhow::{bail, Result};
use bitfield::bitfield;
use bitmatch::bitmatch;
use log::{debug, error, trace};
use num_derive::FromPrimitive;
use num_traits::{
    ops::overflowing::{OverflowingAdd, OverflowingSub},
    FromPrimitive,
};

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

#[derive(Debug, PartialEq, FromPrimitive)]
enum Mode {
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

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct Psr(u32);
    impl Debug;
    n, set_n: 31;
    z, set_z: 30;
    c, set_c: 29;
    v, set_v: 28;
    i, set_i: 7;
    f, set_f: 6;
    t, set_t: 5;
    into Mode, mode, set_mode: 4, 0;
}

impl Psr {
    pub fn set_nz_by<T>(&mut self, result: T)
    where
        T: Eq
            + Copy
            + Ord
            + Shr<usize, Output = T>
            + BitAnd<Output = T>
            + Not<Output = T>
            + Default,
    {
        self.set_n(result & !(!T::default() >> 1) > T::default());
        self.set_z(result == T::default());
    }

    pub fn set_pl_nzcv_by<T>(&mut self, left: T, right: T)
    where
        T: OverflowingAdd
            + IsOverflowAdd
            + Shr<usize, Output = T>
            + BitAnd<Output = T>
            + Not<Output = T>
            + Copy
            + Ord
            + Default,
    {
        self.set_pl_nzc_by(left, right);
        self.set_pl_v_by(left, right);
    }

    pub fn set_pl_nzc_by<T>(&mut self, left: T, right: T)
    where
        T: OverflowingAdd
            + Copy
            + Ord
            + Shr<usize, Output = T>
            + BitAnd<Output = T>
            + Not<Output = T>
            + Default,
    {
        let (result, c) = left.overflowing_add(&right);
        self.set_nz_by(result);
        self.set_c(c);
    }

    pub fn set_ng_nzcv_by<T>(&mut self, left: T, right: T)
    where
        T: OverflowingSub
            + IsOverflowSub
            + Copy
            + Ord
            + Shr<usize, Output = T>
            + BitAnd<Output = T>
            + Not<Output = T>
            + Default,
    {
        self.set_ng_nzc_by(left, right);
        self.set_ng_v_by(left, right);
    }

    pub fn set_ng_nzc_by<T>(&mut self, left: T, right: T)
    where
        T: OverflowingSub
            + Copy
            + Ord
            + Shr<usize, Output = T>
            + BitAnd<Output = T>
            + Not<Output = T>
            + Default,
    {
        let (result, c) = left.overflowing_sub(&right);
        self.set_nz_by(result);
        self.set_c(!c);
    }

    pub fn set_pl_v_by<T>(&mut self, left: T, right: T)
    where
        T: IsOverflowAdd,
    {
        let v = left.is_overflow_add(right);
        self.set_v(v);
    }

    pub fn set_ng_v_by<T>(&mut self, left: T, right: T)
    where
        T: IsOverflowSub,
    {
        let v = left.is_overflow_sub(right);
        self.set_v(v);
    }
}

trait Register<T> {
    fn get(&self, mode: Mode) -> T;
    fn set(&mut self, mode: Mode, val: T);
}

#[derive(Default)]
struct CommonRegister<T = u32>
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
struct FiqRegister<T = u32>
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
struct ModeRegister<T = u32>
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

#[derive(Debug, FromPrimitive)]
enum Cond {
    Eq,
    Ne,
    CsHs,
    CcLo,
    Mi,
    Pl,
    Vs,
    Vc,
    Hi,
    Ls,
    Ge,
    Lt,
    Gt,
    Le,
    Al,
    Nv,
}

impl From<u32> for Cond {
    fn from(n: u32) -> Cond {
        FromPrimitive::from_u32(n).unwrap_or(Cond::Eq)
    }
}

#[derive(Default, Debug)]
struct Exception {
    reset: bool,
    undefined_instruction: bool,
    swi: bool,
    prefetch_abort: bool,
    data_abort: bool,
    address_exceeds: bool,
    irq: bool,
    fiq: bool,
}

#[derive(Default)]
struct Op2 {
    val: u32,
    c: Option<bool>,
    by_register: bool,
}

impl From<ShiftResult> for Op2 {
    fn from(ShiftResult(result, c): ShiftResult) -> Op2 {
        Op2::new(result, c)
    }
}

impl Op2 {
    fn new(val: u32, c: bool) -> Self {
        Self {
            val,
            c: Some(c),
            ..Self::default()
        }
    }

    fn by_register(self, f: bool) -> Self {
        Self {
            by_register: f,
            ..self
        }
    }

    fn c_not_affected(self, f: bool) -> Self {
        Self {
            c: if f { None } else { self.c },
            ..self
        }
    }
}

pub struct Cpu {
    common_r: [CommonRegister; 8],
    fiq_r: [FiqRegister; 5],
    mode_r: [ModeRegister; 2],

    pc: u32,
    pc_invalidated: bool,
    cpsr: Psr,
    spsr: ModeRegister<Psr>,

    exception: Exception,

    cycles: u32,
    stalls: u32,
    prefetch: [u32; 2],
    halt: bool,

    trace: bool,

    pub bus: Bus,
}

impl Cpu {
    pub fn new(bus: Bus) -> Self {
        Self {
            cycles: 0,
            stalls: 0,
            prefetch: Default::default(),
            halt: false,
            bus,
            common_r: Default::default(),
            fiq_r: Default::default(),
            mode_r: Default::default(),
            pc: 0,
            pc_invalidated: false,
            cpsr: Default::default(),
            spsr: Default::default(),
            exception: Default::default(),
            trace: false,
        }
    }

    pub fn reset(&mut self) -> Result<()> {
        self.pc = 0x0000_0000;
        // self.pc = 0x0800_0000;
        self.cpsr.0 = 0x0000_001F;
        self.reset_sp();

        self.refresh_prefetch()?;
        let opecode = self.fetch()?;
        self.prefetch[0] = opecode;

        self.exception = Default::default();

        Ok(())
    }

    pub fn tick(&mut self) -> Result<()> {
        self.cycles = self.cycles.wrapping_add(1);

        self.bus.tick()?;

        if self.stalls > 0 {
            self.stalls -= 1;

            return Ok(());
        }

        if self.check_irq() {
            self.exception.irq = true;
        }

        self.do_exception()?;

        if self.pc_invalidated {
            self.refresh_prefetch()?;
            self.pc_invalidated = false;
        }

        if self.halt {
            return Ok(());
        }

        // let mut register_status: String = "".to_string();

        // if self.trace {
        //     register_status = (0u8..=15u8)
        //         .map(|r| RegisterType::from(r))
        //         .map(|r| format!("{:08X}", self.get_r(r)))
        //         .collect::<Vec<_>>()
        //         .join(" ");
        // }

        let opecode = self.pop_prefetch()?;

        if self.pc >= 0x0800_0000 {
            self.trace = true;
        }

        if self.cpsr.t() {
            let opecode = opecode as u16;

            // if self.trace {
            //     trace!(
            //         "{} cpsr: {:08X} {:04X} prefetch_pc: {:08X}",
            //         register_status,
            //         self.cpsr.0,
            //         opecode,
            //         self.pc,
            //     );
            // }

            self.do_mnemonic_thumb(opecode)?;
        } else {
            // if self.trace {
            //     trace!(
            //         "{} cpsr: {:08X} {:04X} prefetch_pc: {:08X}",
            //         register_status,
            //         self.cpsr.0,
            //         opecode,
            //         self.pc,
            //     );
            // }

            self.do_mnemonic(opecode)?;
        }

        Ok(())
    }

    fn check_irq(&mut self) -> bool {
        if !self.cpsr.i() && self.bus.interrupt_disable {
            if self.bus.interrupt_enable.lcd_v_blank() && self.bus.interrupt_flag.lcd_v_blank() {
                self.bus.interrupt_flag.set_lcd_v_blank(false);
                return true;
            }

            if self.bus.interrupt_enable.lcd_h_blank() && self.bus.interrupt_flag.lcd_h_blank() {
                self.bus.interrupt_flag.set_lcd_h_blank(false);
                return true;
            }

            if self.bus.interrupt_enable.timer_0() && self.bus.interrupt_flag.timer_0() {
                self.bus.interrupt_flag.set_timer_0(false);
                return true;
            }

            if self.bus.interrupt_enable.timer_1() && self.bus.interrupt_flag.timer_1() {
                self.bus.interrupt_flag.set_timer_1(false);
                return true;
            }

            if self.bus.interrupt_enable.timer_2() && self.bus.interrupt_flag.timer_2() {
                self.bus.interrupt_flag.set_timer_2(false);
                return true;
            }

            if self.bus.interrupt_enable.timer_3() && self.bus.interrupt_flag.timer_3() {
                self.bus.interrupt_flag.set_timer_3(false);
                return true;
            }

            if self.bus.interrupt_enable.serial() && self.bus.interrupt_flag.serial() {
                self.bus.interrupt_flag.set_serial(false);
                return true;
            }

            if self.bus.interrupt_enable.dma_0() && self.bus.interrupt_flag.dma_0() {
                self.bus.interrupt_flag.set_dma_0(false);
                return true;
            }

            if self.bus.interrupt_enable.dma_1() && self.bus.interrupt_flag.dma_1() {
                self.bus.interrupt_flag.set_dma_1(false);
                return true;
            }

            if self.bus.interrupt_enable.dma_2() && self.bus.interrupt_flag.dma_2() {
                self.bus.interrupt_flag.set_dma_2(false);
                return true;
            }

            if self.bus.interrupt_enable.dma_3() && self.bus.interrupt_flag.dma_3() {
                self.bus.interrupt_flag.set_dma_3(false);
                return true;
            }

            if self.bus.interrupt_enable.keypad() && self.bus.interrupt_flag.keypad() {
                self.bus.interrupt_flag.set_keypad(false);
                return true;
            }

            if self.bus.interrupt_enable.game_pak() && self.bus.interrupt_flag.game_pak() {
                self.bus.interrupt_flag.set_game_pak(false);
                return true;
            }
        }

        return false;
    }

    fn do_exception(&mut self) -> Result<()> {
        if self.exception.reset {
            self.do_interrupt(0x0000_0000, true, Mode::Supervisor.into())?;
            self.exception.reset = false;
        }

        if self.exception.data_abort {
            self.do_interrupt(0x0000_0010, self.cpsr.f(), Mode::Abort.into())?;
            self.exception.data_abort = false;
        }

        if self.exception.fiq {
            self.do_interrupt(0x0000_001C, true, Mode::Fiq.into())?;
            self.exception.fiq = false;
        }

        if self.exception.irq {
            self.do_interrupt(0x0000_0018, self.cpsr.f(), Mode::Irq.into())?;
            self.exception.irq = false;
        }

        if self.exception.prefetch_abort {
            self.do_interrupt(0x0000_000C, self.cpsr.f(), Mode::Abort.into())?;
            self.exception.prefetch_abort = false;
        }

        if self.exception.swi {
            self.do_interrupt(0x0000_0008, self.cpsr.f(), Mode::Supervisor.into())?;
            self.exception.swi = false;
        }

        if self.exception.undefined_instruction {
            self.do_interrupt(0x0000_0004, self.cpsr.f(), Mode::Undefined.into())?;
            self.exception.undefined_instruction = false;
        }

        if self.exception.address_exceeds {
            self.do_interrupt(0x0000_0014, self.cpsr.f(), Mode::Supervisor.into())?;
        }

        Ok(())
    }

    fn do_interrupt(&mut self, addr: u32, new_f: bool, mode: Mode) -> Result<()> {
        let orig_pc = self.pc;
        let orig_cpsr = self.cpsr;

        self.pc = addr;
        self.pc_invalidated = true;
        self.cpsr.set_mode(mode.into());

        self.set_r(RegisterType::LR, orig_pc - self.instr_size())?;

        self.set_spsr(orig_cpsr);

        self.cpsr.set_t(false);
        self.cpsr.set_i(true);
        self.cpsr.set_f(new_f);

        Ok(())
    }

    fn refresh_prefetch(&mut self) -> Result<()> {
        let opecode = self.fetch()?;
        self.prefetch[0] = opecode;

        self.pc = self.pc.wrapping_add(self.instr_size());

        let opecode = self.fetch()?;
        self.prefetch[1] = opecode;

        Ok(())
    }

    fn pop_prefetch(&mut self) -> Result<u32> {
        self.pc = self.pc.wrapping_add(self.instr_size());

        let next = self.prefetch[0];

        self.prefetch[0] = self.prefetch[1];

        let opecode = self.fetch()?;
        self.prefetch[1] = opecode;

        Ok(next)
    }

    fn fetch(&mut self) -> Result<u32> {
        let opecode = if self.cpsr.t() {
            // THUMB mode
            self.bus.read_16(self.pc)? as u32
        } else {
            // ARM mode
            self.bus.read_32(self.pc)?
        };

        Ok(opecode)
    }

    fn instr_size(&self) -> u32 {
        if self.cpsr.t() {
            2
        } else {
            4
        }
    }

    fn guard(&self, cond: u32) -> bool {
        let cpsr = self.cpsr;

        match Cond::from(cond) {
            Cond::Eq => cpsr.z(),
            Cond::Ne => !cpsr.z(),
            Cond::CsHs => cpsr.c(),
            Cond::CcLo => !cpsr.c(),
            Cond::Mi => cpsr.n(),
            Cond::Pl => !cpsr.n(),
            Cond::Vs => cpsr.v(),
            Cond::Vc => !cpsr.v(),
            Cond::Hi => cpsr.c() && !cpsr.z(),
            Cond::Ls => !cpsr.c() || cpsr.z(),
            Cond::Ge => cpsr.n() == cpsr.v(),
            Cond::Lt => cpsr.n() != cpsr.v(),
            Cond::Gt => !cpsr.z() && (cpsr.n() == cpsr.v()),
            Cond::Le => cpsr.z() || (cpsr.n() != cpsr.v()),
            Cond::Al => true,
            Cond::Nv => false,
        }
    }

    fn get_r(&self, r: RegisterType) -> u32 {
        let mode = self.cpsr.mode();

        match r {
            RegisterType::R0 => self.common_r[0].get(mode),
            RegisterType::R1 => self.common_r[1].get(mode),
            RegisterType::R2 => self.common_r[2].get(mode),
            RegisterType::R3 => self.common_r[3].get(mode),
            RegisterType::R4 => self.common_r[4].get(mode),
            RegisterType::R5 => self.common_r[5].get(mode),
            RegisterType::R6 => self.common_r[6].get(mode),
            RegisterType::R7 => self.common_r[7].get(mode),
            RegisterType::R8 => self.fiq_r[0].get(mode),
            RegisterType::R9 => self.fiq_r[1].get(mode),
            RegisterType::R10 => self.fiq_r[2].get(mode),
            RegisterType::R11 => self.fiq_r[3].get(mode),
            RegisterType::R12 => self.fiq_r[4].get(mode),
            RegisterType::SP => self.mode_r[0].get(mode),
            RegisterType::LR => self.mode_r[1].get(mode),
            RegisterType::PC => self.pc,
        }
    }

    fn set_r(&mut self, r: RegisterType, val: u32) -> Result<()> {
        let mode = self.cpsr.mode();

        match r {
            RegisterType::R0 => self.common_r[0].set(mode, val),
            RegisterType::R1 => self.common_r[1].set(mode, val),
            RegisterType::R2 => self.common_r[2].set(mode, val),
            RegisterType::R3 => self.common_r[3].set(mode, val),
            RegisterType::R4 => self.common_r[4].set(mode, val),
            RegisterType::R5 => self.common_r[5].set(mode, val),
            RegisterType::R6 => self.common_r[6].set(mode, val),
            RegisterType::R7 => self.common_r[7].set(mode, val),
            RegisterType::R8 => self.fiq_r[0].set(mode, val),
            RegisterType::R9 => self.fiq_r[1].set(mode, val),
            RegisterType::R10 => self.fiq_r[2].set(mode, val),
            RegisterType::R11 => self.fiq_r[3].set(mode, val),
            RegisterType::R12 => self.fiq_r[4].set(mode, val),
            RegisterType::SP => self.mode_r[0].set(mode, val),
            RegisterType::LR => self.mode_r[1].set(mode, val),
            RegisterType::PC => {
                self.pc = val & !1u32;
                self.pc_invalidated = true;
            }
        }

        Ok(())
    }

    fn get_spsr(&self) -> Psr {
        self.spsr.get(self.cpsr.mode())
    }

    fn set_spsr(&mut self, psr: Psr) {
        self.spsr.set(self.cpsr.mode(), psr);
    }

    fn reset_sp(&mut self) {
        self.mode_r[0].set(Mode::User, 0x0300_7F00);
        self.mode_r[0].set(Mode::Fiq, 0x00000000);
        self.mode_r[0].set(Mode::Supervisor, 0x03007FE0);
        self.mode_r[0].set(Mode::Abort, 0x00000000);
        self.mode_r[0].set(Mode::Irq, 0x03007FA0);
        self.mode_r[0].set(Mode::Undefined, 0x00000000);
    }

    #[bitmatch]
    fn op2_im(&self, p: u32) -> Op2 {
        Op2::new(p, self.cpsr.c())
    }

    #[bitmatch]
    fn op2_im_shift(&self, p: u32) -> Op2 {
        #[bitmatch]
        let "ssss_nnnnnnnn" = p;

        if s == 0 {
            Op2::new(n, self.cpsr.c())
        } else {
            Op2::from(n.ror(s * 2, self.cpsr.c(), false))
        }
    }

    #[bitmatch]
    fn op2_reg(&self, p: u32) -> Op2 {
        #[bitmatch]
        let "ssss_sttfrrrr" = p;

        let by_register = f == 1;

        let r = RegisterType::from(r);
        let val = self.get_r(r)
            + if by_register && r == RegisterType::PC {
                4
            } else {
                0
            };

        let c_not_affected = by_register && (s >> 1) == 0;

        let shift = if by_register {
            let r = RegisterType::from(s >> 1);
            self.get_r(r) as u8 as u32
        } else {
            s
        };

        let zero_shift = !by_register && shift == 0;

        match t {
            // LSL
            0b00 => Op2::from(val.lsl(shift, self.cpsr.c()))
                .by_register(by_register)
                .c_not_affected(c_not_affected),
            // LSR
            0b01 => Op2::from(val.lsr(shift, zero_shift))
                .by_register(by_register)
                .c_not_affected(c_not_affected),
            // ASR
            0b10 => Op2::from(val.asr(shift, zero_shift))
                .by_register(by_register)
                .c_not_affected(c_not_affected),
            // ROR
            0b11 => Op2::from(val.ror(shift, self.cpsr.c(), zero_shift))
                .by_register(by_register)
                .c_not_affected(c_not_affected),
            _ => panic!("invalid op2 shift type"),
        }
    }

    fn op2(&self, i: u32, p: u32) -> Op2 {
        if i == 1 {
            self.op2_im_shift(p)
        } else {
            self.op2_reg(p)
        }
    }

    fn op2_simple(&self, i: u32, p: u32) -> Op2 {
        if i == 1 {
            self.op2_reg(p)
        } else {
            self.op2_im(p)
        }
    }

    fn rlist(&self, rlist: u16) -> Vec<RegisterType> {
        let mut registers = vec![];

        for r in 0u16..16u16 {
            if rlist & (1 << r) == 0 {
                continue;
            }

            registers.push(RegisterType::from(r));
        }

        registers
    }

    fn rlist_low(&self, rlist: u8) -> Vec<RegisterType> {
        let mut registers = vec![];

        for r in 0u8..8u8 {
            if rlist & (1 << r) == 0 {
                continue;
            }

            registers.push(RegisterType::from(r));
        }

        registers
    }

    #[bitmatch]
    pub fn do_mnemonic(&mut self, opecode: u32) -> Result<()> {
        #[bitmatch]
        match &opecode {
            // B
            "cccc1010_nnnnnnnn_nnnnnnnn_nnnnnnnn" => {
                // trace!("B {:08X}", n);

                if !self.guard(c) {
                    return Ok(());
                }

                self.b(n.into_i24())
            }

            // BL
            "cccc1011_nnnnnnnn_nnnnnnnn_nnnnnnnn" => {
                // trace!("BL {:08X}", n);

                if !self.guard(c) {
                    return Ok(());
                }

                self.bl(n.into_i24())
            }

            // BX
            "cccc0001_00101111_11111111_0001rrrr" => {
                // trace!("BX {:01X}", r);

                if !self.guard(c) {
                    return Ok(());
                }

                self.bx(RegisterType::from(r))
            }

            // SWI
            "cccc1111_????????_????????_????????" => {
                // trace!("SWI");

                if !self.guard(c) {
                    return Ok(());
                }

                self.swi()
            }

            // BKPT
            "cccc0001_0010????_????????_0111????" => {
                // trace!("BKPT");

                if !self.guard(c) {
                    return Ok(());
                }

                self.bkpt()
            }

            // UND
            "cccc011?_????????_????????_???1????" => {
                // trace!("UND");

                if !self.guard(c) {
                    return Ok(());
                }

                self.und()
            }

            // MRS
            "cccc0001_0p001111_rrrr0000_00000000" => {
                // trace!("MRS {:01X}, {:01X}", p, r);

                if !self.guard(c) {
                    return Ok(());
                }

                self.mrs(p, RegisterType::from(r))
            }

            // MSR Register
            "cccc0001_0p10fsxt_11110000_0000rrrr" => {
                // trace!(
                //     "MSR Register {:01X}, {:01X}, {:01X}, {:01X}, {:01X}, {:01X}",
                //     p,
                //     f,
                //     s,
                //     x,
                //     t,
                //     r
                // );

                if !self.guard(c) {
                    return Ok(());
                }

                let val = self.get_r(RegisterType::from(r));

                self.msr(p, f == 1, s == 1, x == 1, t == 1, Psr(val))
            }

            // MSR Immediate
            "cccc0011_0p10fsxt_1111iiii_jjjjjjjj" => {
                // trace!(
                //     "MSR Immediate {:01X}, {:01X}, {:01X}, {:01X}, {:01X}, {:01X}, {:02X}",
                //     p,
                //     f,
                //     s,
                //     x,
                //     t,
                //     i,
                //     j
                // );

                if !self.guard(c) {
                    return Ok(());
                }

                let val = j.rotate_right(i * 2);

                self.msr(p, f == 1, s == 1, x == 1, t == 1, Psr(val))
            }

            // Single Data Transfer - post-indexing
            "cccc01i0_ubtlrrrr_ggggpppp_pppppppp" if self.guard(c) => self
                .single_data_transfer_post(
                    u,
                    b,
                    t,
                    l,
                    RegisterType::from(r),
                    RegisterType::from(g),
                    self.op2_simple(i, p),
                ),

            // Single Data Transfer - pre-indexing
            "cccc01i1_ubwlrrrr_ggggpppp_pppppppp" if self.guard(c) => self
                .single_data_transfer_pre(
                    u,
                    b,
                    w == 1,
                    l,
                    RegisterType::from(r),
                    RegisterType::from(g),
                    self.op2_simple(i, p),
                ),

            // halfword single data transfer - pre-indexing
            "cccc0001_uiwlrrrr_ggggjjjj_1011ssss" if self.guard(c) => self
                .halfword_single_data_transfer_pre(
                    u,
                    w == 1,
                    l,
                    RegisterType::from(r),
                    RegisterType::from(g),
                    self.halfword_transfer_offset(i == 1, j, s),
                ),

            // halfword single data transfer - post-indexing
            "cccc0000_ui0lrrrr_ggggjjjj_1011ssss" if self.guard(c) => self
                .halfword_single_data_transfer_post(
                    u,
                    l,
                    RegisterType::from(r),
                    RegisterType::from(g),
                    self.halfword_transfer_offset(i == 1, j, s),
                ),

            // block data transfer - pre-indexing
            "cccc1001_uswlrrrr_gggggggg_gggggggg" if self.guard(c) => self.block_data_transfer_pre(
                u,
                s == 1,
                w == 1,
                l,
                RegisterType::from(r),
                self.rlist(g as u16),
            ),

            // block data transfer - post-indexing
            "cccc1000_uswlrrrr_gggggggg_gggggggg" if self.guard(c) => self
                .block_data_transfer_post(
                    u,
                    s == 1,
                    w == 1,
                    l,
                    RegisterType::from(r),
                    self.rlist(g as u16),
                ),

            // SWP
            "cccc0001_0b00rrrr_gggg0000_1001ffff" if self.guard(c) => self.swp(
                b,
                RegisterType::from(r),
                RegisterType::from(g),
                RegisterType::from(f),
            ),

            // Multiply
            "cccc000o_ooosrrrr_ggggffff_1001hhhh" => {
                // trace!("Multiply");

                if !self.guard(c) {
                    return Ok(());
                }

                self.multiply(
                    o,
                    s == 1,
                    RegisterType::from(r),
                    RegisterType::from(g),
                    RegisterType::from(f),
                    RegisterType::from(h),
                )
            }

            // ALU
            "cccc00io_ooosrrrr_ggggpppp_pppppppp" => {
                // trace!(
                //     "ALU {:01X}, {:01X}, {:01X}, {:01X}, {:01X}, {:02X}",
                //     i,
                //     o,
                //     s,
                //     r,
                //     g,
                //     p
                // );

                if !self.guard(c) {
                    return Ok(());
                }

                self.alu(
                    o,
                    s == 1,
                    RegisterType::from(r),
                    RegisterType::from(g),
                    self.op2(i, p),
                )
            }

            _ => {
                // FIXME: guard落ちた場合もここはいる
                Ok(())
            }
        }
    }

    #[bitmatch]
    fn do_mnemonic_thumb(&mut self, opecode: u16) -> Result<()> {
        #[bitmatch]
        match &opecode {
            // software interrupt and breakpoint
            // SWI
            "11011111_????????" => self.swi(),

            // BKPT
            "10111110_????????" => self.bkpt(),

            // LSL
            "00000sss_ssrrrggg" => self.thumb_lsl(RegisterType::from(r), RegisterType::from(g), s),

            // LSR
            "00001sss_ssrrrggg" => self.thumb_lsr(RegisterType::from(r), RegisterType::from(g), s),

            // ASR
            "00010sss_ssrrrggg" => self.thumb_asr(RegisterType::from(r), RegisterType::from(g), s),

            // ADD r
            "0001100r_rrgggfff" => {
                let val = self.get_r(RegisterType::from(r));

                self.thumb_add(val, RegisterType::from(g), RegisterType::from(f))
            }

            // ADD imm
            "0001110i_iigggfff" => {
                self.thumb_add(i as u32, RegisterType::from(g), RegisterType::from(f))
            }

            // SUB r
            "0001101r_rrgggfff" => {
                let val = self.get_r(RegisterType::from(r));

                self.thumb_sub(val, RegisterType::from(g), RegisterType::from(f))
            }

            // SUB imm
            "0001111i_iigggfff" => {
                self.thumb_sub(i as u32, RegisterType::from(g), RegisterType::from(f))
            }

            // MOV
            "00100rrr_nnnnnnnn" => self.thumb_mov(RegisterType::from(r), n as u32, true),

            // CMP
            "00101rrr_nnnnnnnn" => self.thumb_cmp(RegisterType::from(r), n as u32),

            // ADD MOV
            "00110rrr_nnnnnnnn" => self.thumb_add_mov(RegisterType::from(r), n),

            // SUB MOV
            "00111rrr_nnnnnnnn" => self.thumb_sub_mov(RegisterType::from(r), n),

            // AND MOV
            "01000000_00rrrggg" => self.thumb_and_mov(RegisterType::from(r), RegisterType::from(g)),

            // EOR MOV
            "01000000_01rrrggg" => self.thumb_eor_mov(RegisterType::from(r), RegisterType::from(g)),

            // LSL MOV
            "01000000_10rrrggg" => self.thumb_lsl_mov(RegisterType::from(r), RegisterType::from(g)),

            // LSR MOV
            "01000000_11rrrggg" => self.thumb_lsr_mov(RegisterType::from(r), RegisterType::from(g)),

            // ASR MOV
            "01000001_00rrrggg" => self.thumb_asr_mov(RegisterType::from(r), RegisterType::from(g)),

            // ADC MOV
            "01000001_01rrrggg" => self.thumb_adc_mov(RegisterType::from(r), RegisterType::from(g)),

            // SBC MOV
            "01000001_10rrrggg" => self.thumb_sbc_mov(RegisterType::from(r), RegisterType::from(g)),

            // ROR MOV
            "01000001_11rrrggg" => self.thumb_ror_mov(RegisterType::from(r), RegisterType::from(g)),

            // TST
            "01000010_00rrrggg" => self.thumb_tst(RegisterType::from(r), RegisterType::from(g)),

            // NEG
            "01000010_01rrrggg" => self.thumb_neg(RegisterType::from(r), RegisterType::from(g)),

            // CMP r
            "01000010_10rrrggg" => {
                let n = self.get_r(RegisterType::from(g));

                self.thumb_cmp(RegisterType::from(r), n)
            }

            // CMN
            "01000010_11rrrggg" => self.thumb_cmn(RegisterType::from(r), RegisterType::from(g)),

            // ORR
            "01000011_00rrrggg" => self.thumb_orr(RegisterType::from(r), RegisterType::from(g)),

            // MUL
            "01000011_01rrrggg" => self.thumb_mul(RegisterType::from(r), RegisterType::from(g)),

            // BIC
            "01000011_10rrrggg" => self.thumb_bic(RegisterType::from(r), RegisterType::from(g)),

            // MVN
            "01000011_11rrrggg" => self.thumb_mvn(RegisterType::from(r), RegisterType::from(g)),

            // hiR
            "010001oo_dsrrrggg" => {
                let rs = if s == 1 {
                    RegisterType::from(r + 8)
                } else {
                    RegisterType::from(r)
                };
                let rd = if d == 1 {
                    RegisterType::from(g + 8)
                } else {
                    RegisterType::from(g)
                };

                match o {
                    // ADD
                    0b00 => {
                        let val = self.get_r(rd);

                        self.thumb_add(val, rs, rd)
                    }
                    // CMP
                    0b01 => {
                        let n = self.get_r(rs);

                        self.thumb_cmp(rd, n)
                    }
                    // MOV
                    0b10 => {
                        let n = self.get_r(rs);

                        self.thumb_mov(rd, n, false)
                    }
                    // BX
                    0b11 => self.bx(rs),
                    _ => bail!("invalid hi register opcode"),
                }
            }

            // LDR relative
            "01001rrr_nnnnnnnn" => {
                let addr = self.pc.wrapping_add(n as u32 * 4) & !2u32;
                let rd = RegisterType::from(r);

                self.ldr(addr, 4, rd)
            }

            // Load/Store sign-extended byte halfword
            "01010oor_rrbbbddd" => {
                let ro = RegisterType::from(r);
                let rb = RegisterType::from(b);
                let rd = RegisterType::from(d);

                let base_addr = self.get_r(rb);
                let offset = self.get_r(ro);
                let addr = base_addr.wrapping_add(offset);

                match o {
                    // STRH
                    0b00 => {
                        let result = self.get_r(rd) as u16;

                        self.bus.write_16(addr, result)
                    }
                    // LDSB
                    0b01 => {
                        let result = self.bus.read_8(addr)? as i8 as i16;

                        self.set_r(rd, result as u32)?;

                        Ok(())
                    }
                    // LDRH
                    0b10 => {
                        let result = self.bus.read_16(addr)? as u16;

                        self.set_r(rd, result as u32)?;

                        Ok(())
                    }
                    // LDSH
                    0b11 => {
                        let result = self.bus.read_16(addr)? as i16 as i32;

                        self.set_r(rd, result as u32)?;

                        Ok(())
                    }
                    _ => bail!("invalid load store sign-extended opecode"),
                }
            }

            // load store with register offset
            "0101oo0_rrrbbbddd" => {
                let ro = RegisterType::from(r);
                let rb = RegisterType::from(b);
                let rd = RegisterType::from(d);

                let base_addr = self.get_r(rb);
                let offset = self.get_r(ro);
                let addr = base_addr.wrapping_add(offset);

                match o {
                    // STR
                    0b00 => {
                        let result = self.get_r(rd);

                        self.bus.write_32(addr, result)
                    }
                    // STRB
                    0b01 => {
                        let result = self.get_r(rd) as u8;

                        self.bus.write_8(addr, result)
                    }
                    // LDR
                    0b10 => {
                        let result = self.bus.read_32(addr)?;

                        self.set_r(rd, result)?;

                        Ok(())
                    }
                    // LDRB
                    0b11 => {
                        let result = self.bus.read_8(addr)?;

                        self.set_r(rd, result as u32)?;

                        Ok(())
                    }
                    _ => bail!("invalid load/store with register offset opcode"),
                }
            }

            // load/store with immediate offset
            "011oonnn_nnbbbddd" => {
                let rb = RegisterType::from(b);
                let rd = RegisterType::from(d);

                let base_addr = self.get_r(rb);

                match o {
                    // STR
                    0b00 => {
                        let result = self.get_r(rd);

                        let addr = base_addr.wrapping_add(n as u32 * 4);

                        self.bus.write_32(addr, result)
                    }
                    // LDR
                    0b01 => {
                        let addr = base_addr.wrapping_add(n as u32 * 4);

                        let result = self.bus.read_32(addr)?;

                        self.set_r(rd, result)?;

                        Ok(())
                    }
                    // STRB
                    0b10 => {
                        let result = self.get_r(rd) as u8;

                        let addr = base_addr.wrapping_add(n as u32);

                        self.bus.write_8(addr, result)
                    }
                    // LDRB
                    0b11 => {
                        let addr = base_addr.wrapping_add(n as u32);

                        let result = self.bus.read_8(addr)?;

                        self.set_r(rd, result as u32)?;

                        Ok(())
                    }
                    _ => bail!("invalid load/store with immediate offset opcode"),
                }
            }

            // load/store halfword
            "1000onnn_nnbbbddd" => {
                let rb = RegisterType::from(b);
                let rd = RegisterType::from(d);

                let base_addr = self.get_r(rb);
                let addr = base_addr.wrapping_add(n as u32);

                match o {
                    // STRH
                    0b0 => {
                        let result = self.get_r(rd) as u16;

                        self.bus.write_16(addr, result)
                    }
                    // LDRH
                    0b1 => {
                        let result = self.bus.read_16(addr)?;

                        self.set_r(rd, result as u32)?;

                        Ok(())
                    }
                    _ => bail!("invalid load/store halfword opcode"),
                }
            }

            // load/store SP-relative
            "1001orrr_nnnnnnnn" => {
                let rd = RegisterType::from(r);

                let base_addr = self.get_r(RegisterType::SP);
                let addr = base_addr.wrapping_add(n as u32);

                match o {
                    // STR
                    0b0 => {
                        let result = self.get_r(rd);

                        self.bus.write_32(addr, result)
                    }
                    // LDR
                    0b1 => {
                        let result = self.bus.read_32(addr)?;

                        self.set_r(rd, result)?;

                        Ok(())
                    }
                    _ => bail!("invalid load/store SP-relative"),
                }
            }

            // get relative address
            "1010orrr_nnnnnnnn" => {
                let rd = RegisterType::from(r);

                match o {
                    0b0 => {
                        let val = (self.pc & !2u32).wrapping_add(n as u32 * 4);
                        self.set_r(rd, val)?;

                        Ok(())
                    }
                    0b1 => {
                        let val = self.get_r(RegisterType::SP).wrapping_add(n as u32 * 4);
                        self.set_r(rd, val)?;

                        Ok(())
                    }
                    _ => bail!("invalid get relative address opcode"),
                }
            }

            // add offset to stack pointer
            "10110000_onnnnnnn" => {
                let mut result = self.get_r(RegisterType::SP);

                if o == 1 {
                    result = result.wrapping_sub(n as u32 * 4);
                } else {
                    result = result.wrapping_add(n as u32 * 4);
                };

                self.set_r(RegisterType::SP, result)?;

                Ok(())
            }

            // push/pop registers
            "1011o10s_nnnnnnnn" => {
                let mut registers = self.rlist_low(n as u8);

                match o {
                    // PUSH
                    0b0 => {
                        if s == 1 {
                            registers.push(RegisterType::LR);
                        }

                        self.thumb_push(registers)
                    }
                    // POP
                    0b1 => {
                        if s == 1 {
                            registers.push(RegisterType::PC);
                        }

                        self.thumb_pop(registers)?;

                        Ok(())
                    }
                    _ => bail!("invalid push pop registers opcode"),
                }
            }

            // multiple load/store
            "1100obbb_nnnnnnnn" => {
                let rb = RegisterType::from(b);
                let registers = self.rlist_low(n as u8);

                match o {
                    // STMIA
                    0b0 => self.thumb_stmia(rb, registers),
                    // LDMIA
                    0b1 => self.thumb_ldmia(rb, registers),
                    _ => bail!("invalid mutiple load store opecode"),
                }
            }

            // conditional branch
            "1101oooo_nnnnnnnn" => {
                if self.guard(o as u32) {
                    self.thumb_branch(n as i8 as i16)
                } else {
                    Ok(())
                }
            }

            // unconditional branch
            "11100nnn_nnnnnnnn" => self.thumb_branch(n.into_i10()),

            // long branch with link
            // first
            "11110nnn_nnnnnnnn" => self.thumb_long_branch_first(n.into_i10()),

            // Second Instruction
            // BX
            "11111nnn_nnnnnnnn" => self.thumb_long_branch_second(n),

            _ => {
                error!("UNDEFINED INSTRUCTION {:08X}", opecode);

                self.und()
            }
        }
    }

    fn b(&mut self, nn: i32) -> Result<()> {
        self.pc = self.pc.wrapping_add((nn * 4) as u32);
        self.pc_invalidated = true;

        Ok(())
    }

    fn bl(&mut self, nn: i32) -> Result<()> {
        self.set_r(RegisterType::LR, self.pc.wrapping_sub(4))?;

        self.pc = self.pc.wrapping_add((nn * 4) as u32);
        self.pc_invalidated = true;

        Ok(())
    }

    fn bx(&mut self, r: RegisterType) -> Result<()> {
        let val = self.get_r(r);

        if val & 1 == 1 {
            self.pc = val - 1;
            self.cpsr.set_t(true);
        } else {
            self.pc = val;
            self.cpsr.set_t(false);
        }

        self.pc_invalidated = true;

        Ok(())
    }

    fn swi(&mut self) -> Result<()> {
        self.exception.swi = true;

        Ok(())
    }

    fn bkpt(&mut self) -> Result<()> {
        debug!("BKPT");

        Ok(())
    }

    fn und(&mut self) -> Result<()> {
        self.exception.undefined_instruction = true;

        Ok(())
    }

    fn alu(
        &mut self,
        opcode: u32,
        state: bool,
        rn: RegisterType,
        rd: RegisterType,
        op2: Op2,
    ) -> Result<()> {
        let operand = self.get_r(rn)
            + if op2.by_register && rn == RegisterType::PC {
                4
            } else {
                0
            };
        let cpsr = match opcode {
            // AND
            0b0000 => self.and(operand, rd, op2),
            // EOR
            0b0001 => self.eor(operand, rd, op2),
            // SUB
            0b0010 => self.sub(operand, rd, op2),
            // RSB
            0b0011 => self.rsb(operand, rd, op2),
            // ADD
            0b0100 => self.add(operand, rd, op2),
            // ADC
            0b0101 => self.adc(operand, rd, op2),
            // SBC
            0b0110 => self.sbc(operand, rd, op2),
            // RSC
            0b0111 => self.rsc(operand, rd, op2),
            // TST
            0b1000 => self.tst(operand, op2, rd),
            // TEQ
            0b1001 => self.teq(operand, op2, rd),
            // CMP
            0b1010 => self.cmp(operand, op2, rd),
            // CMN
            0b1011 => self.cmn(operand, op2, rd),
            // ORR
            0b1100 => self.orr(operand, rd, op2),
            // MOV
            0b1101 => self.mov(rd, op2),
            // BIC
            0b1110 => self.bic(operand, rd, op2),
            // MVN
            0b1111 => self.mvn(rd, op2),
            _ => bail!("invalid alu opcode"),
        }?;

        if state {
            let prev_cpsr = self.cpsr;

            self.cpsr = cpsr;

            if self.cpsr.t() != prev_cpsr.t() {
                self.pc_invalidated = true;
            }
        }

        Ok(())
    }

    fn _alu_pc_special_if_needed(&self, rd: RegisterType) -> Option<Psr> {
        if self.cpsr.mode().has_spsr() && rd == RegisterType::PC {
            Some(self.get_spsr())
        } else {
            None
        }
    }

    fn _alu_logical<Operation>(
        &mut self,
        left: u32,
        right: u32,
        rd: RegisterType,
        op2: Op2,
        f: Operation,
    ) -> Result<Psr>
    where
        Operation: Fn(u32, u32) -> u32,
    {
        let result = f(left, right);

        self.set_r(rd, result)?;

        if let Some(cpsr) = self._alu_pc_special_if_needed(rd) {
            return Ok(cpsr);
        }

        let mut cpsr = self.cpsr;
        cpsr.set_nz_by(result);

        if let Some(c) = op2.c {
            cpsr.set_c(c);
        }

        Ok(cpsr)
    }

    fn _alu_arit_pl<Operation>(
        &mut self,
        left: u32,
        right: u32,
        rd: RegisterType,
        f: Operation,
    ) -> Result<Psr>
    where
        Operation: Fn(u32, u32) -> u32,
    {
        let result = f(left, right);

        self.set_r(rd, result)?;

        if let Some(cpsr) = self._alu_pc_special_if_needed(rd) {
            return Ok(cpsr);
        }

        let mut cpsr = self.cpsr;
        cpsr.set_pl_nzcv_by(left, right);

        Ok(cpsr)
    }

    fn _alu_arit_ng<Operation>(
        &mut self,
        left: u32,
        right: u32,
        rd: RegisterType,
        f: Operation,
    ) -> Result<Psr>
    where
        Operation: Fn(u32, u32) -> u32,
    {
        let result = f(left, right);

        self.set_r(rd, result)?;

        if let Some(cpsr) = self._alu_pc_special_if_needed(rd) {
            return Ok(cpsr);
        }

        let mut cpsr = self.cpsr;
        cpsr.set_ng_nzcv_by(left, right);

        Ok(cpsr)
    }

    fn _alu_test<Operation>(
        &mut self,
        left: u32,
        right: u32,
        op2: Op2,
        rd: RegisterType,
        f: Operation,
    ) -> Result<Psr>
    where
        Operation: Fn(u32, u32) -> u32,
    {
        let result = f(left, right);

        if rd == RegisterType::PC {
            self.pc = result;
            self.pc_invalidated = true;
        }

        if let Some(cpsr) = self._alu_pc_special_if_needed(rd) {
            return Ok(cpsr);
        }

        let mut cpsr = self.cpsr;
        cpsr.set_nz_by(result);

        if let Some(c) = op2.c {
            cpsr.set_c(c);
        }

        Ok(cpsr)
    }

    fn and(&mut self, operand: u32, rd: RegisterType, op2: Op2) -> Result<Psr> {
        self._alu_logical(operand, op2.val, rd, op2, |left, right| left & right)
    }

    fn eor(&mut self, operand: u32, rd: RegisterType, op2: Op2) -> Result<Psr> {
        self._alu_logical(operand, op2.val, rd, op2, |left, right| left ^ right)
    }

    fn sub(&mut self, operand: u32, rd: RegisterType, op2: Op2) -> Result<Psr> {
        self._alu_arit_ng(operand, op2.val, rd, |left, right| left.wrapping_sub(right))
    }

    fn rsb(&mut self, operand: u32, rd: RegisterType, op2: Op2) -> Result<Psr> {
        self._alu_arit_ng(op2.val, operand, rd, |left, right| left.wrapping_sub(right))
    }

    fn add(&mut self, operand: u32, rd: RegisterType, op2: Op2) -> Result<Psr> {
        self._alu_arit_pl(operand, op2.val, rd, |left, right| left.wrapping_add(right))
    }

    fn adc(&mut self, operand: u32, rd: RegisterType, op2: Op2) -> Result<Psr> {
        let val1 = operand;
        let val2 = op2.val;
        let val3 = self.cpsr.c() as u32;

        let (result1, c1) = val1.overflowing_add(val2);
        let (result2, c2) = result1.overflowing_add(val3);
        let v1 = val1.is_overflow_add(val2);
        let v2 = result1.is_overflow_add(val3);

        self.set_r(rd, result2)?;

        if let Some(cpsr) = self._alu_pc_special_if_needed(rd) {
            return Ok(cpsr);
        }

        let mut cpsr = self.cpsr;
        cpsr.set_nz_by(result2);
        cpsr.set_c(c1 | c2);
        cpsr.set_v(v1 | v2);

        Ok(cpsr)
    }

    fn _sbc(&mut self, left: u32, right: u32, rd: RegisterType) -> Result<Psr> {
        let val1 = left;
        let val2 = right;
        let val3 = !self.cpsr.c() as u32;

        let (result1, c1) = val1.overflowing_sub(val2);
        let (result2, c2) = result1.overflowing_sub(val3);
        let v1 = val1.is_overflow_sub(val2);
        let v2 = result1.is_overflow_sub(val3);

        self.set_r(rd, result2)?;

        if let Some(cpsr) = self._alu_pc_special_if_needed(rd) {
            return Ok(cpsr);
        }

        let mut cpsr = self.cpsr;
        cpsr.set_nz_by(result2);
        cpsr.set_c(!c1 & !c2);
        cpsr.set_v(v1 | v2);

        Ok(cpsr)
    }

    fn sbc(&mut self, operand: u32, rd: RegisterType, op2: Op2) -> Result<Psr> {
        self._sbc(operand, op2.val, rd)
    }

    fn rsc(&mut self, operand: u32, rd: RegisterType, op2: Op2) -> Result<Psr> {
        self._sbc(op2.val, operand, rd)
    }

    fn tst(&mut self, operand: u32, op2: Op2, rd: RegisterType) -> Result<Psr> {
        self._alu_test(operand, op2.val, op2, rd, |left, right| left & right)
    }

    fn teq(&mut self, operand: u32, op2: Op2, rd: RegisterType) -> Result<Psr> {
        self._alu_test(operand, op2.val, op2, rd, |left, right| left ^ right)
    }

    fn cmp(&mut self, operand: u32, op2: Op2, rd: RegisterType) -> Result<Psr> {
        let left = operand;
        let right = op2.val;

        if rd == RegisterType::PC {
            self.pc_invalidated = true;
        }

        if let Some(cpsr) = self._alu_pc_special_if_needed(rd) {
            return Ok(cpsr);
        }

        let mut cpsr = self.cpsr;
        cpsr.set_ng_nzcv_by(left, right);

        Ok(cpsr)
    }

    fn cmn(&mut self, operand: u32, op2: Op2, rd: RegisterType) -> Result<Psr> {
        let left = operand;
        let right = op2.val;

        if rd == RegisterType::PC {
            self.pc_invalidated = true;
        }

        if let Some(cpsr) = self._alu_pc_special_if_needed(rd) {
            return Ok(cpsr);
        }

        let mut cpsr = self.cpsr;
        cpsr.set_pl_nzcv_by(left, right);

        Ok(cpsr)
    }

    fn orr(&mut self, operand: u32, rd: RegisterType, op2: Op2) -> Result<Psr> {
        self._alu_logical(operand, op2.val, rd, op2, |left, right| left | right)
    }

    fn mov(&mut self, rd: RegisterType, op2: Op2) -> Result<Psr> {
        // trace!("MOV rd: {:?} = op2 {:08X}", rd, op2);
        self._alu_logical(op2.val, 0, rd, op2, |left, _| left)
    }

    fn bic(&mut self, operand: u32, rd: RegisterType, op2: Op2) -> Result<Psr> {
        self._alu_logical(operand, op2.val, rd, op2, |left, right| left & !right)
    }

    fn mvn(&mut self, rd: RegisterType, op2: Op2) -> Result<Psr> {
        self._alu_logical(op2.val, 0, rd, op2, |left, _| !left)
    }

    fn multiply(
        &mut self,
        opcode: u32,
        state: bool,
        rd: RegisterType,
        rn: RegisterType,
        rs: RegisterType,
        rm: RegisterType,
    ) -> Result<()> {
        let cpsr = match opcode {
            // MUL
            0b0000 => self.mul(rd, rm, rs),
            // MLA
            0b0001 => self.mla(rd, rn, rm, rs),
            // UMULL
            0b0100 => self.umull(rd, rn, rm, rs),
            // UMLAL
            0b0101 => self.umlal(rd, rn, rm, rs),
            // SMULL
            0b0110 => self.smull(rd, rn, rm, rs),
            // SMLAL
            0b0111 => self.smlal(rd, rn, rm, rs),
            _ => panic!("invalid multiply opcode"),
        }?;

        if state {
            self.cpsr = cpsr;
        }

        Ok(())
    }

    fn mul(&mut self, rd: RegisterType, rm: RegisterType, rs: RegisterType) -> Result<Psr> {
        let left = self.get_r(rm);
        let right = self.get_r(rs);

        let result = left.wrapping_mul(right);

        self.set_r(rd, result)?;

        let mut cpsr = self.cpsr;
        cpsr.set_nz_by(result);

        Ok(cpsr)
    }

    fn mla(
        &mut self,
        rd: RegisterType,
        rn: RegisterType,
        rm: RegisterType,
        rs: RegisterType,
    ) -> Result<Psr> {
        let left = self.get_r(rm);
        let right = self.get_r(rs);
        let add = self.get_r(rn);

        let result = add.wrapping_add(left.wrapping_mul(right));

        self.set_r(rd, result)?;

        let mut cpsr = self.cpsr;
        cpsr.set_nz_by(result);

        Ok(cpsr)
    }

    fn umull(
        &mut self,
        rd: RegisterType,
        rn: RegisterType,
        rm: RegisterType,
        rs: RegisterType,
    ) -> Result<Psr> {
        let left = self.get_r(rm) as u64;
        let right = self.get_r(rs) as u64;

        let result = left.wrapping_mul(right);

        self.set_r(rd, (result >> 32) as u32)?;
        self.set_r(rn, result as u32)?;

        let mut cpsr = self.cpsr;
        cpsr.set_nz_by(result);

        Ok(cpsr)
    }

    fn umlal(
        &mut self,
        rd: RegisterType,
        rn: RegisterType,
        rm: RegisterType,
        rs: RegisterType,
    ) -> Result<Psr> {
        let left = self.get_r(rm) as u64;
        let right = self.get_r(rs) as u64;
        let add = ((self.get_r(rd) as u64) << 32) | (self.get_r(rn) as u64);

        let result = add.wrapping_add(left.wrapping_mul(right));

        self.set_r(rd, (result >> 32) as u32)?;
        self.set_r(rn, result as u32)?;

        let mut cpsr = self.cpsr;
        cpsr.set_nz_by(result);

        Ok(cpsr)
    }

    fn smull(
        &mut self,
        rd: RegisterType,
        rn: RegisterType,
        rm: RegisterType,
        rs: RegisterType,
    ) -> Result<Psr> {
        let left = self.get_r(rm) as i64;
        let right = self.get_r(rs) as i64;

        let result = left.wrapping_mul(right);

        self.set_r(rd, (result >> 32) as u32)?;
        self.set_r(rn, result as u32)?;

        let mut cpsr = self.cpsr;
        cpsr.set_nz_by(result);

        Ok(cpsr)
    }

    fn smlal(
        &mut self,
        rd: RegisterType,
        rn: RegisterType,
        rm: RegisterType,
        rs: RegisterType,
    ) -> Result<Psr> {
        let left = self.get_r(rm) as i64;
        let right = self.get_r(rs) as i64;
        let add = (((self.get_r(rd) as u64) << 32) | (self.get_r(rn) as u64)) as i64;

        let result = add.wrapping_add(left.wrapping_mul(right));

        self.set_r(rd, (result >> 32) as u32)?;
        self.set_r(rn, result as u32)?;

        let mut cpsr = self.cpsr;
        cpsr.set_nz_by(result);

        Ok(cpsr)
    }

    fn mrs(&mut self, p: u32, r: RegisterType) -> Result<()> {
        let psr = if p == 0 { self.cpsr } else { self.get_spsr() };

        self.set_r(r, psr.0)?;

        Ok(())
    }

    fn msr(&mut self, p: u32, f: bool, _s: bool, _x: bool, t: bool, val: Psr) -> Result<()> {
        let mut psr = if p == 0 { self.cpsr } else { self.get_spsr() };

        if f {
            psr.set_n(val.n());
            psr.set_z(val.z());
            psr.set_c(val.c());
            psr.set_v(val.v());
        }

        if t && self.cpsr.mode() != Mode::User {
            psr.set_i(val.i());
            psr.set_f(val.f());
            psr.set_t(val.t());
            psr.set_mode(val.mode().into());
        }

        if p == 0 {
            let orig_t = self.cpsr.t();

            self.cpsr = psr;

            if self.cpsr.t() != orig_t {
                self.pc_invalidated = true;
            }
        } else {
            self.set_spsr(psr);
        };

        Ok(())
    }

    fn ldr(&mut self, addr: u32, size: u32, rd: RegisterType) -> Result<()> {
        let result = match size {
            1 => self.bus.read_8(addr)? as u32,
            2 => self.bus.read_16(addr)? as u32,
            4 => self.bus.read_32(addr)? as u32,
            _ => bail!("invalid byte size"),
        };

        self.set_r(rd, result)?;

        Ok(())
    }

    fn str(&mut self, addr: u32, size: u32, rd: RegisterType) -> Result<()> {
        match size {
            1 => {
                self.bus.write_8(addr, self.get_r(rd) as u8)?;
            }
            2 => {
                self.bus.write_16(addr, self.get_r(rd) as u16)?;
            }
            4 => {
                self.bus.write_32(addr, self.get_r(rd) as u32)?;
            }
            _ => bail!("invalid byte size"),
        }

        Ok(())
    }

    fn single_data_transfer_pre(
        &mut self,
        up_down: u32,
        byte_word: u32,
        writeback: bool,
        load_store: u32,
        rn: RegisterType,
        rd: RegisterType,
        op2: Op2,
    ) -> Result<()> {
        let base_addr = self.get_r(rn);
        let offset = op2.val;

        let addr = if up_down == 1 {
            // up: add to base addr
            base_addr.wrapping_add(offset)
        } else {
            // down: subtract from base addr
            base_addr.wrapping_sub(offset)
        };

        let size = if byte_word == 1 { 1 } else { 4 };

        if load_store == 1 {
            self.ldr(addr, size, rd)?;
        } else {
            self.str(addr, size, rd)?;
        }

        if writeback {
            self.set_r(rn, addr)?;
        }

        Ok(())
    }

    fn single_data_transfer_post(
        &mut self,
        up_down: u32,
        byte_word: u32,
        _memory: u32,
        load_store: u32,
        rn: RegisterType,
        rd: RegisterType,
        op2: Op2,
    ) -> Result<()> {
        let addr = self.get_r(rn);

        let size = if byte_word == 1 { 1 } else { 4 };

        if load_store == 1 {
            self.ldr(addr, size, rd)?;
        } else {
            self.str(addr, size, rd)?;
        }

        let new_addr = if up_down == 1 {
            addr.wrapping_add(op2.val)
        } else {
            addr.wrapping_sub(op2.val)
        };

        self.set_r(rn, new_addr)?;

        Ok(())
    }

    fn halfword_transfer_offset(&self, immediate: bool, upper: u32, lower: u32) -> u32 {
        if immediate {
            (upper as u32) << 4 | (lower as u32)
        } else {
            self.get_r(RegisterType::from(lower))
        }
    }

    fn halfword_single_data_transfer_pre(
        &mut self,
        up_down: u32,
        writeback: bool,
        load_store: u32,
        rn: RegisterType,
        rd: RegisterType,
        offset: u32,
    ) -> Result<()> {
        let base_addr = self.get_r(rn);

        let addr = if up_down == 1 {
            base_addr.wrapping_add(offset)
        } else {
            base_addr.wrapping_sub(offset)
        };

        if load_store == 1 {
            self.ldr(addr, 2, rd)?;
        } else {
            self.str(addr, 2, rd)?;
        }

        if writeback {
            self.set_r(rn, addr)?;
        }

        Ok(())
    }

    fn halfword_single_data_transfer_post(
        &mut self,
        up_down: u32,
        load_store: u32,
        rn: RegisterType,
        rd: RegisterType,
        offset: u32,
    ) -> Result<()> {
        let addr = self.get_r(rn);

        if load_store == 1 {
            self.ldr(addr, 2, rd)?;
        } else {
            self.str(addr, 2, rd)?;
        }

        let new_addr = if up_down == 1 {
            addr.wrapping_add(offset)
        } else {
            addr.wrapping_sub(offset)
        };

        self.set_r(rn, new_addr)?;

        Ok(())
    }

    fn block_data_transfer_pre(
        &mut self,
        up_down: u32,
        _state: bool,
        writeback: bool,
        load_store: u32,
        rn: RegisterType,
        rlist: Vec<RegisterType>,
    ) -> Result<()> {
        // trace!("Block Data Transfer Pre-Indexing");

        let base_addr = self.get_r(rn);
        let mut offset: i32 = 0;

        let mut rlist = rlist.clone();

        if up_down == 0 {
            rlist.reverse();
        };

        for r in rlist.into_iter() {
            offset = if up_down == 1 {
                offset.wrapping_add(4)
            } else {
                offset.wrapping_sub(4)
            };

            let addr = ((base_addr as i32) + offset) as u32;

            if load_store == 1 {
                self.ldr(addr, 4, r)?;
            } else {
                self.str(addr, 4, r)?;
            };
        }

        if writeback {
            self.set_r(rn, ((base_addr as i32) + offset) as u32)?;
        }

        Ok(())
    }

    fn block_data_transfer_post(
        &mut self,
        up_down: u32,
        _state: bool,
        writeback: bool,
        load_store: u32,
        rn: RegisterType,
        rlist: Vec<RegisterType>,
    ) -> Result<()> {
        // trace!("Block Data Transfer Post-Indexing");

        let base_addr = self.get_r(rn);
        let mut offset: i32 = 0;

        let mut rlist = rlist.clone();

        if up_down == 0 {
            rlist.reverse();
        };

        for r in rlist.into_iter() {
            let addr = ((base_addr as i32) + offset) as u32;

            if load_store == 1 {
                self.ldr(addr, 4, r)?;
            } else {
                self.str(addr, 4, r)?;
            };

            offset = if up_down == 1 {
                offset.wrapping_add(4)
            } else {
                offset.wrapping_sub(4)
            };
        }

        if writeback {
            self.set_r(rn, ((base_addr as i32) + offset) as u32)?;
        }

        Ok(())
    }

    fn swp(
        &mut self,
        byte_word: u32,
        rn: RegisterType,
        rd: RegisterType,
        rm: RegisterType,
    ) -> Result<()> {
        let addr = self.get_r(rn);
        let source = self.get_r(rm);

        let result = if byte_word == 1 {
            self.bus.read_8(addr)? as u32
        } else {
            self.bus.read_32(addr)?
        };

        if byte_word == 1 {
            self.bus.write_8(addr, source as u8)?;
        } else {
            self.bus.write_32(addr, source)?;
        }

        self.set_r(rd, result)?;

        Ok(())
    }

    fn thumb_lsl(&mut self, rs: RegisterType, rd: RegisterType, offset: u16) -> Result<()> {
        let source = self.get_r(rs);

        let ShiftResult(result, c) = source.lsl(offset as u32, self.cpsr.c());

        self.set_r(rd, result)?;

        self.cpsr.set_nz_by(result);
        self.cpsr.set_c(c);

        Ok(())
    }

    fn thumb_lsr(&mut self, rs: RegisterType, rd: RegisterType, offset: u16) -> Result<()> {
        let source = self.get_r(rs);

        let ShiftResult(result, c) = source.lsr(offset as u32, offset == 0);

        self.set_r(rd, result)?;

        self.cpsr.set_nz_by(result);
        self.cpsr.set_c(c);

        Ok(())
    }

    fn thumb_asr(&mut self, rs: RegisterType, rd: RegisterType, offset: u16) -> Result<()> {
        let source = self.get_r(rs);

        let ShiftResult(result, c) = source.asr(offset as u32, offset == 0);

        self.set_r(rd, result as u32)?;

        self.cpsr.set_nz_by(result);
        self.cpsr.set_c(c);

        Ok(())
    }

    fn thumb_add(&mut self, val: u32, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rs);
        let right = val;
        let result = left.wrapping_add(right);

        self.set_r(rd, result)?;
        self.cpsr.set_pl_nzcv_by(left, right);

        Ok(())
    }

    fn thumb_sub(&mut self, val: u32, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rs);
        let right = val;
        let result = left.wrapping_sub(right);

        self.set_r(rd, result)?;
        self.cpsr.set_ng_nzcv_by(left, right);

        Ok(())
    }

    fn thumb_mov(&mut self, rd: RegisterType, n: u32, f: bool) -> Result<()> {
        let result = if rd == RegisterType::PC { n & !1u32 } else { n };

        self.set_r(rd, result)?;

        if f {
            self.cpsr.set_nz_by(result);
        }

        Ok(())
    }

    fn thumb_cmp(&mut self, rd: RegisterType, n: u32) -> Result<()> {
        let left = self.get_r(rd);
        let right = n as u32;

        self.cpsr.set_ng_nzcv_by(left, right);

        Ok(())
    }

    fn thumb_add_mov(&mut self, rd: RegisterType, n: u16) -> Result<()> {
        let left = self.get_r(rd);
        let right = n as u32;
        let result = left.wrapping_add(right);

        self.set_r(rd, result)?;
        self.cpsr.set_pl_nzcv_by(left, right);

        Ok(())
    }

    fn thumb_sub_mov(&mut self, rd: RegisterType, n: u16) -> Result<()> {
        let left = self.get_r(rd);
        let right = n as u32;
        let result = left.wrapping_sub(right);

        self.set_r(rd, result)?;
        self.cpsr.set_ng_nzcv_by(left, right);

        Ok(())
    }

    fn thumb_and_mov(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);
        let result = left & right;

        self.set_r(rd, result)?;
        self.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_eor_mov(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);
        let result = left ^ right;

        self.set_r(rd, result)?;
        self.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_lsl_mov(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);

        let ShiftResult(result, c) = left.lsl(right, self.cpsr.c());

        self.set_r(rd, result)?;
        self.cpsr.set_nz_by(result);
        self.cpsr.set_c(c);

        Ok(())
    }

    fn thumb_lsr_mov(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);
        let ShiftResult(result, c) = left.lsr(right, false);

        self.set_r(rd, result)?;
        self.cpsr.set_nz_by(result);
        self.cpsr.set_c(c);

        Ok(())
    }

    fn thumb_asr_mov(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);
        let ShiftResult(result, c) = left.asr(right, false);

        self.set_r(rd, result)?;
        self.cpsr.set_nz_by(result);
        self.cpsr.set_c(c);

        Ok(())
    }

    fn thumb_adc_mov(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);
        let c = self.cpsr.c() as u32;

        let (result1, c1) = left.overflowing_add(right);
        let (result2, c2) = result1.overflowing_add(c);
        let v1 = left.is_overflow_add(right);
        let v2 = result1.is_overflow_add(c);

        self.set_r(rd, result2)?;
        self.cpsr.set_nz_by(result2);
        self.cpsr.set_c(c1 | c2);
        self.cpsr.set_v(v1 | v2);

        Ok(())
    }

    fn thumb_sbc_mov(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);
        let c = self.cpsr.c() as u32;

        let (result1, c1) = left.overflowing_sub(right);
        let (result2, c2) = result1.overflowing_sub(c);
        let v1 = left.is_overflow_sub(right);
        let v2 = result1.is_overflow_sub(c);

        self.set_r(rd, result2)?;
        self.cpsr.set_nz_by(result2);
        self.cpsr.set_c(c1 | c2);
        self.cpsr.set_v(v1 | v2);

        Ok(())
    }

    fn thumb_ror_mov(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);
        let ShiftResult(result, c) = left.ror(right, self.cpsr.c(), false);

        self.set_r(rd, result)?;
        self.cpsr.set_nz_by(result);
        self.cpsr.set_c(c);

        Ok(())
    }

    fn thumb_tst(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);
        let result = left & right;

        self.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_neg(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = 0u32;
        let right = self.get_r(rs);
        let result = left.wrapping_sub(right);

        self.set_r(rd, result)?;
        self.cpsr.set_ng_nzcv_by(left, right);

        Ok(())
    }

    fn thumb_cmn(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);

        self.cpsr.set_pl_nzcv_by(left, right);

        Ok(())
    }

    fn thumb_orr(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);
        let result = left | right;

        self.set_r(rd, result)?;
        self.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_mul(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);
        let result = left * right;

        self.set_r(rd, result)?;
        self.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_bic(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);
        let result = left & !right;

        self.set_r(rd, result)?;
        self.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_mvn(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let val = self.get_r(rs);
        let result = !val;

        self.set_r(rd, result)?;
        self.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_push(&mut self, registers: Vec<RegisterType>) -> Result<()> {
        let mut addr = self.get_r(RegisterType::SP);

        for r in registers.into_iter().rev() {
            let val = self.get_r(r);

            addr = addr.wrapping_sub(4);

            self.bus.write_32(addr, val)?;
        }

        self.set_r(RegisterType::SP, addr)?;

        Ok(())
    }

    fn thumb_pop(&mut self, registers: Vec<RegisterType>) -> Result<()> {
        self.thumb_ldmia(RegisterType::SP, registers)
    }

    fn thumb_stmia(&mut self, rb: RegisterType, registers: Vec<RegisterType>) -> Result<()> {
        let mut addr = self.get_r(rb);

        for r in registers {
            let val = self.get_r(r);

            self.bus.write_32(addr, val)?;

            addr = addr.wrapping_add(4);
        }

        self.set_r(rb, addr)?;

        Ok(())
    }

    fn thumb_ldmia(&mut self, rb: RegisterType, registers: Vec<RegisterType>) -> Result<()> {
        let mut addr = self.get_r(rb);

        for r in registers {
            let val = self.bus.read_32(addr)?;

            self.set_r(r, val)?;

            addr = addr.wrapping_add(4);
        }

        self.set_r(rb, addr)?;

        Ok(())
    }

    fn thumb_branch(&mut self, offset: i16) -> Result<()> {
        let base_addr = self.pc;
        let offset = (offset as i32) << 1;
        let addr = (base_addr as i32).wrapping_add(offset) as u32;

        self.pc = addr;
        self.pc_invalidated = true;

        Ok(())
    }

    fn thumb_long_branch_first(&mut self, offset: i16) -> Result<()> {
        let base_addr = self.pc;
        let offset = (offset as i32) << 12;
        let addr = (base_addr as i32).wrapping_add(offset) as u32;

        self.set_r(RegisterType::LR, addr)?;

        Ok(())
    }

    fn thumb_long_branch_second(&mut self, offset: u16) -> Result<()> {
        let orig_pc = self.pc;
        let base_addr = self.get_r(RegisterType::LR);
        let offset = (offset as u32) << 1;
        let addr = base_addr.wrapping_add(offset);

        self.pc = addr;
        self.pc_invalidated = true;
        self.set_r(RegisterType::LR, orig_pc - 1)?;

        Ok(())
    }
}

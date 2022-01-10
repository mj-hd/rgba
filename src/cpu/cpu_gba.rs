use crate::bus::{AccessType, Bus};
use anyhow::{bail, Result};

use log::{debug, trace};

use super::{
    psr::{Cond, Psr},
    register::{CommonRegister, FiqRegister, ModeRegister, Register, RegisterType},
    Mode,
};

#[derive(Debug)]
pub enum Exception {
    Reset,
    Undefined,
    Swi,
    PrefetchAbort,
    DataAbort,
    AddressExceeded,
    Irq,
    Fiq,
}

pub struct Cpu {
    common_r: [CommonRegister; 8],
    fiq_r: [FiqRegister; 5],
    mode_r: [ModeRegister; 2],

    pub(super) pc: u32,
    pub(super) pc_invalidated: bool,
    pub(super) cpsr: Psr,
    pub(super) spsr: ModeRegister<Psr>,

    cycles: u32,
    prefetch: [u32; 2],
    halt: bool,

    trace: bool,

    fetch_access_type: AccessType,

    pub bus: Bus,
}

impl Cpu {
    pub fn new(bus: Bus) -> Self {
        Self {
            cycles: 0,
            prefetch: Default::default(),
            halt: false,
            bus,
            common_r: Default::default(),
            fiq_r: Default::default(),
            mode_r: Default::default(),
            pc: 0,
            pc_invalidated: false,
            fetch_access_type: AccessType::NonSeq,
            cpsr: Default::default(),
            spsr: Default::default(),
            trace: false,
        }
    }

    pub fn reset(&mut self) -> Result<()> {
        self.pc = 0x0000_0000;
        self.cpsr.0 = 0x0000_001F;
        self.reset_sp();

        self.refresh_prefetch()?;

        Ok(())
    }

    pub fn reset_skip_bios(&mut self) -> Result<()> {
        self.pc = 0x0800_0000;
        self.cpsr.0 = 0x0000_001F;
        self.reset_sp_skip_bios();

        self.refresh_prefetch()?;

        Ok(())
    }

    pub fn tick(&mut self) -> Result<()> {
        self.cycles = self.cycles.wrapping_add(1);

        if self.pc_invalidated {
            self.refresh_prefetch()?;
            self.pc_invalidated = false;
        }

        self.bus.tick()?;

        if self.bus.dmas.active() {
            return Ok(());
        }

        if self.bus.stalls > 0 {
            self.bus.stalls -= 1;

            return Ok(());
        }

        if self.halt {
            return Ok(());
        }

        let mut register_status = "".to_string();

        if self.check_irq() {
            // TODO: リファクタ
            self.pc = self.pc.wrapping_add(4);
            self.do_exception(Exception::Irq)?;

            return Ok(());
        }

        if self.trace {
            register_status = (0u8..=15u8)
                .map(|r| RegisterType::from(r))
                .map(|r| format!("{:08X}", self.get_r(r)))
                .collect::<Vec<_>>()
                .join(" ");
        }

        let opecode = self.pop_prefetch()?;

        if self.trace {
            trace!(
                "{} cpsr: {:08X} {:04X} prefetch_pc: {:08X}",
                register_status,
                self.cpsr.0,
                opecode,
                self.pc,
            );
        }

        if self.cpsr.t() {
            let opecode = opecode as u16;

            self.do_mnemonic_thumb(opecode)?;
        } else {
            self.do_mnemonic_arm(opecode)?;
        }

        Ok(())
    }

    fn check_irq(&mut self) -> bool {
        if !self.cpsr.i() && self.bus.ime {
            if self.bus.interrupt_enable.lcd_v_blank() && self.bus.interrupt_flag.lcd_v_blank() {
                return true;
            }

            if self.bus.interrupt_enable.lcd_h_blank() && self.bus.interrupt_flag.lcd_h_blank() {
                return true;
            }

            if self.bus.interrupt_enable.timer_0() && self.bus.interrupt_flag.timer_0() {
                return true;
            }

            if self.bus.interrupt_enable.timer_1() && self.bus.interrupt_flag.timer_1() {
                return true;
            }

            if self.bus.interrupt_enable.timer_2() && self.bus.interrupt_flag.timer_2() {
                return true;
            }

            if self.bus.interrupt_enable.timer_3() && self.bus.interrupt_flag.timer_3() {
                return true;
            }

            if self.bus.interrupt_enable.serial() && self.bus.interrupt_flag.serial() {
                return true;
            }

            if self.bus.interrupt_enable.dma_0() && self.bus.interrupt_flag.dma_0() {
                return true;
            }

            if self.bus.interrupt_enable.dma_1() && self.bus.interrupt_flag.dma_1() {
                return true;
            }

            if self.bus.interrupt_enable.dma_2() && self.bus.interrupt_flag.dma_2() {
                return true;
            }

            if self.bus.interrupt_enable.dma_3() && self.bus.interrupt_flag.dma_3() {
                return true;
            }

            if self.bus.interrupt_enable.keypad() && self.bus.interrupt_flag.keypad() {
                return true;
            }

            if self.bus.interrupt_enable.game_pak() && self.bus.interrupt_flag.game_pak() {
                return true;
            }
        }

        return false;
    }

    pub(super) fn do_exception(&mut self, exception: Exception) -> Result<()> {
        match exception {
            Exception::Reset => {
                self.do_interrupt(0x0000_0000, true, Mode::Supervisor.into())?;
            }

            Exception::DataAbort => {
                self.do_interrupt(0x0000_0010, self.cpsr.f(), Mode::Abort.into())?;
            }

            Exception::Fiq => {
                self.do_interrupt(0x0000_001C, true, Mode::Fiq.into())?;
            }

            Exception::Irq => {
                self.do_interrupt(0x0000_0018, self.cpsr.f(), Mode::Irq.into())?;
            }

            Exception::PrefetchAbort => {
                self.do_interrupt(0x0000_000C, self.cpsr.f(), Mode::Abort.into())?;
            }

            Exception::Swi => {
                self.do_interrupt(0x0000_0008, self.cpsr.f(), Mode::Supervisor.into())?;
            }

            Exception::Undefined => {
                self.do_interrupt(0x0000_0004, self.cpsr.f(), Mode::Undefined.into())?;
            }

            Exception::AddressExceeded => {
                self.do_interrupt(0x0000_0014, self.cpsr.f(), Mode::Supervisor.into())?;
            }
        }

        Ok(())
    }

    fn do_interrupt(&mut self, addr: u32, new_f: bool, mode: Mode) -> Result<()> {
        let orig_pc = self.pc;
        let orig_cpsr = self.cpsr;

        self.pc = addr;
        self.pc_invalidated = true; // 1N + 2S
        self.cpsr.set_mode(mode.into());

        self.set_r(RegisterType::LR, orig_pc - self.instr_size())?;

        self.set_spsr(orig_cpsr);

        self.cpsr.set_t(false);
        self.cpsr.set_i(true);
        self.cpsr.set_f(new_f);

        Ok(())
    }

    fn refresh_prefetch(&mut self) -> Result<()> {
        self.fetch_access_type = AccessType::NonSeq;

        let opecode = self.fetch()?; // 1N
        self.prefetch[0] = opecode;

        self.pc = self.pc.wrapping_add(self.instr_size());

        let opecode = self.fetch()?; // 1S
        self.prefetch[1] = opecode;

        Ok(())
    }

    fn pop_prefetch(&mut self) -> Result<u32> {
        self.pc = self.pc.wrapping_add(self.instr_size());

        let next = self.prefetch[0];

        self.prefetch[0] = self.prefetch[1];

        let opecode = self.fetch()?; // 1S
        self.prefetch[1] = opecode;

        Ok(next)
    }

    fn fetch(&mut self) -> Result<u32> {
        let access_type = self.fetch_access_type;
        let opecode = if self.cpsr.t() {
            // THUMB mode
            self.bus.read_16(self.pc, access_type)? as u32
        } else {
            // ARM mode
            self.bus.read_32(self.pc, access_type)?
        };

        self.fetch_access_type = AccessType::Seq;

        Ok(opecode)
    }

    fn instr_size(&self) -> u32 {
        if self.cpsr.t() {
            2
        } else {
            4
        }
    }

    pub(super) fn guard(&self, cond: u32) -> bool {
        let cpsr = self.cpsr;

        Cond::from(cond).guard(cpsr)
    }

    pub(super) fn get_r_by_mode(&self, r: RegisterType, mode: Mode) -> u32 {
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

    pub(super) fn get_r(&self, r: RegisterType) -> u32 {
        self.get_r_by_mode(r, self.cpsr.mode())
    }

    pub(super) fn set_r_by_mode(&mut self, r: RegisterType, mode: Mode, val: u32) -> Result<()> {
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

    pub(super) fn set_r(&mut self, r: RegisterType, val: u32) -> Result<()> {
        self.set_r_by_mode(r, self.cpsr.mode(), val)
    }

    pub(super) fn get_spsr(&self) -> Psr {
        self.spsr.get(self.cpsr.mode())
    }

    pub(super) fn set_spsr(&mut self, psr: Psr) {
        self.spsr.set(self.cpsr.mode(), psr);
    }

    pub(super) fn reset_sp(&mut self) {
        self.mode_r[0].set(Mode::User, 0x0300_7F00);
        self.mode_r[0].set(Mode::Fiq, 0x0000_0000);
        self.mode_r[0].set(Mode::Supervisor, 0x0300_7FE0);
        self.mode_r[0].set(Mode::Abort, 0x0000_0000);
        self.mode_r[0].set(Mode::Irq, 0x0300_7FA0);
        self.mode_r[0].set(Mode::Undefined, 0x0000_0000);
    }

    pub(super) fn reset_sp_skip_bios(&mut self) {
        self.mode_r[0].set(Mode::User, 0x0300_7F00);
        self.mode_r[0].set(Mode::Fiq, 0x0300_7F00);
        self.mode_r[0].set(Mode::Supervisor, 0x0300_7FE0);
        self.mode_r[0].set(Mode::Abort, 0x0300_7F00);
        self.mode_r[0].set(Mode::Irq, 0x0300_7FA0);
        self.mode_r[0].set(Mode::Undefined, 0x0300_7F00);
    }

    pub(super) fn rlist(&self, rlist: u16) -> Vec<RegisterType> {
        let mut registers = vec![];

        for r in 0u16..16u16 {
            if rlist & (1 << r) == 0 {
                continue;
            }

            registers.push(RegisterType::from(r));
        }

        registers
    }

    pub(super) fn rlist_low(&self, rlist: u8) -> Vec<RegisterType> {
        let mut registers = vec![];

        for r in 0u8..8u8 {
            if rlist & (1 << r) == 0 {
                continue;
            }

            registers.push(RegisterType::from(r));
        }

        registers
    }

    pub(super) fn ldr(
        &mut self,
        addr: u32,
        size: u32,
        rd: RegisterType,
        access: AccessType,
    ) -> Result<()> {
        self.ldr_by_mode(addr, size, rd, self.cpsr.mode(), access)
    }

    pub(super) fn ldr_by_mode(
        &mut self,
        addr: u32,
        size: u32,
        rd: RegisterType,
        mode: Mode,
        access: AccessType,
    ) -> Result<()> {
        let result = match size {
            1 => self.bus.read_8(addr, access)? as u32,
            2 => self.bus.read_16(addr, access)? as u32,
            4 => self.bus.read_32(addr, access)? as u32,
            _ => bail!("invalid byte size"),
        };

        self.set_r_by_mode(rd, mode, result)?;

        Ok(())
    }

    pub(super) fn str_by_mode(
        &mut self,
        addr: u32,
        size: u32,
        rd: RegisterType,
        mode: Mode,
        access: AccessType,
    ) -> Result<()> {
        let mut val = self.get_r_by_mode(rd, mode);

        if rd == RegisterType::PC {
            val = val.wrapping_add(4);
        }

        match size {
            1 => {
                self.bus.write_8(addr, val as u8, access)?;
            }
            2 => {
                self.bus.write_16(addr, val as u16, access)?;
            }
            4 => {
                self.bus.write_32(addr, val as u32, access)?;
            }
            _ => bail!("invalid byte size"),
        }

        Ok(())
    }

    pub(super) fn str(
        &mut self,
        addr: u32,
        size: u32,
        rd: RegisterType,
        access: AccessType,
    ) -> Result<()> {
        self.str_by_mode(addr, size, rd, self.cpsr.mode(), access)
    }

    pub(super) fn swi(&mut self) -> Result<()> {
        self.do_exception(Exception::Swi)?; // 1N + 2S

        Ok(())
    }

    pub(super) fn bkpt(&mut self) -> Result<()> {
        debug!("BKPT");

        Ok(())
    }

    pub(super) fn und(&mut self) -> Result<()> {
        self.bus.idle_stall(1); // 1I
        self.do_exception(Exception::Undefined)?; // 1N + 2S

        Ok(())
    }

    pub(super) fn bx(&mut self, r: RegisterType) -> Result<()> {
        let val = self.get_r(r);

        if val & 1 == 1 {
            self.pc = val - 1;
            self.cpsr.set_t(true);
        } else {
            self.pc = val;
            self.cpsr.set_t(false);
        }

        self.pc_invalidated = true; // 1N + 2S

        Ok(())
    }
}

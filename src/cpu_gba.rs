use crate::{
    arit::{is_overflow_ng, is_overflow_pl},
    bus::Bus,
};
use anyhow::{bail, Result};
use bitfield::bitfield;
use bitmatch::bitmatch;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[derive(Debug, FromPrimitive, Clone, Copy)]
pub enum Registers {
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
}

impl From<u32> for Registers {
    fn from(n: u32) -> Self {
        FromPrimitive::from_u32(n).unwrap_or(Registers::R0)
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
}

impl From<u32> for Mode {
    fn from(n: u32) -> Mode {
        FromPrimitive::from_u32(n).unwrap_or(Mode::User)
    }
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct PSR(u32);
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

impl PSR {
    pub fn set_nz_by<T>(&mut self, result: T)
    where
        T: Eq + Ord + Default,
    {
        self.set_n(result < Default::default());
        self.set_z(result == Default::default());
    }

    pub fn set_pl_nzcv_by(&mut self, left: u32, right: u32) {
        self.set_pl_nzc_by(left, right);
        self.set_pl_v_by(left, right);
    }

    pub fn set_pl_nzc_by(&mut self, left: u32, right: u32) {
        let (result, c) = left.overflowing_add(right);
        self.set_nz_by(result);
        self.set_c(c);
    }

    pub fn set_ng_nzcv_by(&mut self, left: u32, right: u32) {
        self.set_ng_nzc_by(left, right);
        self.set_ng_v_by(left, right);
    }

    pub fn set_ng_nzc_by(&mut self, left: u32, right: u32) {
        let (result, c) = left.overflowing_sub(right);
        self.set_nz_by(result);
        self.set_c(c);
    }

    pub fn set_pl_v_by(&mut self, left: u32, right: u32) {
        let v = is_overflow_pl(left, right);
        self.set_v(v);
    }

    pub fn set_ng_v_by(&mut self, left: u32, right: u32) {
        let v = is_overflow_ng(left, right);
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
        self.r[mode as usize]
    }

    fn set(&mut self, mode: Mode, val: T) {
        self.r[mode as usize] = val;
    }
}

#[derive(Default)]
pub struct R {
    common_r: [CommonRegister; 7],
    fiq_r: [FiqRegister; 5],
    mode_r: [ModeRegister; 2],

    pc: u32,
    cpsr: PSR,
    spsr: ModeRegister<PSR>,
}

impl R {
    pub fn get(&self, r: Registers) -> u32 {
        let mode = self.cpsr.mode();

        match r {
            Registers::R0 => self.common_r[0].get(mode),
            Registers::R1 => self.common_r[1].get(mode),
            Registers::R2 => self.common_r[2].get(mode),
            Registers::R3 => self.common_r[3].get(mode),
            Registers::R4 => self.common_r[4].get(mode),
            Registers::R5 => self.common_r[5].get(mode),
            Registers::R6 => self.common_r[6].get(mode),
            Registers::R7 => self.common_r[7].get(mode),
            Registers::R8 => self.fiq_r[0].get(mode),
            Registers::R9 => self.fiq_r[1].get(mode),
            Registers::R10 => self.fiq_r[2].get(mode),
            Registers::R11 => self.fiq_r[3].get(mode),
            Registers::R12 => self.fiq_r[4].get(mode),
            Registers::SP => self.mode_r[0].get(mode),
            Registers::LR => self.mode_r[1].get(mode),
        }
    }

    pub fn set(&mut self, r: Registers, val: u32) {
        let mode = self.cpsr.mode();

        match r {
            Registers::R0 => self.common_r[0].set(mode, val),
            Registers::R1 => self.common_r[1].set(mode, val),
            Registers::R2 => self.common_r[2].set(mode, val),
            Registers::R3 => self.common_r[3].set(mode, val),
            Registers::R4 => self.common_r[4].set(mode, val),
            Registers::R5 => self.common_r[5].set(mode, val),
            Registers::R6 => self.common_r[6].set(mode, val),
            Registers::R7 => self.common_r[7].set(mode, val),
            Registers::R8 => self.fiq_r[0].set(mode, val),
            Registers::R9 => self.fiq_r[1].set(mode, val),
            Registers::R10 => self.fiq_r[2].set(mode, val),
            Registers::R11 => self.fiq_r[3].set(mode, val),
            Registers::R12 => self.fiq_r[4].set(mode, val),
            Registers::SP => self.mode_r[0].set(mode, val),
            Registers::LR => self.mode_r[1].set(mode, val),
        }
    }

    pub fn get_spsr(&self) -> PSR {
        self.spsr.get(self.cpsr.mode())
    }

    pub fn set_spsr(&mut self, psr: PSR) {
        self.spsr.set(self.cpsr.mode(), psr);
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

pub struct Cpu {
    r: R,

    cycles: u32,
    stalls: u32,
    halt: bool,

    bus: Bus,
}

impl Cpu {
    pub fn new(bus: Bus) -> Self {
        Self {
            r: Default::default(),
            cycles: 0,
            stalls: 0,
            halt: false,
            bus,
        }
    }

    pub fn reset(&mut self) -> Result<()> {
        // TODO
        Ok(())
    }

    pub fn tick(&mut self) -> Result<()> {
        self.cycles = self.cycles.wrapping_add(1);

        self.bus.tick()?;

        if self.stalls > 0 {
            self.stalls -= 1;

            return Ok(());
        }

        self.interrupt()?;

        if self.halt {
            return Ok(());
        }

        let opecode = self.bus.read_32(self.r.pc)?;

        self.r.pc = self.r.pc.wrapping_add(4);

        self.do_mnemonic(opecode)?;

        Ok(())
    }

    pub fn interrupt(&mut self) -> Result<()> {
        // TODO
        Ok(())
    }

    fn guard(&self, cond: u32) -> bool {
        let cpsr = self.r.cpsr;

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
            Cond::Le => cpsr.z() && (cpsr.n() != cpsr.v()),
            Cond::Al => true,
            Cond::Nv => false,
        }
    }

    #[bitmatch]
    fn op2_im(&self, p: u32) -> (u32, bool) {
        #[bitmatch]
        let "ssss_nnnnnnnn" = p;

        (n.rotate_right(s), n & 1 == 1)
    }

    #[bitmatch]
    fn op2_reg(&self, p: u32) -> (u32, bool) {
        #[bitmatch]
        let "ssss_sttfrrrr" = p;

        let val = self.r.get(Registers::from(r));

        let shift = if f == 1 {
            let r = s >> 1;
            self.r.get(Registers::from(r)) as u8 as u32
        } else {
            s
        };

        match t {
            // LSL
            0b00 => val.overflowing_shl(shift),
            // LSR
            0b01 => val.overflowing_shr(shift),
            // ASR
            0b10 => {
                let (result, c) = (val as i32).overflowing_shr(shift);

                (result as u32, c)
            }
            // ROR
            0b11 => (val.rotate_right(shift), val & 1 == 1),
            _ => panic!("invalid op2 shift type"),
        }
    }

    fn op2(&self, i: u32, p: u32) -> (u32, bool) {
        if i == 1 {
            self.op2_im(p)
        } else {
            self.op2_reg(p)
        }
    }

    #[bitmatch]
    pub fn do_mnemonic(&mut self, opecode: u32) -> Result<()> {
        #[bitmatch]
        match &opecode {
            // B
            "cccc1010_nnnnnnnn_nnnnnnnn_nnnnnnnn" if self.guard(c) => self.b(n as i32),

            // BL
            "cccc1011_nnnnnnnn_nnnnnnnn_nnnnnnnn" if self.guard(c) => self.bl(n as i32),

            // BX
            "cccc0001_00101111_11111111_0001rrrr" if self.guard(c) => self.bx(Registers::from(r)),

            // SWI
            "cccc1111_nnnnnnnn_nnnnnnnn_nnnnnnnn" if self.guard(c) => self.swi(n),

            // BKPT
            "cccc0001_0010nnnn_nnnnnnnn_0111mmmm" if self.guard(c) => self.bkpt(n << 12 | m),

            // UND
            "cccc011?_????????_????????_???1????" if self.guard(c) => self.und(),

            // MRS
            "cccc0001_0p001111_rrrr0000_00000000" if self.guard(c) => {
                self.mrs(p, Registers::from(r))
            }

            // MSR Register
            "cccc0001_0p10fsxc_11110000_0000rrrr" if self.guard(c) => {
                let val = self.r.get(Registers::from(r));

                self.msr(p, f == 1, s == 1, x == 1, c == 1, PSR(val))
            }

            // MSR Immediate
            "cccc0011_0p10fsxc_1111iiii_jjjjjjjj" if self.guard(c) => {
                let val = j.rotate_right(i);

                self.msr(p, f == 1, s == 1, x == 1, c == 1, PSR(val))
            }

            // ALU
            "cccc00io_ooosrrrr_ggggpppp_pppppppp" if self.guard(c) => self.alu(
                o,
                s == 1,
                Registers::from(r),
                Registers::from(g),
                self.op2(i, p),
            ),

            // Multiply
            "cccc000o_ooosrrrr_ggggffff_1001hhhh" if self.guard(c) => self.multiply(
                o,
                s == 1,
                Registers::from(r),
                Registers::from(g),
                Registers::from(f),
                Registers::from(h),
            ),

            // Single Data Transfer - post-indexing
            "cccc01i0_ubtlrrrr_ggggpppp_pppppppp" if self.guard(c) => self
                .single_data_transfer_post(
                    u,
                    b,
                    t,
                    l,
                    Registers::from(r),
                    Registers::from(g),
                    self.op2(i, p),
                ),

            // Single Data Transfer - pre-indexing
            "cccc01i1_ubwlrrrr_ggggpppp_pppppppp" if self.guard(c) => self
                .single_data_transfer_pre(
                    u,
                    b,
                    w == 1,
                    l,
                    Registers::from(r),
                    Registers::from(g),
                    self.op2(i, p),
                ),

            _ => Ok(()),
        }
    }

    fn b(&mut self, nn: i32) -> Result<()> {
        self.r.pc = self.r.pc.wrapping_add(4);
        self.r.pc = self.r.pc.wrapping_add((nn * 4) as u32);

        Ok(())
    }

    fn bl(&mut self, nn: i32) -> Result<()> {
        self.r.set(Registers::LR, self.r.pc);

        self.r.pc = self.r.pc.wrapping_add(4);
        self.r.pc = self.r.pc.wrapping_add((nn * 4) as u32);

        Ok(())
    }

    fn bx(&mut self, r: Registers) -> Result<()> {
        let val = self.r.get(r);

        self.r.pc = val;
        self.r.cpsr.set_t(val & 1 == 1);

        Ok(())
    }

    fn swi(&mut self, _: u32) -> Result<()> {
        unimplemented!("SWI");
    }

    fn bkpt(&mut self, _: u32) -> Result<()> {
        unimplemented!("BKPT");
    }

    fn und(&mut self) -> Result<()> {
        unimplemented!("UND");
    }

    fn alu(
        &mut self,
        opcode: u32,
        state: bool,
        rn: Registers,
        rd: Registers,
        op2: (u32, bool),
    ) -> Result<()> {
        let cpsr = match opcode {
            // AND
            0b0000 => self.and(rn, rd, op2),
            // EOR
            0b0001 => self.eor(rn, rd, op2),
            // SUB
            0b0010 => self.sub(rn, rd, op2),
            // RSB
            0b0011 => self.rsb(rn, rd, op2),
            // ADD
            0b0100 => self.add(rn, rd, op2),
            // ADC
            0b0101 => self.adc(rn, rd, op2),
            // SBC
            0b0110 => self.sbc(rn, rd, op2),
            // RSC
            0b0111 => self.rsc(rn, rd, op2),
            // TST
            0b1000 => self.tst(rn, op2),
            // TEQ
            0b1001 => self.teq(rn, op2),
            // CMP
            0b1010 => self.cmp(rn, op2),
            // CMN
            0b1011 => self.cmn(rn, op2),
            // ORR
            0b1100 => self.orr(rn, rd, op2),
            // MOV
            0b1101 => self.mov(rd, op2),
            // BIC
            0b1110 => self.bic(rn, rd, op2),
            // MVN
            0b1111 => self.mvn(rd, op2),
            _ => bail!("invalid alu opcode"),
        }?;

        if state {
            self.r.cpsr = cpsr;
        }

        Ok(())
    }

    fn _alu_logical<Operation>(
        &mut self,
        left: u32,
        right: u32,
        rd: Registers,
        c: bool,
        f: Operation,
    ) -> Result<PSR>
    where
        Operation: Fn(u32, u32) -> u32,
    {
        let result = f(left, right);

        self.r.set(rd, result);

        let mut cpsr = self.r.cpsr;
        cpsr.set_nz_by(result);
        cpsr.set_c(c);

        Ok(cpsr)
    }

    fn _alu_arit_pl<Operation>(
        &mut self,
        left: u32,
        right: u32,
        rd: Registers,
        f: Operation,
    ) -> Result<PSR>
    where
        Operation: Fn(u32, u32) -> u32,
    {
        let result = f(left, right);

        self.r.set(rd, result);

        let mut cpsr = self.r.cpsr;
        cpsr.set_pl_nzcv_by(left, right);

        Ok(cpsr)
    }

    fn _alu_arit_ng<Operation>(
        &mut self,
        left: u32,
        right: u32,
        rd: Registers,
        f: Operation,
    ) -> Result<PSR>
    where
        Operation: Fn(u32, u32) -> u32,
    {
        let result = f(left, right);

        self.r.set(rd, result);

        let mut cpsr = self.r.cpsr;
        cpsr.set_ng_nzcv_by(left, right);

        Ok(cpsr)
    }

    fn _alu_test<Operation>(&mut self, left: u32, right: u32, c: bool, f: Operation) -> Result<PSR>
    where
        Operation: Fn(u32, u32) -> u32,
    {
        let result = f(left, right);

        let mut cpsr = self.r.cpsr;
        cpsr.set_nz_by(result);
        cpsr.set_c(c);

        Ok(cpsr)
    }

    fn and(&mut self, rn: Registers, rd: Registers, (op2, c): (u32, bool)) -> Result<PSR> {
        self._alu_logical(self.r.get(rn), op2, rd, c, |left, right| left & right)
    }

    fn eor(&mut self, rn: Registers, rd: Registers, (op2, c): (u32, bool)) -> Result<PSR> {
        self._alu_logical(self.r.get(rn), op2, rd, c, |left, right| left ^ right)
    }

    fn sub(&mut self, rn: Registers, rd: Registers, (op2, _): (u32, bool)) -> Result<PSR> {
        self._alu_arit_ng(self.r.get(rn), op2, rd, |left, right| left - right)
    }

    fn rsb(&mut self, rn: Registers, rd: Registers, (op2, _): (u32, bool)) -> Result<PSR> {
        self._alu_arit_ng(op2, self.r.get(rn), rd, |left, right| left - right)
    }

    fn add(&mut self, rn: Registers, rd: Registers, (op2, _): (u32, bool)) -> Result<PSR> {
        self._alu_arit_pl(self.r.get(rn), op2, rd, |left, right| left + right)
    }

    fn adc(&mut self, rn: Registers, rd: Registers, (op2, _): (u32, bool)) -> Result<PSR> {
        let val1 = self.r.get(rn);
        let val2 = op2;
        let val3 = self.r.cpsr.c() as u32;

        let (result1, c1) = val1.overflowing_add(val2);
        let (result2, c2) = result1.overflowing_add(val3);
        let v1 = is_overflow_pl(val1, val2);
        let v2 = is_overflow_pl(result1, val3);

        self.r.set(rd, result2);

        let mut cpsr = self.r.cpsr;
        cpsr.set_nz_by(result2);
        cpsr.set_c(c1 | c2);
        cpsr.set_v(v1 | v2);

        Ok(cpsr)
    }

    fn _sbc(&mut self, left: u32, right: u32, rd: Registers) -> Result<PSR> {
        let val1 = left;
        let val2 = right;
        let val3 = !self.r.cpsr.c() as u32;

        let (result1, c1) = val1.overflowing_sub(val2);
        let (result2, c2) = result1.overflowing_sub(val3);
        let v1 = is_overflow_ng(val1, val2);
        let v2 = is_overflow_ng(result1, val3);

        self.r.set(rd, result2);

        let mut cpsr = self.r.cpsr;
        cpsr.set_nz_by(result2);
        cpsr.set_c(c1 | c2);
        cpsr.set_v(v1 | v2);

        Ok(cpsr)
    }

    fn sbc(&mut self, rn: Registers, rd: Registers, (op2, _): (u32, bool)) -> Result<PSR> {
        self._sbc(self.r.get(rn), op2, rd)
    }

    fn rsc(&mut self, rn: Registers, rd: Registers, (op2, _): (u32, bool)) -> Result<PSR> {
        self._sbc(op2, self.r.get(rn), rd)
    }

    fn tst(&mut self, rn: Registers, (op2, c): (u32, bool)) -> Result<PSR> {
        self._alu_test(self.r.get(rn), op2, c, |left, right| left & right)
    }

    fn teq(&mut self, rn: Registers, (op2, c): (u32, bool)) -> Result<PSR> {
        self._alu_test(self.r.get(rn), op2, c, |left, right| left ^ right)
    }

    fn cmp(&mut self, rn: Registers, (op2, _): (u32, bool)) -> Result<PSR> {
        let left = self.r.get(rn);
        let right = op2;

        let mut cpsr = self.r.cpsr;
        cpsr.set_ng_nzc_by(left, right);

        Ok(cpsr)
    }

    fn cmn(&mut self, rn: Registers, (op2, _): (u32, bool)) -> Result<PSR> {
        let left = self.r.get(rn);
        let right = op2;

        let mut cpsr = self.r.cpsr;
        cpsr.set_pl_nzc_by(left, right);

        Ok(cpsr)
    }

    fn orr(&mut self, rn: Registers, rd: Registers, (op2, c): (u32, bool)) -> Result<PSR> {
        self._alu_logical(self.r.get(rn), op2, rd, c, |left, right| left | right)
    }

    fn mov(&mut self, rd: Registers, (op2, c): (u32, bool)) -> Result<PSR> {
        self._alu_logical(op2, 0, rd, c, |left, _| left)
    }

    fn bic(&mut self, rn: Registers, rd: Registers, (op2, c): (u32, bool)) -> Result<PSR> {
        self._alu_logical(self.r.get(rn), op2, rd, c, |left, right| left & !right)
    }

    fn mvn(&mut self, rd: Registers, (op2, c): (u32, bool)) -> Result<PSR> {
        self._alu_logical(op2, 0, rd, c, |left, _| !left)
    }

    fn multiply(
        &mut self,
        opcode: u32,
        state: bool,
        rd: Registers,
        rn: Registers,
        rs: Registers,
        rm: Registers,
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
            self.r.cpsr = cpsr;
        }

        Ok(())
    }

    fn mul(&mut self, rd: Registers, rm: Registers, rs: Registers) -> Result<PSR> {
        let left = self.r.get(rm);
        let right = self.r.get(rs);

        let result = left.wrapping_mul(right);

        self.r.set(rd, result);

        let mut cpsr = self.r.cpsr;
        cpsr.set_nz_by(result);

        Ok(cpsr)
    }

    fn mla(&mut self, rd: Registers, rn: Registers, rm: Registers, rs: Registers) -> Result<PSR> {
        let left = self.r.get(rm);
        let right = self.r.get(rs);
        let add = self.r.get(rn);

        let result = add.wrapping_add(left.wrapping_mul(right));

        self.r.set(rd, result);

        let mut cpsr = self.r.cpsr;
        cpsr.set_nz_by(result);

        Ok(cpsr)
    }

    fn umull(&mut self, rd: Registers, rn: Registers, rm: Registers, rs: Registers) -> Result<PSR> {
        let left = self.r.get(rm) as u64;
        let right = self.r.get(rs) as u64;

        let result = left.wrapping_mul(right);

        self.r.set(rd, (result >> 32) as u32);
        self.r.set(rn, result as u32);

        let mut cpsr = self.r.cpsr;
        cpsr.set_nz_by(result);

        Ok(cpsr)
    }

    fn umlal(&mut self, rd: Registers, rn: Registers, rm: Registers, rs: Registers) -> Result<PSR> {
        let left = self.r.get(rm) as u64;
        let right = self.r.get(rs) as u64;
        let add = ((self.r.get(rd) as u64) << 32) | (self.r.get(rn) as u64);

        let result = add.wrapping_add(left.wrapping_mul(right));

        self.r.set(rd, (result >> 32) as u32);
        self.r.set(rn, result as u32);

        let mut cpsr = self.r.cpsr;
        cpsr.set_nz_by(result);

        Ok(cpsr)
    }

    fn smull(&mut self, rd: Registers, rn: Registers, rm: Registers, rs: Registers) -> Result<PSR> {
        let left = self.r.get(rm) as i64;
        let right = self.r.get(rs) as i64;

        let result = left.wrapping_mul(right);

        self.r.set(rd, (result >> 32) as u32);
        self.r.set(rn, result as u32);

        let mut cpsr = self.r.cpsr;
        cpsr.set_nz_by(result);

        Ok(cpsr)
    }

    fn smlal(&mut self, rd: Registers, rn: Registers, rm: Registers, rs: Registers) -> Result<PSR> {
        let left = self.r.get(rm) as i64;
        let right = self.r.get(rs) as i64;
        let add = (((self.r.get(rd) as u64) << 32) | (self.r.get(rn) as u64)) as i64;

        let result = add.wrapping_add(left.wrapping_mul(right));

        self.r.set(rd, (result >> 32) as u32);
        self.r.set(rn, result as u32);

        let mut cpsr = self.r.cpsr;
        cpsr.set_nz_by(result);

        Ok(cpsr)
    }

    fn mrs(&mut self, p: u32, r: Registers) -> Result<()> {
        let psr = if p == 0 {
            self.r.cpsr
        } else {
            self.r.get_spsr()
        };

        self.r.set(r, psr.0);

        Ok(())
    }

    fn msr(&mut self, p: u32, f: bool, _s: bool, _x: bool, c: bool, val: PSR) -> Result<()> {
        let mut psr = if p == 0 {
            self.r.cpsr
        } else {
            self.r.get_spsr()
        };

        if f {
            psr.set_n(val.n());
            psr.set_z(val.z());
            psr.set_c(val.c());
            psr.set_v(val.v());
        }

        if c && psr.mode() == Mode::Supervisor {
            psr.set_i(val.i());
            psr.set_f(val.f());
            psr.set_t(val.t());
        }

        if p == 0 {
            self.r.cpsr = psr;
        } else {
            self.r.set_spsr(psr);
        };

        Ok(())
    }

    fn ldr(&mut self, addr: u32, byte_word: u32, rd: Registers) -> Result<()> {
        let result = if byte_word == 1 {
            self.bus.read_8(addr)? as u32
        } else {
            self.bus.read_32(addr)? as u32
        };

        self.r.set(rd, result);

        Ok(())
    }

    fn str(&mut self, addr: u32, byte_word: u32, rd: Registers) -> Result<()> {
        if byte_word == 1 {
            let result = self.r.get(rd) as u8;
            self.bus.write_8(addr, result)
        } else {
            let result = self.r.get(rd);
            self.bus.write_32(addr, result)
        }
    }

    fn single_data_transfer_pre(
        &mut self,
        up_down: u32,
        byte_word: u32,
        writeback: bool,
        load_store: u32,
        rn: Registers,
        rd: Registers,
        (op2, _): (u32, bool),
    ) -> Result<()> {
        let base_addr = self.r.get(rn);
        let offset = op2;

        let addr = if up_down == 1 {
            base_addr.wrapping_add(offset)
        } else {
            base_addr.wrapping_sub(offset)
        };

        if load_store == 1 {
            self.ldr(addr, byte_word, rd)?;
        } else {
            self.str(addr, byte_word, rd)?;
        }

        if writeback {
            self.r.set(rn, addr);
        }

        Ok(())
    }

    fn single_data_transfer_post(
        &mut self,
        up_down: u32,
        byte_word: u32,
        _memory: u32,
        load_store: u32,
        rn: Registers,
        rd: Registers,
        (op2, _): (u32, bool),
    ) -> Result<()> {
        let addr = self.r.get(rn);
        let offset = op2;

        if load_store == 1 {
            self.ldr(addr, byte_word, rd)?;
        } else {
            self.str(addr, byte_word, rd)?;
        }

        let new_addr = if up_down == 1 {
            addr.wrapping_add(offset)
        } else {
            addr.wrapping_sub(offset)
        };

        self.r.set(rn, new_addr);

        Ok(())
    }
}

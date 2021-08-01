use crate::{arit::IsOverflowAdd, arit::IsOverflowSub, bus::Bus};
use anyhow::{bail, Result};
use bitfield::bitfield;
use bitmatch::bitmatch;
use num_derive::FromPrimitive;
use num_traits::{
    ops::overflowing::{OverflowingAdd, OverflowingSub},
    FromPrimitive,
};

#[derive(Debug, FromPrimitive, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
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

impl From<u16> for Registers {
    fn from(n: u16) -> Self {
        FromPrimitive::from_u16(n).unwrap_or(Registers::R0)
    }
}

impl From<u8> for Registers {
    fn from(n: u8) -> Self {
        FromPrimitive::from_u8(n).unwrap_or(Registers::R0)
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

    pub fn set_pl_nzcv_by<T>(&mut self, left: T, right: T)
    where
        T: OverflowingAdd + IsOverflowAdd + Copy + Ord + Default,
    {
        self.set_pl_nzc_by(left, right);
        self.set_pl_v_by(left, right);
    }

    pub fn set_pl_nzc_by<T>(&mut self, left: T, right: T)
    where
        T: OverflowingAdd + Ord + Default,
    {
        let (result, c) = left.overflowing_add(&right);
        self.set_nz_by(result);
        self.set_c(c);
    }

    pub fn set_ng_nzcv_by<T>(&mut self, left: T, right: T)
    where
        T: OverflowingSub + IsOverflowSub + Copy + Ord + Default,
    {
        self.set_ng_nzc_by(left, right);
        self.set_ng_v_by(left, right);
    }

    pub fn set_ng_nzc_by<T>(&mut self, left: T, right: T)
    where
        T: OverflowingSub + Ord + Default,
    {
        let (result, c) = left.overflowing_sub(&right);
        self.set_nz_by(result);
        self.set_c(c);
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

            // halfword single data transfer - pre-indexing
            "cccc0001_uiwlrrrr_ggggjjjj_1011ssss" if self.guard(c) => self
                .halfword_single_data_transfer_pre(
                    u,
                    w == 1,
                    l,
                    Registers::from(r),
                    Registers::from(g),
                    self.halfword_transfer_offset(i == 1, j, s),
                ),

            // halfword single data transfer - post-indexing
            "cccc0000_ui0lrrrr_ggggjjjj_1011ssss" if self.guard(c) => self
                .halfword_single_data_transfer_post(
                    u,
                    l,
                    Registers::from(r),
                    Registers::from(g),
                    self.halfword_transfer_offset(i == 1, j, s),
                ),

            // block data transfer - pre-indexing
            "cccc1001_uswlrrrr_ggggffff_hhhhkkkk" if self.guard(c) => self.block_data_transfer_pre(
                u,
                s == 1,
                w == 1,
                l,
                Registers::from(r),
                vec![
                    Registers::from(g),
                    Registers::from(f),
                    Registers::from(h),
                    Registers::from(k),
                ],
            ),

            // block data transfer - post-indexing
            "cccc1000_uswlrrrr_ggggffff_hhhhkkkk" if self.guard(c) => self
                .block_data_transfer_post(
                    u,
                    s == 1,
                    w == 1,
                    l,
                    Registers::from(r),
                    vec![
                        Registers::from(g),
                        Registers::from(f),
                        Registers::from(h),
                        Registers::from(k),
                    ],
                ),

            // SWP
            "cccc0001_0b00rrrr_gggg0000_1001ffff" if self.guard(c) => self.swp(
                b,
                Registers::from(r),
                Registers::from(g),
                Registers::from(f),
            ),

            _ => Ok(()),
        }
    }

    #[bitmatch]
    fn do_mnemonic_thumb(&mut self, opecode: u16) -> Result<()> {
        #[bitmatch]
        match &opecode {
            // LSL
            "00000sss_ssrrrggg" => self.thumb_lsl(Registers::from(r), Registers::from(g), s),

            // LSR
            "00001sss_ssrrrggg" => self.thumb_lsr(Registers::from(r), Registers::from(g), s),

            // ASR
            "00010sss_ssrrrggg" => self.thumb_asr(Registers::from(r), Registers::from(g), s),

            // ADD r
            "0001100r_rrgggfff" => {
                let val = self.r.get(Registers::from(r)) as u16;

                self.thumb_add(val, Registers::from(g), Registers::from(f))
            }

            // ADD imm
            "0001110i_iigggfff" => self.thumb_add(i, Registers::from(g), Registers::from(f)),

            // SUB r
            "0001101r_rrgggfff" => {
                let val = self.r.get(Registers::from(r)) as u16;

                self.thumb_sub(val, Registers::from(g), Registers::from(f))
            }

            // SUB imm
            "0001111i_iigggfff" => self.thumb_sub(i, Registers::from(g), Registers::from(f)),

            // MOV
            "00100rrr_nnnnnnnn" => self.thumb_mov(Registers::from(r), n),

            // CMP
            "00101rrr_nnnnnnnn" => self.thumb_cmp(Registers::from(r), n),

            // ADD MOV
            "00110rrr_nnnnnnnn" => self.thumb_add_mov(Registers::from(r), n),

            // SUB MOV
            "00111rrr_nnnnnnnn" => self.thumb_sub_mov(Registers::from(r), n),

            // AND MOV
            "01000000_00rrrggg" => self.thumb_and_mov(Registers::from(r), Registers::from(g)),

            // EOR MOV
            "01000000_01rrrggg" => self.thumb_eor_mov(Registers::from(r), Registers::from(g)),

            // LSL MOV
            "01000000_10rrrggg" => self.thumb_lsl_mov(Registers::from(r), Registers::from(g)),

            // LSR MOV
            "01000000_11rrrggg" => self.thumb_lsr_mov(Registers::from(r), Registers::from(g)),

            // ASR MOV
            "01000001_00rrrggg" => self.thumb_asr_mov(Registers::from(r), Registers::from(g)),

            // ADC MOV
            "01000001_01rrrggg" => self.thumb_adc_mov(Registers::from(r), Registers::from(g)),

            // SBC MOV
            "01000001_10rrrggg" => self.thumb_sbc_mov(Registers::from(r), Registers::from(g)),

            // ROR MOV
            "01000001_11rrrggg" => self.thumb_ror_mov(Registers::from(r), Registers::from(g)),

            // TST
            "01000010_00rrrggg" => self.thumb_tst(Registers::from(r), Registers::from(g)),

            // NEG
            "01000010_01rrrggg" => self.thumb_neg(Registers::from(r), Registers::from(g)),

            // CMP r
            "01000010_10rrrggg" => {
                let n = self.r.get(Registers::from(g)) as u16;

                self.thumb_cmp(Registers::from(r), n)
            }

            // CMN
            "01000010_11rrrggg" => self.thumb_cmn(Registers::from(r), Registers::from(g)),

            // ORR
            "01000011_00rrrggg" => self.thumb_orr(Registers::from(r), Registers::from(g)),

            // MUL
            "01000011_01rrrggg" => self.thumb_mul(Registers::from(r), Registers::from(g)),

            // BIC
            "01000011_10rrrggg" => self.thumb_bic(Registers::from(r), Registers::from(g)),

            // MVN
            "01000011_11rrrggg" => self.thumb_mvn(Registers::from(r), Registers::from(g)),

            // hiR
            "010001oo_dsrrrggg" => {
                let rs = if s == 1 {
                    Registers::from(r + 8)
                } else {
                    Registers::from(r)
                };
                let rd = if d == 1 {
                    Registers::from(g + 8)
                } else {
                    Registers::from(g)
                };

                match o {
                    // ADD
                    0b00 => {
                        let val = self.r.get(rd) as u16;

                        self.thumb_add(val, rs, rd)
                    }
                    // CMP
                    0b01 => {
                        let n = self.r.get(rs) as u16;

                        self.thumb_cmp(rd, n)
                    }
                    // MOV
                    0b10 => {
                        let n = self.r.get(rs) as u16;

                        self.thumb_mov(rd, n)
                    }
                    // BX
                    0b11 => self.bx(rs),
                    _ => bail!("invalid hi register opcode"),
                }
            }

            // LDR relative
            "01001rrr_nnnnnnnn" => {
                let addr = self.r.pc.wrapping_add(n as u32);
                let rd = Registers::from(r);

                self.ldr(addr, 4, rd)
            }

            // Load/Store sign-extended byte halfword
            "01010oo_rrrbbbddd" => {
                let ro = Registers::from(r);
                let rb = Registers::from(b);
                let rd = Registers::from(d);

                let base_addr = self.r.get(rb);
                let offset = self.r.get(ro);
                let addr = base_addr.wrapping_add(offset);

                match o {
                    // STRH
                    0b00 => {
                        let result = self.r.get(rd) as u16;

                        self.bus.write_16(addr, result)
                    }
                    // LDSB
                    0b01 => {
                        let result = self.bus.read_8(addr)? as i8 as i16;

                        self.r.set(rd, result as u32);

                        Ok(())
                    }
                    // LDRH
                    0b10 => {
                        let result = self.bus.read_16(addr)? as u16;

                        self.r.set(rd, result as u32);

                        Ok(())
                    }
                    // LDSH
                    0b11 => {
                        let result = self.bus.read_16(addr)? as i16 as i32;

                        self.r.set(rd, result as u32);

                        Ok(())
                    }
                    _ => bail!("invalid load store sign-extended opecode"),
                }
            }

            // load store with register offset
            "0101oo0_rrrbbbddd" => {
                let ro = Registers::from(r);
                let rb = Registers::from(b);
                let rd = Registers::from(d);

                let base_addr = self.r.get(rb);
                let offset = self.r.get(ro);
                let addr = base_addr.wrapping_add(offset);

                match o {
                    // STR
                    0b00 => {
                        let result = self.r.get(rd);

                        self.bus.write_32(addr, result)
                    }
                    // STRB
                    0b01 => {
                        let result = self.r.get(rd) as u8;

                        self.bus.write_8(addr, result)
                    }
                    // LDR
                    0b10 => {
                        let result = self.bus.read_32(addr)?;

                        self.r.set(rd, result);

                        Ok(())
                    }
                    // LDRB
                    0b11 => {
                        let result = self.bus.read_8(addr)?;

                        self.r.set(rd, result as u32);

                        Ok(())
                    }
                    _ => bail!("invalid load/store with register offset opcode"),
                }
            }

            // load/store with immediate offset
            "011oonnn_nnbbbddd" => {
                let rb = Registers::from(b);
                let rd = Registers::from(d);

                let base_addr = self.r.get(rb);
                let addr = base_addr.wrapping_add(n as u32);

                match o {
                    // STR
                    0b00 => {
                        let result = self.r.get(rd);

                        self.bus.write_32(addr, result)
                    }
                    // LDR
                    0b01 => {
                        let result = self.bus.read_32(addr)?;

                        self.r.set(rd, result);

                        Ok(())
                    }
                    // STRB
                    0b10 => {
                        let result = self.r.get(rd) as u8;

                        self.bus.write_8(addr, result)
                    }
                    // LDRB
                    0b11 => {
                        let result = self.bus.read_8(addr)?;

                        self.r.set(rd, result as u32);

                        Ok(())
                    }
                    _ => bail!("invalid load/store with immediate offset opcode"),
                }
            }

            // load/store halfword
            "1000onnn_nnbbbddd" => {
                let rb = Registers::from(b);
                let rd = Registers::from(d);

                let base_addr = self.r.get(rb);
                let addr = base_addr.wrapping_add(n as u32);

                match o {
                    // STRH
                    0b0 => {
                        let result = self.r.get(rd) as u16;

                        self.bus.write_16(addr, result)
                    }
                    // LDRH
                    0b1 => {
                        let result = self.bus.read_16(addr)?;

                        self.r.set(rd, result as u32);

                        Ok(())
                    }
                    _ => bail!("invalid load/store halfword opcode"),
                }
            }

            // load/store SP-relative
            "1001orrr_nnnnnnnn" => {
                let rd = Registers::from(r);

                let base_addr = self.r.get(Registers::SP);
                let addr = base_addr.wrapping_add(n as u32);

                match o {
                    // STR
                    0b0 => {
                        let result = self.r.get(rd);

                        self.bus.write_32(addr, result)
                    }
                    // LDR
                    0b1 => {
                        let result = self.bus.read_32(addr)?;

                        self.r.set(rd, result);

                        Ok(())
                    }
                    _ => bail!("invalid load/store SP-relative"),
                }
            }

            // get relative address
            "1010orrr_nnnnnnnn" => {
                let rd = Registers::from(r);

                match o {
                    0b0 => {
                        let val = (self.r.pc.wrapping_add(4) & !2u32).wrapping_add(n as u32);
                        self.r.set(rd, val);

                        Ok(())
                    }
                    0b1 => {
                        let val = self.r.get(Registers::SP).wrapping_add(n as u32);
                        self.r.set(rd, val);

                        Ok(())
                    }
                    _ => bail!("invalid get relative address opcode"),
                }
            }

            // add offset to stack pointer
            "10110000_onnnnnnn" => {
                let mut result = self.r.get(Registers::SP);

                if o == 1 {
                    result = result.wrapping_sub(n as u32);
                } else {
                    result = result.wrapping_add(n as u32);
                };

                self.r.set(Registers::SP, result);

                Ok(())
            }

            _ => bail!("invalid thumb opecode"),
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
        let v1 = val1.is_overflow_add(val2);
        let v2 = result1.is_overflow_add(val3);

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
        let v1 = val1.is_overflow_sub(val2);
        let v2 = result1.is_overflow_sub(val3);

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

    fn ldr(&mut self, addr: u32, size: u32, rd: Registers) -> Result<()> {
        let result = match size {
            1 => self.bus.read_8(addr)? as u32,
            2 => self.bus.read_16(addr)? as u32,
            4 => self.bus.read_32(addr)? as u32,
            _ => bail!("invalid byte size"),
        };

        self.r.set(rd, result);

        Ok(())
    }

    fn str(&mut self, addr: u32, size: u32, rd: Registers) -> Result<()> {
        match size {
            1 => {
                let result = self.r.get(rd) as u8;
                self.bus.write_8(addr, result)
            }
            2 => {
                let result = self.r.get(rd) as u16;
                self.bus.write_16(addr, result)
            }
            4 => {
                let result = self.r.get(rd);
                self.bus.write_32(addr, result)
            }
            _ => bail!("invalid byte size"),
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
        (offset, _): (u32, bool),
    ) -> Result<()> {
        let base_addr = self.r.get(rn);

        let addr = if up_down == 1 {
            base_addr.wrapping_add(offset)
        } else {
            base_addr.wrapping_sub(offset)
        };

        let size = if byte_word == 1 { 1 } else { 4 };

        if load_store == 1 {
            self.ldr(addr, size, rd)?;
        } else {
            self.str(addr, size, rd)?;
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
        (offset, _): (u32, bool),
    ) -> Result<()> {
        let addr = self.r.get(rn);

        let size = if byte_word == 1 { 1 } else { 4 };

        if load_store == 1 {
            self.ldr(addr, size, rd)?;
        } else {
            self.str(addr, size, rd)?;
        }

        let new_addr = if up_down == 1 {
            addr.wrapping_add(offset)
        } else {
            addr.wrapping_sub(offset)
        };

        self.r.set(rn, new_addr);

        Ok(())
    }

    fn halfword_transfer_offset(&self, immediate: bool, upper: u32, lower: u32) -> u32 {
        if immediate {
            (upper as u32) << 4 | (lower as u32)
        } else {
            self.r.get(Registers::from(lower))
        }
    }

    fn halfword_single_data_transfer_pre(
        &mut self,
        up_down: u32,
        writeback: bool,
        load_store: u32,
        rn: Registers,
        rd: Registers,
        offset: u32,
    ) -> Result<()> {
        let base_addr = self.r.get(rn);

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
            self.r.set(rn, addr);
        }

        Ok(())
    }

    fn halfword_single_data_transfer_post(
        &mut self,
        up_down: u32,
        load_store: u32,
        rn: Registers,
        rd: Registers,
        offset: u32,
    ) -> Result<()> {
        let addr = self.r.get(rn);

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

        self.r.set(rn, new_addr);

        Ok(())
    }

    fn block_data_transfer_pre(
        &mut self,
        up_down: u32,
        _state: bool,
        writeback: bool,
        load_store: u32,
        rn: Registers,
        rlist: Vec<Registers>,
    ) -> Result<()> {
        let mut rlist = rlist.clone();
        rlist.sort();

        let base_addr = self.r.get(rn);
        let mut offset: i32 = 0;

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
            self.r.set(rn, ((base_addr as i32) + offset) as u32);
        }

        Ok(())
    }

    fn block_data_transfer_post(
        &mut self,
        up_down: u32,
        _state: bool,
        writeback: bool,
        load_store: u32,
        rn: Registers,
        rlist: Vec<Registers>,
    ) -> Result<()> {
        let mut rlist = rlist.clone();
        rlist.sort();

        let base_addr = self.r.get(rn);
        let mut offset: i32 = 0;

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
            self.r.set(rn, ((base_addr as i32) + offset) as u32);
        }

        Ok(())
    }

    fn swp(&mut self, byte_word: u32, rn: Registers, rd: Registers, rm: Registers) -> Result<()> {
        let addr = self.r.get(rn);
        let source = self.r.get(rm);

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

        self.r.set(rd, result);

        Ok(())
    }

    fn thumb_move_shift(
        &mut self,
        opecode: u16,
        offset: u16,
        rs: Registers,
        rd: Registers,
    ) -> Result<()> {
        match opecode {
            0b00 => self.thumb_lsl(rs, rd, offset)?,
            0b01 => self.thumb_lsr(rs, rd, offset)?,
            0b10 => self.thumb_asr(rs, rd, offset)?,
            _ => bail!("invalid thumb move shift"),
        }

        Ok(())
    }

    fn thumb_lsl(&mut self, rs: Registers, rd: Registers, offset: u16) -> Result<()> {
        let source = self.r.get(rs) as u16;

        let (result, c) = source.overflowing_shl(offset as u32);

        self.r.set(rd, result as u32);

        self.r.cpsr.set_nz_by(result);
        self.r.cpsr.set_c(c);

        Ok(())
    }

    fn thumb_lsr(&mut self, rs: Registers, rd: Registers, offset: u16) -> Result<()> {
        let source = self.r.get(rs) as u16;

        let (result, c) = source.overflowing_shr(offset as u32);

        self.r.set(rd, result as u32);

        self.r.cpsr.set_nz_by(result);
        self.r.cpsr.set_c(c);

        Ok(())
    }

    fn thumb_asr(&mut self, rs: Registers, rd: Registers, offset: u16) -> Result<()> {
        let source = self.r.get(rs) as i16;

        let (result, c) = source.overflowing_shr(offset as u32);

        self.r.set(rd, result as u32);

        self.r.cpsr.set_nz_by(result);
        self.r.cpsr.set_c(c);

        Ok(())
    }

    fn thumb_add(&mut self, val: u16, rs: Registers, rd: Registers) -> Result<()> {
        let left = self.r.get(rs) as u16;
        let right = val;
        let result = left.wrapping_add(right);

        self.r.set(rd, result as u32);
        self.r.cpsr.set_pl_nzcv_by(left, right);

        Ok(())
    }

    fn thumb_sub(&mut self, val: u16, rs: Registers, rd: Registers) -> Result<()> {
        let left = self.r.get(rs) as u16;
        let right = val;
        let result = left.wrapping_sub(right);

        self.r.set(rd, result as u32);
        self.r.cpsr.set_ng_nzcv_by(left, right);

        Ok(())
    }

    fn thumb_mov(&mut self, rd: Registers, n: u16) -> Result<()> {
        let result = n as u16;

        self.r.set(rd, result as u32);
        self.r.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_cmp(&mut self, rd: Registers, n: u16) -> Result<()> {
        let left = self.r.get(rd) as u16;
        let right = n as u16;

        self.r.cpsr.set_ng_nzcv_by(left, right);

        Ok(())
    }

    fn thumb_add_mov(&mut self, rd: Registers, n: u16) -> Result<()> {
        let left = self.r.get(rd);
        let right = n as u32;
        let result = left.wrapping_add(right);

        self.r.set(rd, result);
        self.r.cpsr.set_pl_nzcv_by(left, right);

        Ok(())
    }

    fn thumb_sub_mov(&mut self, rd: Registers, n: u16) -> Result<()> {
        let left = self.r.get(rd) as u16;
        let right = n as u16;
        let result = left.wrapping_sub(right);

        self.r.set(rd, result as u32);
        self.r.cpsr.set_ng_nzcv_by(left, right);

        Ok(())
    }

    fn thumb_and_mov(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let left = self.r.get(rd) as u16;
        let right = self.r.get(rs) as u16;
        let result = left & right;

        self.r.set(rd, result as u32);
        self.r.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_eor_mov(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let left = self.r.get(rd) as u16;
        let right = self.r.get(rs) as u16;
        let result = left ^ right;

        self.r.set(rd, result as u32);
        self.r.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_lsl_mov(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let left = self.r.get(rd) as u16;
        let right = self.r.get(rs) as u16;
        let result = left << right;

        self.r.set(rd, result as u32);
        self.r.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_lsr_mov(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let left = self.r.get(rd) as u16;
        let right = self.r.get(rs) as u16;
        let result = left >> right;

        self.r.set(rd, result as u32);
        self.r.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_asr_mov(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let left = self.r.get(rd) as i16;
        let right = self.r.get(rs) as u16;
        let result = left >> right;

        self.r.set(rd, result as u32);
        self.r.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_adc_mov(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let left = self.r.get(rd) as u16;
        let right = self.r.get(rs) as u16;
        let c = self.r.cpsr.c() as u16;

        let (result1, c1) = left.overflowing_add(right);
        let (result2, c2) = result1.overflowing_add(c);
        let v1 = left.is_overflow_add(right);
        let v2 = result1.is_overflow_add(c);

        self.r.cpsr.set_nz_by(result2);
        self.r.cpsr.set_c(c1 | c2);
        self.r.cpsr.set_v(v1 | v2);

        Ok(())
    }

    fn thumb_sbc_mov(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let left = self.r.get(rd) as u16;
        let right = self.r.get(rs) as u16;
        let c = self.r.cpsr.c() as u16;

        let (result1, c1) = left.overflowing_sub(right);
        let (result2, c2) = result1.overflowing_sub(c);
        let v1 = left.is_overflow_sub(right);
        let v2 = result1.is_overflow_sub(c);

        self.r.cpsr.set_nz_by(result2);
        self.r.cpsr.set_c(c1 | c2);
        self.r.cpsr.set_v(v1 | v2);

        Ok(())
    }

    fn thumb_ror_mov(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let left = self.r.get(rd) as u16;
        let right = self.r.get(rs) as u16;
        let result = left.rotate_right(right as u32);

        self.r.set(rd, result as u32);
        self.r.cpsr.set_nz_by(result);
        self.r.cpsr.set_c(left & 0b00000001 == 1);

        Ok(())
    }

    fn thumb_tst(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let left = self.r.get(rd) as u16;
        let right = self.r.get(rs) as u16;
        let result = left & right;

        self.r.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_neg(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let left = 0 as u16;
        let right = self.r.get(rs) as u16;
        let result = left.wrapping_sub(right);

        self.r.set(rd, result as u32);
        self.r.cpsr.set_ng_nzcv_by(left, right);

        Ok(())
    }

    fn thumb_cmn(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let left = self.r.get(rd) as u16;
        let right = self.r.get(rs) as u16;

        self.r.cpsr.set_pl_nzcv_by(left, right);

        Ok(())
    }

    fn thumb_orr(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let left = self.r.get(rd) as u16;
        let right = self.r.get(rs) as u16;
        let result = left | right;

        self.r.set(rd, result as u32);
        self.r.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_mul(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let left = self.r.get(rd) as u16;
        let right = self.r.get(rs) as u16;
        let result = left * right;

        self.r.set(rd, result as u32);
        self.r.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_bic(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let left = self.r.get(rd) as u16;
        let right = self.r.get(rs) as u16;
        let result = left & !right;

        self.r.set(rd, result as u32);
        self.r.cpsr.set_nz_by(result);

        Ok(())
    }

    fn thumb_mvn(&mut self, rd: Registers, rs: Registers) -> Result<()> {
        let val = self.r.get(rs) as u16;
        let result = !val;

        self.r.set(rd, result as u32);
        self.r.cpsr.set_nz_by(result);

        Ok(())
    }
}

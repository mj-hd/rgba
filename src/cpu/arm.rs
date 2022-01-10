use anyhow::{bail, Result};
use bitmatch::bitmatch;

use crate::{
    arit::{IntoI24, IsOverflowAdd, IsOverflowSub, MulOperand, Shift, ShiftResult},
    bus::AccessType,
};

use super::{cpu_gba::Cpu, psr::Psr, register::RegisterType, Mode};

#[derive(Default, Clone, Copy)]
pub struct Op2 {
    pub val: u32,
    pub c: Option<bool>,
    pub by_register: bool,
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

    pub fn by_register(self, f: bool) -> Self {
        Self {
            by_register: f,
            ..self
        }
    }

    pub fn c_not_affected(self, f: bool) -> Self {
        Self {
            c: if f { None } else { self.c },
            ..self
        }
    }
}

impl Cpu {
    #[bitmatch]
    pub(super) fn do_mnemonic_arm(&mut self, opecode: u32) -> Result<()> {
        #[bitmatch]
        match &opecode {
            // B
            "cccc1010_nnnnnnnn_nnnnnnnn_nnnnnnnn" => {
                if !self.guard(c) {
                    return Ok(());
                }

                self.b(n.into_i24())
            }

            // BL
            "cccc1011_nnnnnnnn_nnnnnnnn_nnnnnnnn" => {
                if !self.guard(c) {
                    return Ok(());
                }

                self.bl(n.into_i24())
            }

            // BX
            "cccc0001_00101111_11111111_0001rrrr" => {
                if !self.guard(c) {
                    return Ok(());
                }

                self.bx(RegisterType::from(r))
            }

            // SWI
            "cccc1111_????????_????????_????????" => {
                if !self.guard(c) {
                    return Ok(());
                }

                self.swi()
            }

            // BKPT
            "cccc0001_0010????_????????_0111????" => {
                if !self.guard(c) {
                    return Ok(());
                }

                self.bkpt()
            }

            // UND
            "cccc011?_????????_????????_???1????" => {
                if !self.guard(c) {
                    return Ok(());
                }

                self.und()
            }

            // MRS
            "cccc0001_0p001111_rrrr0000_00000000" => {
                if !self.guard(c) {
                    return Ok(());
                }

                self.mrs(p, RegisterType::from(r))
            }

            // MSR Register
            "cccc0001_0p10fsxt_11110000_0000rrrr" => {
                if !self.guard(c) {
                    return Ok(());
                }

                let val = self.get_r(RegisterType::from(r));

                self.msr(p, f == 1, s == 1, x == 1, t == 1, Psr(val))
            }

            // MSR Immediate
            "cccc0011_0p10fsxt_1111iiii_jjjjjjjj" => {
                if !self.guard(c) {
                    return Ok(());
                }

                let val = j.rotate_right(i * 2);

                self.msr(p, f == 1, s == 1, x == 1, t == 1, Psr(val))
            }

            // Single Data Transfer - post-indexing
            "cccc01i0_ubtlrrrr_ggggpppp_pppppppp" => {
                if !self.guard(c) {
                    return Ok(());
                }

                self.single_data_transfer_post(
                    u,
                    b,
                    t,
                    l,
                    RegisterType::from(r),
                    RegisterType::from(g),
                    self.op2_simple(i, p),
                )
            }

            // Single Data Transfer - pre-indexing
            "cccc01i1_ubwlrrrr_ggggpppp_pppppppp" => {
                if !self.guard(c) {
                    return Ok(());
                }

                self.single_data_transfer_pre(
                    u,
                    b,
                    w == 1,
                    l,
                    RegisterType::from(r),
                    RegisterType::from(g),
                    self.op2_simple(i, p),
                )
            }

            // halfword single data transfer - pre-indexing
            "cccc0001_uiwlrrrr_ggggjjjj_1ee1ssss" if e != 0 => {
                if !self.guard(c) {
                    return Ok(());
                }

                self.halfword_single_data_transfer_pre(
                    u,
                    w == 1,
                    l,
                    e,
                    RegisterType::from(r),
                    RegisterType::from(g),
                    self.halfword_transfer_offset(i == 1, j, s),
                )
            }

            // halfword single data transfer - post-indexing
            "cccc0000_ui0lrrrr_ggggjjjj_1ee1ssss" if e != 0 => {
                if !self.guard(c) {
                    return Ok(());
                }

                self.halfword_single_data_transfer_post(
                    u,
                    l,
                    e,
                    RegisterType::from(r),
                    RegisterType::from(g),
                    self.halfword_transfer_offset(i == 1, j, s),
                )
            }

            // block data transfer - pre-indexing
            "cccc1001_uswlrrrr_gggggggg_gggggggg" => {
                if !self.guard(c) {
                    return Ok(());
                }

                self.block_data_transfer_pre(
                    u,
                    s == 1,
                    w == 1,
                    l,
                    RegisterType::from(r),
                    self.rlist(g as u16),
                )
            }

            // block data transfer - post-indexing
            "cccc1000_uswlrrrr_gggggggg_gggggggg" => {
                if !self.guard(c) {
                    return Ok(());
                }

                self.block_data_transfer_post(
                    u,
                    s == 1,
                    w == 1,
                    l,
                    RegisterType::from(r),
                    self.rlist(g as u16),
                )
            }

            // SWP
            "cccc0001_0b00rrrr_gggg0000_1001ffff" => {
                if !self.guard(c) {
                    return Ok(());
                }

                self.swp(
                    b,
                    RegisterType::from(r),
                    RegisterType::from(g),
                    RegisterType::from(f),
                )
            }

            // Multiply
            "cccc000o_ooosrrrr_ggggffff_1001hhhh" => {
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

            _ => Ok(()),
        }
    }

    fn b(&mut self, nn: i32) -> Result<()> {
        self.pc = self.pc.wrapping_add((nn * 4) as u32);
        self.pc_invalidated = true; // 1N + 2S

        Ok(())
    }

    fn bl(&mut self, nn: i32) -> Result<()> {
        self.set_r(RegisterType::LR, self.pc.wrapping_sub(4))?;

        self.pc = self.pc.wrapping_add((nn * 4) as u32);
        self.pc_invalidated = true; // 1N + 2S

        Ok(())
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

        if op2.by_register {
            self.bus.idle_stall(1); // 1I
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

        self.bus.idle_stall(right.leading_blocks()); // mI

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

        self.bus.idle_stall(right.leading_blocks() + 1); // (m+1)I

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

        self.bus.idle_stall(right.leading_blocks() + 1); // (m+1)I

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

        self.bus.idle_stall(right.leading_blocks() + 2); // (m+2)I

        Ok(cpsr)
    }

    fn smull(
        &mut self,
        rd: RegisterType,
        rn: RegisterType,
        rm: RegisterType,
        rs: RegisterType,
    ) -> Result<Psr> {
        let left = self.get_r(rm) as i32 as i64;
        let right = self.get_r(rs) as i32 as i64;

        let result = left.wrapping_mul(right);

        self.set_r(rd, (result >> 32) as u32)?;
        self.set_r(rn, result as u32)?;

        let mut cpsr = self.cpsr;
        cpsr.set_nz_by(result as u64);

        self.bus.idle_stall(right.leading_blocks() + 1); // (m+1)I

        Ok(cpsr)
    }

    fn smlal(
        &mut self,
        rd: RegisterType,
        rn: RegisterType,
        rm: RegisterType,
        rs: RegisterType,
    ) -> Result<Psr> {
        let left = self.get_r(rm) as i32 as i64;
        let right = self.get_r(rs) as i32 as i64;
        let add = (((self.get_r(rd) as u64) << 32) | (self.get_r(rn) as u64)) as i64;

        let result = add.wrapping_add(left.wrapping_mul(right));

        self.set_r(rd, (result >> 32) as u32)?;
        self.set_r(rn, result as u32)?;

        let mut cpsr = self.cpsr;
        cpsr.set_nz_by(result);

        self.bus.idle_stall(right.leading_blocks() + 2); // (m+2)I

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
            if writeback {
                self.set_r(rn, addr)?;
            }

            self.ldr(addr, size, rd, AccessType::NonSeq)?; // 1N
            self.bus.idle_stall(1); // 1I
        } else {
            self.str(addr, size, rd, AccessType::NonSeq)?; // 1N

            if writeback {
                self.set_r(rn, addr)?;
            }
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

        let new_addr = if up_down == 1 {
            addr.wrapping_add(op2.val)
        } else {
            addr.wrapping_sub(op2.val)
        };

        let size = if byte_word == 1 { 1 } else { 4 };

        if load_store == 1 {
            self.set_r(rn, new_addr)?;
            self.ldr(addr, size, rd, AccessType::NonSeq)?; // 1N
            self.bus.idle_stall(1); // 1I
        } else {
            self.str(addr, size, rd, AccessType::NonSeq)?; // 1N
            self.set_r(rn, new_addr)?;
        }

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
        extended: u32,
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
            if writeback {
                self.set_r(rn, addr)?;
            }

            let val = match extended {
                // zero-extended halfword
                0b01 => self.bus.read_16(addr, AccessType::NonSeq)? as u32,
                // sign extended byte
                0b10 => self.bus.read_8(addr, AccessType::NonSeq)? as i8 as i32 as u32,
                // sign extended halfword
                0b11 => self.bus.read_16(addr, AccessType::NonSeq)? as i16 as i32 as u32,
                _ => bail!("unexpected halfword extended"),
            }; // 1N

            self.bus.idle_stall(1); // 1I

            self.set_r(rd, val)?;
        } else {
            self.str(addr, 2, rd, AccessType::NonSeq)?; // 1N

            if writeback {
                self.set_r(rn, addr)?;
            }
        }

        Ok(())
    }

    fn halfword_single_data_transfer_post(
        &mut self,
        up_down: u32,
        load_store: u32,
        extended: u32,
        rn: RegisterType,
        rd: RegisterType,
        offset: u32,
    ) -> Result<()> {
        let addr = self.get_r(rn);

        let new_addr = if up_down == 1 {
            addr.wrapping_add(offset)
        } else {
            addr.wrapping_sub(offset)
        };

        if load_store == 1 {
            self.set_r(rn, new_addr)?;

            let val = match extended {
                // zero-extended halfword
                0b01 => self.bus.read_16(addr, AccessType::NonSeq)? as u32,
                // sign extended byte
                0b10 => self.bus.read_8(addr, AccessType::NonSeq)? as i8 as i32 as u32,
                // sign extended halfword
                0b11 => self.bus.read_16(addr, AccessType::NonSeq)? as i16 as i32 as u32,
                _ => bail!("unexpected halfword extended"),
            }; // 1N

            self.bus.idle_stall(1); // 1I

            self.set_r(rd, val)?;
        } else {
            self.str(addr, 2, rd, AccessType::NonSeq)?; // 1N
            self.set_r(rn, new_addr)?;
        }

        Ok(())
    }

    fn block_data_transfer_pre(
        &mut self,
        up_down: u32,
        force_user: bool,
        writeback: bool,
        load_store: u32,
        rn: RegisterType,
        rlist: Vec<RegisterType>,
    ) -> Result<()> {
        let base_addr = if !force_user {
            self.get_r(rn)
        } else {
            self.get_r_by_mode(rn, Mode::User)
        };
        let mut offset: i32 = 0;

        let mut rlist = rlist.clone();

        if up_down == 0 {
            rlist.reverse();
        };

        let mut access_type = AccessType::NonSeq;

        for r in rlist.into_iter() {
            offset = if up_down == 1 {
                offset.wrapping_add(4)
            } else {
                offset.wrapping_sub(4)
            };

            let addr = ((base_addr as i32) + offset) as u32;

            if load_store == 1 {
                // 1N + (n-1)S
                if !force_user {
                    self.ldr(addr, 4, r, access_type)?;
                } else {
                    self.ldr_by_mode(addr, 4, r, Mode::User, access_type)?;
                }
            } else {
                // 1N + (n-1)S
                if !force_user {
                    self.str(addr, 4, r, access_type)?;
                } else {
                    self.str_by_mode(addr, 4, r, Mode::User, access_type)?;
                }
            };

            access_type = AccessType::Seq;
        }

        // TODO: 他のwritebackタイミングも調査
        if writeback {
            let addr = ((base_addr as i32) + offset) as u32;
            self.set_r(rn, addr)?;
        }

        if load_store == 1 {
            self.bus.idle_stall(1); // 1I
        }

        Ok(())
    }

    fn block_data_transfer_post(
        &mut self,
        up_down: u32,
        force_user: bool,
        writeback: bool,
        load_store: u32,
        rn: RegisterType,
        rlist: Vec<RegisterType>,
    ) -> Result<()> {
        let base_addr = if !force_user {
            self.get_r(rn)
        } else {
            self.get_r_by_mode(rn, Mode::User)
        };
        let mut offset: i32 = 0;

        let mut rlist = rlist.clone();

        if up_down == 0 {
            rlist.reverse();
        };

        let mut access_type = AccessType::NonSeq;

        for r in rlist.into_iter() {
            let addr = ((base_addr as i32) + offset) as u32;

            if load_store == 1 {
                // 1N + (n-1)S
                if !force_user {
                    self.ldr(addr, 4, r, access_type)?;
                } else {
                    self.ldr_by_mode(addr, 4, r, Mode::User, access_type)?;
                }
            } else {
                // 1N + (n-1)S
                if !force_user {
                    self.str(addr, 4, r, access_type)?;
                } else {
                    self.str_by_mode(addr, 4, r, Mode::User, access_type)?;
                }
            };

            access_type = AccessType::Seq;

            offset = if up_down == 1 {
                offset.wrapping_add(4)
            } else {
                offset.wrapping_sub(4)
            };

            if writeback {
                self.set_r(rn, ((base_addr as i32) + offset) as u32)?;
            }
        }

        if load_store == 1 {
            self.bus.idle_stall(1); // 1I
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

        // 1N
        let result = if byte_word == 1 {
            self.bus.read_8(addr, AccessType::NonSeq)? as u32
        } else {
            self.bus.read_32(addr, AccessType::NonSeq)?
        };

        // 1N
        if byte_word == 1 {
            self.bus.write_8(addr, source as u8, AccessType::NonSeq)?;
        } else {
            self.bus.write_32(addr, source, AccessType::NonSeq)?;
        }

        self.bus.idle_stall(1); // 1I

        self.set_r(rd, result)?;

        Ok(())
    }
}

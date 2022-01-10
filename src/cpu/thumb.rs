use anyhow::{bail, Result};
use bitmatch::bitmatch;
use log::error;

use crate::{
    arit::{IntoI10, IsOverflowAdd, IsOverflowSub, MulOperand, Shift, ShiftResult},
    bus::AccessType,
};

use super::{cpu_gba::Cpu, register::RegisterType};

impl Cpu {
    #[bitmatch]
    pub(super) fn do_mnemonic_thumb(&mut self, opecode: u16) -> Result<()> {
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

                self.thumb_add(val, RegisterType::from(g), RegisterType::from(f), true)
            }

            // ADD imm
            "0001110i_iigggfff" => {
                self.thumb_add(i as u32, RegisterType::from(g), RegisterType::from(f), true)
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
                let n = self.get_r(RegisterType::from(r));

                self.thumb_cmp(RegisterType::from(g), n)
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

                        self.thumb_add(val, rs, rd, false)
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

                self.bus.idle_stall(1); // 1I

                self.ldr(addr, 4, rd, AccessType::NonSeq) // 1N
            }

            // Load/Store sign-extended byte halfword
            "0101oo1r_rrbbbddd" => {
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

                        // 1N
                        self.bus.write_16(addr, result, AccessType::NonSeq)
                    }
                    // LDSB
                    0b01 => {
                        // 1N
                        let result = self.bus.read_8(addr, AccessType::NonSeq)? as i8 as i16;

                        self.set_r(rd, result as u32)?;

                        self.bus.idle_stall(1); // 1I

                        Ok(())
                    }
                    // LDRH
                    0b10 => {
                        // 1N
                        let result = self.bus.read_16(addr, AccessType::NonSeq)? as u16;

                        self.set_r(rd, result as u32)?;

                        self.bus.idle_stall(1); // 1I

                        Ok(())
                    }
                    // LDSH
                    0b11 => {
                        // 1N
                        let result = self.bus.read_16(addr, AccessType::NonSeq)? as i16 as i32;

                        self.set_r(rd, result as u32)?;

                        self.bus.idle_stall(1); // 1I

                        Ok(())
                    }
                    _ => bail!("invalid load store sign-extended opecode"),
                }
            }

            // load store with register offset
            "0101oo0r_rrbbbddd" => {
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

                        // 1N
                        self.bus.write_32(addr, result, AccessType::NonSeq)
                    }
                    // STRB
                    0b01 => {
                        let result = self.get_r(rd) as u8;

                        // 1N
                        self.bus.write_8(addr, result, AccessType::NonSeq)
                    }
                    // LDR
                    0b10 => {
                        // 1N
                        let result = self.bus.read_32(addr, AccessType::NonSeq)?;

                        self.set_r(rd, result)?;

                        self.bus.idle_stall(1); // 1I

                        Ok(())
                    }
                    // LDRB
                    0b11 => {
                        // 1N
                        let result = self.bus.read_8(addr, AccessType::NonSeq)?;

                        self.set_r(rd, result as u32)?;

                        self.bus.idle_stall(1); // 1I

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

                        // 1N
                        self.bus.write_32(addr, result, AccessType::NonSeq)
                    }
                    // LDR
                    0b01 => {
                        let addr = base_addr.wrapping_add(n as u32 * 4);

                        // 1N
                        let result = self.bus.read_32(addr, AccessType::NonSeq)?;

                        self.set_r(rd, result)?;

                        self.bus.idle_stall(1); // 1I

                        Ok(())
                    }
                    // STRB
                    0b10 => {
                        let result = self.get_r(rd) as u8;

                        let addr = base_addr.wrapping_add(n as u32);

                        // 1N
                        self.bus.write_8(addr, result, AccessType::NonSeq)
                    }
                    // LDRB
                    0b11 => {
                        let addr = base_addr.wrapping_add(n as u32);

                        // 1N
                        let result = self.bus.read_8(addr, AccessType::NonSeq)?;

                        self.set_r(rd, result as u32)?;

                        self.bus.idle_stall(1); // 1I

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
                let addr = base_addr.wrapping_add(n as u32 * 2);

                match o {
                    // STRH
                    0b0 => {
                        let result = self.get_r(rd) as u16;

                        // 1N
                        self.bus.write_16(addr, result, AccessType::NonSeq)
                    }
                    // LDRH
                    0b1 => {
                        // 1N
                        let result = self.bus.read_16(addr, AccessType::NonSeq)?;

                        self.set_r(rd, result as u32)?;

                        self.bus.idle_stall(1); // 1I

                        Ok(())
                    }
                    _ => bail!("invalid load/store halfword opcode"),
                }
            }

            // load/store SP-relative
            "1001orrr_nnnnnnnn" => {
                let rd = RegisterType::from(r);

                let base_addr = self.get_r(RegisterType::SP);
                let addr = base_addr.wrapping_add(n as u32 * 4);

                match o {
                    // STR
                    0b0 => {
                        let result = self.get_r(rd);

                        // 1N
                        self.bus.write_32(addr, result, AccessType::NonSeq)
                    }
                    // LDR
                    0b1 => {
                        // 1N
                        let result = self.bus.read_32(addr, AccessType::NonSeq)?;

                        self.set_r(rd, result)?;

                        self.bus.idle_stall(1); // 1I

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

    fn thumb_add(&mut self, val: u32, rs: RegisterType, rd: RegisterType, s: bool) -> Result<()> {
        let left = self.get_r(rs);
        let right = val;
        let result = left.wrapping_add(right);

        self.set_r(rd, result)?;

        if s {
            self.cpsr.set_pl_nzcv_by(left, right);
        }

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

        self.bus.idle_stall(1); // 1I

        Ok(())
    }

    fn thumb_lsr_mov(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);
        let ShiftResult(result, c) = left.lsr(right, false);

        self.set_r(rd, result)?;
        self.cpsr.set_nz_by(result);
        self.cpsr.set_c(c);

        self.bus.idle_stall(1); // 1I

        Ok(())
    }

    fn thumb_asr_mov(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);
        let ShiftResult(result, c) = left.asr(right, false);

        self.set_r(rd, result)?;
        self.cpsr.set_nz_by(result);
        self.cpsr.set_c(c);

        self.bus.idle_stall(1); // 1I

        Ok(())
    }

    fn thumb_ror_mov(&mut self, rs: RegisterType, rd: RegisterType) -> Result<()> {
        let left = self.get_r(rd);
        let right = self.get_r(rs);
        let ShiftResult(result, c) = left.ror(right, self.cpsr.c(), false);

        self.set_r(rd, result)?;
        self.cpsr.set_nz_by(result);
        self.cpsr.set_c(c);

        self.bus.idle_stall(1); // 1I

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
        let c = !self.cpsr.c() as u32;

        let (result1, c1) = left.overflowing_sub(right);
        let (result2, c2) = result1.overflowing_sub(c);
        let v1 = left.is_overflow_sub(right);
        let v2 = result1.is_overflow_sub(c);

        self.set_r(rd, result2)?;
        self.cpsr.set_nz_by(result2);
        self.cpsr.set_c(!c1 | !c2);
        self.cpsr.set_v(v1 | v2);

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
        let result = left.wrapping_mul(right);

        self.set_r(rd, result)?;
        self.cpsr.set_nz_by(result);

        self.bus.idle_stall(left.leading_blocks()); // mI

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

        let mut access_type = AccessType::NonSeq;

        for r in registers.into_iter().rev() {
            let val = self.get_r(r);

            addr = addr.wrapping_sub(4);

            self.bus.write_32(addr, val, access_type)?; // 1N + (n-1)S

            access_type = AccessType::Seq;
        }

        self.set_r(RegisterType::SP, addr)?;

        Ok(())
    }

    fn thumb_pop(&mut self, registers: Vec<RegisterType>) -> Result<()> {
        self.thumb_ldmia(RegisterType::SP, registers)
    }

    fn thumb_stmia(&mut self, rb: RegisterType, registers: Vec<RegisterType>) -> Result<()> {
        let mut addr = self.get_r(rb);

        let mut access_type = AccessType::NonSeq;

        for r in registers {
            let val = self.get_r(r);

            self.bus.write_32(addr, val, access_type)?; // 1N + (n-1)S

            access_type = AccessType::Seq;

            addr = addr.wrapping_add(4);
        }

        self.set_r(rb, addr)?;

        Ok(())
    }

    fn thumb_ldmia(&mut self, rb: RegisterType, registers: Vec<RegisterType>) -> Result<()> {
        let mut addr = self.get_r(rb);

        let mut access_type = AccessType::NonSeq;

        for r in registers {
            let val = self.bus.read_32(addr, access_type)?; // 1N + (n-1)S

            access_type = AccessType::Seq;

            self.set_r(r, val)?;

            addr = addr.wrapping_add(4);
        }

        self.bus.idle_stall(1); // 1I

        self.set_r(rb, addr)?;

        Ok(())
    }

    fn thumb_branch(&mut self, offset: i16) -> Result<()> {
        let base_addr = self.pc;
        let offset = (offset as i32) << 1;
        let addr = (base_addr as i32).wrapping_add(offset) as u32;

        self.pc = addr;
        self.pc_invalidated = true; // 1N + 2S

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
        self.pc_invalidated = true; // 1N + 2S
        self.set_r(RegisterType::LR, orig_pc - 1)?;

        Ok(())
    }
}

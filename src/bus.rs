use anyhow::Result;

use crate::{ppu::Ppu, rom::Rom};

pub struct Bus {
    wram_onboard: Box<[u8; 0x4_0000]>,
    wram_onchip: Box<[u8; 0x8000]>,

    pub ppu: Ppu,

    pub rom: Rom,
}

impl Bus {
    pub fn new(rom: Rom, ppu: Ppu) -> Self {
        Self {
            wram_onboard: Box::new([0; 0x4_0000]),
            wram_onchip: Box::new([0; 0x8000]),

            ppu,
            rom,
        }
    }

    pub fn tick(&mut self) -> Result<()> {
        self.ppu.tick()?;
        // TODO
        Ok(())
    }

    fn high(&self, val: u16) -> u8 {
        (val >> 8) as u8
    }

    fn low(&self, val: u16) -> u8 {
        val as u8
    }

    pub fn read_8(&self, addr: u32) -> Result<u8> {
        match addr {
            0x0200_0000..=0x0203_FFFF => Ok(self.wram_onboard[(addr - 0x0200_0000) as usize]),
            0x0300_0000..=0x0300_7FFF => Ok(self.wram_onchip[(addr - 0x0300_0000) as usize]),
            0x0400_0000..=0x04FF_FFFF => {
                if addr % 2 == 0 {
                    Ok(self.low(self.read_16(addr)?))
                } else {
                    Ok(self.high(self.read_16(addr - 1)?))
                }
            }
            0x0500_0000..=0x07FF_FFFF => self.ppu.read_vram_8(addr),
            0x0800_0000..=0x0FFF_FFFF => self.rom.read_8(addr),
            _ => Ok(0),
        }
    }

    pub fn read_16(&self, addr: u32) -> Result<u16> {
        match addr {
            0x0400_0000 => self.ppu.read_dispcnt(),
            0x0400_0002 => self.ppu.read_green_swap(),
            0x0400_0004 => self.ppu.read_dispstat(),
            0x0400_0006 => self.ppu.read_v_count(),
            0x0400_0008 => self.ppu.read_bg_0_cnt(),
            0x0400_000A => self.ppu.read_bg_1_cnt(),
            0x0400_000C => self.ppu.read_bg_2_cnt(),
            0x0400_000E => self.ppu.read_bg_3_cnt(),
            0x0400_0010 => self.ppu.read_bg_0_offset_x(),
            0x0400_0012 => self.ppu.read_bg_0_offset_y(),
            0x0400_0014 => self.ppu.read_bg_1_offset_x(),
            0x0400_0016 => self.ppu.read_bg_1_offset_y(),
            0x0400_0018 => self.ppu.read_bg_2_offset_x(),
            0x0400_001A => self.ppu.read_bg_2_offset_y(),
            0x0400_001C => self.ppu.read_bg_3_offset_x(),
            0x0400_001E => self.ppu.read_bg_3_offset_y(),
            0x0400_0020 => self.ppu.read_bg_2_p_a(),
            0x0400_0022 => self.ppu.read_bg_2_p_b(),
            0x0400_0024 => self.ppu.read_bg_2_p_c(),
            0x0400_0026 => self.ppu.read_bg_2_p_d(),
            0x0400_0028 => self.ppu.read_bg_2_x_l(),
            0x0400_002A => self.ppu.read_bg_2_x_h(),
            0x0400_002C => self.ppu.read_bg_2_y_l(),
            0x0400_002E => self.ppu.read_bg_2_y_h(),
            0x0400_0030 => self.ppu.read_bg_3_p_a(),
            0x0400_0032 => self.ppu.read_bg_3_p_b(),
            0x0400_0034 => self.ppu.read_bg_3_p_c(),
            0x0400_0036 => self.ppu.read_bg_3_p_d(),
            0x0400_0038 => self.ppu.read_bg_3_x_l(),
            0x0400_003A => self.ppu.read_bg_3_x_h(),
            0x0400_003C => self.ppu.read_bg_3_y_l(),
            0x0400_003E => self.ppu.read_bg_3_y_h(),
            0x0400_0040 => self.ppu.read_win_0_h(),
            0x0400_0042 => self.ppu.read_win_1_h(),
            0x0400_0044 => self.ppu.read_win_0_v(),
            0x0400_0046 => self.ppu.read_win_1_v(),
            0x0400_0048 => self.ppu.read_win_in(),
            0x0400_004A => self.ppu.read_win_out(),
            0x0400_004C => self.ppu.read_mosaic(),
            0x0400_0050 => self.ppu.read_bld_cnt(),
            0x0400_0052 => self.ppu.read_bld_alpha(),
            0x0400_0054 => self.ppu.read_bld_y(),
            _ => {
                let low = self.read_8(addr)?;
                let high = self.read_8(addr + 1)?;

                Ok((high as u16) << 8 | low as u16)
            }
        }
    }

    pub fn read_32(&self, addr: u32) -> Result<u32> {
        let lowest = self.read_8(addr)?;
        let lower = self.read_8(addr + 1)?;
        let higher = self.read_8(addr + 2)?;
        let highest = self.read_8(addr + 3)?;

        Ok((highest as u32) << 24 | (higher as u32) << 16 | (lower as u32) << 8 | lowest as u32)
    }

    pub fn write_8(&mut self, addr: u32, val: u8) -> Result<()> {
        match addr {
            0x0200_0000..=0x0203_FFFF => {
                self.wram_onboard[(addr - 0x0200_0000) as usize] = val;

                Ok(())
            }
            0x0300_0000..=0x0300_7FFF => {
                self.wram_onchip[(addr - 0x0300_0000) as usize] = val;

                Ok(())
            }
            0x0400_0000..=0x04FF_FFFF => {
                if addr % 2 == 0 {
                    let data = self.read_16(addr)?;
                    self.write_16(addr, data & 0xFF00 | (val as u16))
                } else {
                    let data = self.read_16(addr)?;
                    self.write_16(addr, data & 0x00FF | ((val as u16) << 8))
                }
            }
            0x0500_0000..=0x07FF_FFFF => self.ppu.write_vram_8(addr, val),
            0x0800_0000..=0x0FFF_FFFF => self.rom.write_8(addr, val),
            _ => Ok(()),
        }
    }

    pub fn write_16(&mut self, addr: u32, val: u16) -> Result<()> {
        // TODO readonly
        match addr {
            0x0400_0000 => self.ppu.write_dispcnt(val),
            0x0400_0002 => self.ppu.write_green_swap(val),
            0x0400_0004 => self.ppu.write_dispstat(val),
            0x0400_0008 => self.ppu.write_bg_0_cnt(val),
            0x0400_000A => self.ppu.write_bg_1_cnt(val),
            0x0400_000C => self.ppu.write_bg_2_cnt(val),
            0x0400_000E => self.ppu.write_bg_3_cnt(val),
            0x0400_0010 => self.ppu.write_bg_0_offset_x(val),
            0x0400_0012 => self.ppu.write_bg_0_offset_y(val),
            0x0400_0014 => self.ppu.write_bg_1_offset_x(val),
            0x0400_0016 => self.ppu.write_bg_1_offset_y(val),
            0x0400_0018 => self.ppu.write_bg_2_offset_x(val),
            0x0400_001A => self.ppu.write_bg_2_offset_y(val),
            0x0400_001C => self.ppu.write_bg_3_offset_x(val),
            0x0400_001E => self.ppu.write_bg_3_offset_y(val),
            0x0400_0020 => self.ppu.write_bg_2_p_a(val),
            0x0400_0022 => self.ppu.write_bg_2_p_b(val),
            0x0400_0024 => self.ppu.write_bg_2_p_c(val),
            0x0400_0026 => self.ppu.write_bg_2_p_d(val),
            0x0400_0028 => self.ppu.write_bg_2_x_l(val),
            0x0400_002A => self.ppu.write_bg_2_x_h(val),
            0x0400_002C => self.ppu.write_bg_2_y_l(val),
            0x0400_002E => self.ppu.write_bg_2_y_h(val),
            0x0400_0030 => self.ppu.write_bg_3_p_a(val),
            0x0400_0032 => self.ppu.write_bg_3_p_b(val),
            0x0400_0034 => self.ppu.write_bg_3_p_c(val),
            0x0400_0036 => self.ppu.write_bg_3_p_d(val),
            0x0400_0038 => self.ppu.write_bg_3_x_l(val),
            0x0400_003A => self.ppu.write_bg_3_x_h(val),
            0x0400_003C => self.ppu.write_bg_3_y_l(val),
            0x0400_003E => self.ppu.write_bg_3_y_h(val),
            0x0400_0040 => self.ppu.write_win_0_h(val),
            0x0400_0042 => self.ppu.write_win_1_h(val),
            0x0400_0044 => self.ppu.write_win_0_v(val),
            0x0400_0046 => self.ppu.write_win_1_v(val),
            0x0400_0048 => self.ppu.write_win_in(val),
            0x0400_004A => self.ppu.write_win_out(val),
            0x0400_004C => self.ppu.write_mosaic(val),
            0x0400_0050 => self.ppu.write_bld_cnt(val),
            0x0400_0052 => self.ppu.write_bld_alpha(val),
            0x0400_0054 => self.ppu.write_bld_y(val),
            _ => {
                self.write_8(addr, val as u8)?;
                self.write_8(addr + 1, (val >> 8) as u8)?;

                Ok(())
            }
        }
    }

    pub fn write_32(&mut self, addr: u32, val: u32) -> Result<()> {
        self.write_8(addr, val as u8)?;
        self.write_8(addr + 1, (val >> 8) as u8)?;
        self.write_8(addr + 2, (val >> 16) as u8)?;
        self.write_8(addr + 3, (val >> 24) as u8)?;

        Ok(())
    }
}

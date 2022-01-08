use anyhow::{bail, Result};

use crate::{
    bios::BIOS,
    dma::{Dmas, DmasResult},
    keypad::KeyPad,
    ppu::ppu::Ppu,
    rom::Rom,
    timer::Timers,
};
use bitfield::bitfield;

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct If(u16);
    pub game_pak, set_game_pak: 13;
    pub keypad, set_keypad: 12;
    pub dma_3, set_dma_3: 11;
    pub dma_2, set_dma_2: 10;
    pub dma_1, set_dma_1: 9;
    pub dma_0, set_dma_0: 8;
    pub serial, set_serial: 7;
    pub timer_3, set_timer_3: 6;
    pub timer_2, set_timer_2: 5;
    pub timer_1, set_timer_1: 4;
    pub timer_0, set_timer_0: 3;
    pub lcd_v_counter, set_lcd_v_counter: 2;
    pub lcd_h_blank, set_lcd_h_blank: 1;
    pub lcd_v_blank, set_lcd_v_blank: 0;
}

pub struct Bus {
    wram_onboard: Box<[u8; 0x4_0000]>,
    wram_onchip: Box<[u8; 0x8000]>,

    // TODO: リファクタしたい
    prev_h_blank: bool,
    prev_v_blank: bool,
    prev_v_counter: bool,
    prev_timer_0: bool,
    prev_timer_1: bool,
    prev_timer_2: bool,
    prev_timer_3: bool,
    prev_keypad: bool,

    pub ime: bool,
    pub interrupt_enable: If,
    pub interrupt_flag: If,

    pub ppu: Ppu,
    pub timers: Timers,
    pub dmas: Dmas,
    pub keypad: KeyPad,
    pub rom: Box<Rom>,
}

impl Bus {
    pub fn new(rom: Box<Rom>, ppu: Ppu) -> Self {
        Self {
            wram_onboard: Box::new([0; 0x4_0000]),
            wram_onchip: Box::new([0; 0x8000]),

            prev_h_blank: false,
            prev_v_blank: false,
            prev_v_counter: false,
            prev_timer_0: false,
            prev_timer_1: false,
            prev_timer_2: false,
            prev_timer_3: false,
            prev_keypad: false,

            ime: false,
            interrupt_enable: If(0),
            interrupt_flag: If(0),

            ppu,
            timers: Timers::new(),
            dmas: Dmas::new(),
            keypad: KeyPad::new(),
            rom,
        }
    }

    pub fn tick(&mut self) -> Result<()> {
        self.timers.tick()?;
        self.keypad.tick()?;
        self.ppu.tick()?;

        let transfer = self.dmas.tick(
            self.ppu.dispstat.v_blank(),
            self.ppu.dispstat.h_blank(),
            false,
            false,
        )?;

        match transfer {
            DmasResult::Transfer(size, src_addr, dst_addr) => match size {
                2 => self.write_16(dst_addr, self.read_16(src_addr)?)?,
                4 => self.write_32(dst_addr, self.read_32(src_addr)?)?,
                _ => bail!("unexpected transfer size"),
            },
            DmasResult::Irq(0) => {
                self.interrupt_flag.set_dma_0(true);
            }
            DmasResult::Irq(1) => {
                self.interrupt_flag.set_dma_1(true);
            }
            DmasResult::Irq(2) => {
                self.interrupt_flag.set_dma_2(true);
            }
            DmasResult::Irq(3) => {
                self.interrupt_flag.set_dma_3(true);
            }
            _ => (),
        }

        if self.ppu.dispstat.h_blank_irq_enable()
            && self.ppu.dispstat.h_blank()
            && self.prev_h_blank != self.ppu.dispstat.h_blank()
        {
            self.interrupt_flag.set_lcd_h_blank(true);
        }

        if self.ppu.dispstat.v_blank_irq_enable()
            && self.ppu.dispstat.v_blank()
            && self.prev_v_blank != self.ppu.dispstat.v_blank()
        {
            self.interrupt_flag.set_lcd_v_blank(true);
        }

        if self.ppu.dispstat.v_counter_irq_enable()
            && self.ppu.dispstat.v_counter()
            && self.prev_v_counter != self.ppu.dispstat.v_counter()
        {
            self.interrupt_flag.set_lcd_v_counter(true);
        }

        if self.timers.irq(0) && !self.prev_timer_0 {
            self.interrupt_flag.set_timer_0(true);
        }

        if self.timers.irq(1) && !self.prev_timer_1 {
            self.interrupt_flag.set_timer_1(true);
        }

        if self.timers.irq(2) && !self.prev_timer_2 {
            self.interrupt_flag.set_timer_2(true);
        }

        if self.timers.irq(3) && !self.prev_timer_3 {
            self.interrupt_flag.set_timer_3(true);
        }

        if self.keypad.irq && !self.prev_keypad {
            self.interrupt_flag.set_keypad(true);
        }

        self.prev_h_blank = self.ppu.dispstat.h_blank();
        self.prev_v_blank = self.ppu.dispstat.v_blank();
        self.prev_v_counter = self.ppu.dispstat.v_counter();
        self.prev_timer_0 = self.timers.irq(0);
        self.prev_timer_1 = self.timers.irq(1);
        self.prev_timer_2 = self.timers.irq(2);
        self.prev_timer_3 = self.timers.irq(3);

        // TODO
        Ok(())
    }

    #[inline]
    fn high(&self, val: u16) -> u8 {
        (val >> 8) as u8
    }

    #[inline]
    fn low(&self, val: u16) -> u8 {
        val as u8
    }

    #[inline]
    pub fn read_8(&self, addr: u32) -> Result<u8> {
        match addr {
            0x0000_0000..=0x0003_FFFF => Ok(unsafe { *BIOS.get_unchecked(addr as usize) }),
            0x0200_0000..=0x0203_FFFF => Ok(unsafe {
                *self
                    .wram_onboard
                    .get_unchecked((addr - 0x0200_0000) as usize)
            }),
            0x0204_0000..=0x0207_FFFF => Ok(unsafe {
                *self
                    .wram_onboard
                    .get_unchecked((addr - 0x0204_0000) as usize)
            }),
            0x0300_0000..=0x0300_7FFF => Ok(unsafe {
                *self
                    .wram_onchip
                    .get_unchecked((addr - 0x0300_0000) as usize)
            }),
            0x0300_8000..=0x0300_FFFF => Ok(unsafe {
                *self
                    .wram_onchip
                    .get_unchecked((addr - 0x0300_8000) as usize)
            }),
            0x03FF_FF00..=0x03FF_FFFF => Ok(unsafe {
                *self
                    .wram_onchip
                    .get_unchecked((0x7F00 + addr - 0x03FF_FF00) as usize)
            }),
            0x0400_0000..=0x04FF_FFFF => {
                if addr % 2 == 0 {
                    Ok(self.low(self.read_16(addr)?))
                } else {
                    Ok(self.high(self.read_16(addr - 1)?))
                }
            }
            0x0500_0000..=0x07FF_FFFF => self.ppu.vram.read_vram_8(addr),
            0x0800_0000..=0x0FFF_FFFF => self.rom.read_8(addr),
            _ => Ok(0),
        }
    }

    #[inline]
    pub fn read_16(&self, addr: u32) -> Result<u16> {
        let addr = addr & 0xFFFF_FFFE;

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
            // HACK: デフォルト200hらしく、サウンドレジスタ未実装なので一旦
            0x0400_0088 => Ok(0x0000_0200),
            0x0400_00B0..=0x0400_00DF => self.dmas.read_16(addr),
            0x0400_0100..=0x0400_010F => self.timers.read_16(addr),
            0x0400_0130 => self.keypad.read_input(),
            0x0400_0132 => self.keypad.read_cnt(),
            0x0400_0200 => Ok(self.interrupt_enable.0),
            0x0400_0202 => Ok(self.interrupt_flag.0),
            0x0400_0208 => Ok(if self.ime { 1 } else { 0 }),
            0x0400_0000..=0x04FF_FFFF => Ok(0),
            _ => {
                let low = self.read_8(addr)?;
                let high = self.read_8(addr + 1)?;

                Ok((high as u16) << 8 | low as u16)
            }
        }
    }

    pub fn read_32(&self, addr: u32) -> Result<u32> {
        let addr = addr & 0xFFFF_FFFC;

        let low = self.read_16(addr)?;
        let high = self.read_16(addr + 2)?;

        Ok((high as u32) << 16 | low as u32)
    }

    #[inline]
    pub fn write_8(&mut self, addr: u32, val: u8) -> Result<()> {
        match addr {
            0x0200_0000..=0x0203_FFFF => {
                self.wram_onboard[(addr - 0x0200_0000) as usize] = val;

                Ok(())
            }
            0x0204_0000..=0x0207_FFFF => {
                self.wram_onboard[(addr - 0x0204_0000) as usize] = val;

                Ok(())
            }
            0x0300_0000..=0x0300_7FFF => {
                self.wram_onchip[(addr - 0x0300_0000) as usize] = val;

                Ok(())
            }
            0x0300_8000..=0x0300_FFFF => {
                self.wram_onchip[(addr - 0x0300_8000) as usize] = val;

                Ok(())
            }
            0x03FF_FF00..=0x03FF_FFFF => {
                self.wram_onchip[(0x7F00 + addr - 0x03FF_FF00) as usize] = val;

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
            0x0500_0000..=0x07FF_FFFF => self.ppu.vram.write_vram_8(addr, val),
            0x0800_0000..=0x0FFF_FFFF => self.rom.write_8(addr, val),
            _ => Ok(()),
        }
    }

    #[inline]
    pub fn write_16(&mut self, addr: u32, val: u16) -> Result<()> {
        let addr = addr & 0xFFFF_FFFE;

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
            0x0400_00B0..=0x0400_00DF => self.dmas.write_16(addr, val),
            0x0400_0100..=0x0400_010F => self.timers.write_16(addr, val),
            0x0400_0130 => self.keypad.write_input(val),
            0x0400_0132 => self.keypad.write_cnt(val),
            0x0400_0200 => {
                self.interrupt_enable.0 = val;

                Ok(())
            }
            0x0400_0202 => {
                self.interrupt_flag.0 &= !val;

                Ok(())
            }
            0x0400_0208 => {
                self.ime = val > 0;

                Ok(())
            }
            0x0400_0000..=0x04FF_FFFF => Ok(()),
            _ => {
                self.write_8(addr, val as u8)?;
                self.write_8(addr + 1, (val >> 8) as u8)?;

                Ok(())
            }
        }
    }

    pub fn write_32(&mut self, addr: u32, val: u32) -> Result<()> {
        let addr = addr & 0xFFFF_FFFC;

        self.write_16(addr, val as u16)?;
        self.write_16(addr + 2, (val >> 16) as u16)?;

        Ok(())
    }
}

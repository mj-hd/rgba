use anyhow::Result;
use bitfield::bitfield;
use image::{GenericImage, ImageBuffer, Rgba};

use crate::utils::Shared;

use super::{
    bg::{BgScreen, EffectBgScreen},
    oam::SpriteScreen,
    palette::Palette,
    PixelFormat,
};

pub enum BgMode {
    Mode0,
    Mode1,
    Mode2,
    Mode3,
    Mode4,
    Mode5,
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct DispCnt(u16);
    impl Debug;
    pub obj_window_display, _: 15;
    pub window_1_display, _: 14;
    pub window_0_display, _: 13;
    pub screen_display_obj, _: 12;
    pub screen_display_bg3, _: 11;
    pub screen_display_bg2, _: 10;
    pub screen_display_bg1, _: 9;
    pub screen_display_bg0, _: 8;
    pub force_blank, _: 7;
    pub obj_character_vram_mapping, _: 6;
    pub h_blank_interval_free, _: 5;
    pub display_frame_select, _: 4;
    pub cgb_mode, _: 3;
    _bg_mode, _: 2, 0;
}

impl DispCnt {
    pub fn bg_mode(&self) -> BgMode {
        match self._bg_mode() {
            0 => BgMode::Mode0,
            1 => BgMode::Mode1,
            2 => BgMode::Mode2,
            3 => BgMode::Mode3,
            4 => BgMode::Mode4,
            5 => BgMode::Mode5,
            _ => panic!("unknown bg mode"),
        }
    }
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct DispStat(u16);
    impl Debug;
    pub v_count_settings, set_v_count_settings: 15, 8;
    pub v_counter_irq_enable, set_v_counter_irq_enable: 5;
    pub h_blank_irq_enable, set_h_blank_irq_enable: 4;
    pub v_blank_irq_enable, set_v_blank_irq_enable: 3;
    pub v_counter, set_v_counter: 2;
    pub h_blank, set_h_blank: 1;
    pub v_blank, set_v_blank: 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct BgCnt(u16);
    impl Debug;
    _screen_size, _: 15, 14;
    pub display_area_overflow, _: 13;
    pub screen_base_block, _: 12, 8;
    _colors_palette, _: 7;
    pub mosaic, _: 6;
    pub char_base_block, _: 3, 2;
    pub bg_priority, _: 1, 0;
}

impl BgCnt {
    pub fn screen_size(&self) -> (usize, usize) {
        match self._screen_size() {
            0b00 => (256, 256),
            0b01 => (512, 256),
            0b10 => (256, 512),
            0b11 => (512, 512),
            _ => panic!("unknown screen size"),
        }
    }

    pub fn pixel_format(&self) -> PixelFormat {
        match self._colors_palette() {
            false => PixelFormat::Bpp4,
            true => PixelFormat::Bpp8,
        }
    }
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct BgReferencePoint(u32);
    impl Debug;
    pub sign, set_sign: 27;
    pub int, set_int: 26, 8;
    pub frac, set_frac: 7, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct BgRotationScalingParam(u16);
    impl Debug;
    pub sign, set_sign: 15;
    pub int, set_int: 14, 8;
    pub frac, set_frac: 7, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct HorizontalDimension(u16);
    impl Debug;
    pub left_most, set_left_most: 15, 8;
    pub right_most, set_right_most: 7, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct VerticalDimension(u16);
    impl Debug;
    pub top_most, set_top_most: 15, 8;
    pub bottom_most, set_bottom_most: 7, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct WinIn(u16);
    impl Debug;
    pub win_1_color_special_effect, _: 13;
    pub win_1_obj_enable, _: 12;
    pub win_1_bg3_enable, _: 11;
    pub win_1_bg2_enable, _: 10;
    pub win_1_bg1_enable, _: 9;
    pub win_1_bg0_enable, _: 8;
    pub win_0_color_special_effect, _: 5;
    pub win_0_obj_enable, _: 4;
    pub win_0_bg3_enable, _: 3;
    pub win_0_bg2_enable, _: 2;
    pub win_0_bg1_enable, _: 1;
    pub win_0_bg0_enable, _: 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct WinOut(u16);
    impl Debug;
    pub obj_win_color_special_effect, _: 13;
    pub obj_win_obj_enable, _: 12;
    pub obj_win_bg3_enable, _: 11;
    pub obj_win_bg2_enable, _: 10;
    pub obj_win_bg1_enable, _: 9;
    pub obj_win_bg0_enable, _: 8;
    pub outside_color_special_effect, _: 5;
    pub outside_obj_enable, _: 4;
    pub outside_bg3_enable, _: 3;
    pub outside_bg2_enable, _: 2;
    pub outside_bg1_enable, _: 1;
    pub outside_bg0_enable, _: 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct Mosaic(u16);
    impl Debug;
    pub obj_mosaic_v_size, _: 15, 12;
    pub obj_mosaic_h_size, _: 11, 7;
    pub bg_mosaic_v_size, _: 7, 4;
    pub bg_mosaic_h_size, _: 3, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct BldCnt(u16);
    impl Debug;
    pub bd_2_target_pixel, _: 13;
    pub obj_2_target_pixel, _: 12;
    pub bg3_2_target_pixel, _: 11;
    pub bg2_2_target_pixel, _: 10;
    pub bg1_2_target_pixel, _: 9;
    pub bg0_2_target_pixel, _: 8;
    pub color_special_effect, _: 7, 6;
    pub bd_1_target_pixel, _: 5;
    pub obj_1_target_pixel, _: 4;
    pub bg3_1_target_pixel, _: 3;
    pub bg2_1_target_pixel, _: 2;
    pub bg1_1_target_pixel, _: 1;
    pub bg0_1_target_pixel, _: 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct BldAlpha(u16);
    impl Debug;
    pub evb_coef, _: 12, 8;
    pub eva_coef, _: 4, 0;
}

pub struct Vram {
    palette: Box<[u8; 0x400]>,
    vram: Box<[u8; 0x1_8000]>,
    sprite: Box<[u8; 0x4000]>,
}

impl Vram {
    #[inline]
    pub fn read_vram_8(&self, addr: u32) -> Result<u8> {
        match addr {
            0x0500_0000..=0x0500_03FF => {
                Ok(unsafe { *self.palette.get_unchecked((addr - 0x0500_0000) as usize) })
            }
            0x0500_0400..=0x0500_07FF => {
                Ok(unsafe { *self.palette.get_unchecked((addr - 0x0500_0400) as usize) })
            }
            0x0600_0000..=0x0601_7FFF => {
                Ok(unsafe { *self.vram.get_unchecked((addr - 0x0600_0000) as usize) })
            }
            0x0602_0000..=0x0602_7FFF => {
                Ok(unsafe { *self.vram.get_unchecked((addr - 0x0602_0000) as usize) })
            }
            0x0700_0000..=0x0700_3FFF => {
                Ok(unsafe { *self.sprite.get_unchecked((addr - 0x0700_0000) as usize) })
            }
            _ => Ok(0),
        }
    }

    #[inline]
    pub fn read_vram_16(&self, addr: u32) -> Result<u16> {
        let low = self.read_vram_8(addr)?;
        let high = self.read_vram_8(addr + 1)?;

        Ok((high as u16) << 8 | low as u16)
    }

    pub fn read_vram_32(&self, addr: u32) -> Result<u32> {
        let low = self.read_vram_16(addr)?;
        let high = self.read_vram_16(addr + 2)?;

        Ok((high as u32) << 16 | low as u32)
    }

    #[inline]
    pub fn write_vram_8(&mut self, addr: u32, val: u8) -> Result<()> {
        match addr {
            0x0500_0000..=0x0500_03FF => {
                unsafe {
                    *self
                        .palette
                        .get_unchecked_mut((addr - 0x0500_0000) as usize) = val;
                }

                //debug!("WRITE PALETTE: ({:08X})={:02X}", addr, val);

                Ok(())
            }
            0x0500_0400..=0x0500_07FF => {
                unsafe {
                    *self
                        .palette
                        .get_unchecked_mut((addr - 0x0500_0400) as usize) = val;
                }

                //debug!("WRITE PALETTE: ({:08X})={:02X}", addr, val);

                Ok(())
            }
            0x0600_0000..=0x0601_7FFF => {
                unsafe {
                    *self.vram.get_unchecked_mut((addr - 0x0600_0000) as usize) = val;
                }

                //trace!("WRITE VRAM: ({:08X})={:02X}", addr, val);

                Ok(())
            }
            0x0602_0000..=0x0602_7FFF => {
                unsafe {
                    *self.vram.get_unchecked_mut((addr - 0x0602_0000) as usize) = val;
                }

                //trace!("WRITE VRAM: ({:08X})={:02X}", addr, val);

                Ok(())
            }
            0x0700_0000..=0x0700_3FFF => {
                unsafe {
                    *self.sprite.get_unchecked_mut((addr - 0x0700_0000) as usize) = val;
                }

                //trace!("WRITE SPRITE: ({:08X})={:02X}", addr, val);

                Ok(())
            }
            _ => Ok(()),
        }
    }

    pub fn write_vram_16(&mut self, addr: u32, val: u16) -> Result<()> {
        self.write_vram_8(addr, val as u8)?;
        self.write_vram_8(addr + 1, (val >> 8) as u8)?;

        Ok(())
    }
}

impl Default for Vram {
    fn default() -> Self {
        Self {
            palette: Box::new([0; 0x400]),
            vram: Box::new([0; 0x1_8000]),
            sprite: Box::new([0; 0x4000]),
        }
    }
}

enum Transform {
    None,
    Rotate(f32, f32, f32, f32, f32, f32),
    Scale(f32, f32, f32, f32, f32, f32),
}

#[derive(Default)]
struct Win {
    h: HorizontalDimension,
    v: VerticalDimension,
}

#[derive(PartialEq)]
enum Mode {
    Visible,
    HBlank,
    VBlank,
}

pub struct Ppu {
    pub vram: Shared<Vram>,

    stalls: u8,
    h_count: u16,

    dispcnt: DispCnt,
    pub dispstat: DispStat,
    green_swap: bool,
    v_count: u8,

    bg_screens: [BgScreen; 2],
    effect_bg_screens: [EffectBgScreen; 2],
    sprite_screen: SpriteScreen,
    wins: [Win; 2],

    win_in: WinIn,
    win_out: WinOut,

    mosaic: Mosaic,

    bld_cnt: BldCnt,
    bld_alpha: BldAlpha,
    bld_y: u8,

    mode: Mode,

    current_frame: Box<ImageBuffer<Rgba<u8>, Vec<u8>>>,
}

impl Ppu {
    pub fn new() -> Self {
        Self {
            vram: Default::default(),
            dispcnt: Default::default(),
            green_swap: Default::default(),
            dispstat: Default::default(),
            stalls: 4,
            v_count: 0,
            h_count: 0,
            bg_screens: [Default::default(), Default::default()],
            effect_bg_screens: [Default::default(), Default::default()],
            sprite_screen: Default::default(),
            wins: [Default::default(), Default::default()],
            current_frame: Box::new(ImageBuffer::new(240, 160)),
            win_in: Default::default(),
            win_out: Default::default(),
            mosaic: Default::default(),
            bld_cnt: Default::default(),
            bld_alpha: Default::default(),
            bld_y: Default::default(),
            mode: Mode::Visible,
        }
    }

    pub fn tick(&mut self) -> Result<()> {
        self.stalls -= 1;

        if self.stalls > 0 {
            return Ok(());
        }

        self.stalls = 4;

        match self.h_count {
            // Visible
            0..=239 => {
                self.mode = Mode::Visible;

                self.dispstat.set_h_blank(false);
            }
            // HBlank
            240..=307 => {
                self.mode = Mode::HBlank;

                self.dispstat.set_h_blank(true);
            }
            _ => {}
        }

        match self.v_count {
            // Visible
            0..=159 => {
                self.dispstat.set_v_blank(false);
            }
            // VBlank
            160..=227 => {
                self.mode = Mode::VBlank;

                self.dispstat.set_v_blank(true);
            }
            _ => {}
        }

        if self.v_count as u16 == self.dispstat.v_count_settings() {
            self.dispstat.set_v_counter(true);
        } else {
            self.dispstat.set_v_counter(false);
        }

        if self.mode == Mode::Visible {
            self.render_bg()?;
            if self.h_count == 0 {
                self.sprite_screen.clear(self.v_count as u32);
                self.sprite_screen
                    .render_sprites(&mut self.vram, self.dispcnt)?;
            }
        }

        self.h_count += 1;

        if self.h_count == 308 {
            if self.mode != Mode::VBlank {
                self.composite()?;
            }

            self.h_count = 0;
            self.v_count += 1;
        }

        if self.v_count == 228 {
            self.v_count = 0;
        }

        Ok(())
    }

    fn render_bg(&mut self) -> Result<()> {
        let pos = (self.h_count as u32, self.v_count as u32);

        match self.dispcnt.bg_mode() {
            BgMode::Mode0 => {
                if self.dispcnt.screen_display_bg0() {
                    self.bg_screens[0].render_tile_screen(&mut self.vram, pos)?;
                }

                if self.dispcnt.screen_display_bg1() {
                    self.bg_screens[1].render_tile_screen(&mut self.vram, pos)?;
                }

                if self.dispcnt.screen_display_bg2() {
                    self.effect_bg_screens[0]
                        .bg
                        .render_tile_screen(&mut self.vram, pos)?;
                }

                if self.dispcnt.screen_display_bg3() {
                    self.effect_bg_screens[1]
                        .bg
                        .render_tile_screen(&mut self.vram, pos)?;
                }

                Ok(())
            }
            BgMode::Mode1 => Ok(()),
            BgMode::Mode3 => {
                if self.dispcnt.screen_display_bg2() {
                    self.effect_bg_screens[0].render_bitmap_screen_full(
                        &mut self.vram,
                        self.dispcnt,
                        pos,
                        (240, 160),
                    )?;
                }

                Ok(())
            }
            BgMode::Mode4 => {
                if self.dispcnt.screen_display_bg2() {
                    self.effect_bg_screens[0].render_bitmap_screen_256(
                        self.dispcnt,
                        &mut self.vram,
                        pos,
                    )?;
                }

                Ok(())
            }
            BgMode::Mode5 => {
                if self.dispcnt.screen_display_bg2() {
                    self.effect_bg_screens[0].render_bitmap_screen_full(
                        &mut self.vram,
                        self.dispcnt,
                        pos,
                        (160, 128),
                    )?;
                }

                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn composite(&mut self) -> Result<()> {
        let y = self.v_count as u32;

        let bg_pixel = Palette::new_bg(0).get_bg_color(&self.vram)?.to_pixel();

        for x in 0..240 {
            let mut pixel = bg_pixel;

            if !self.dispcnt.force_blank() {
                if self.dispcnt.screen_display_bg0() {
                    let p = self.bg_screens[0].frame.get_pixel(
                        (self.bg_screens[0].offset.0 as u32 + x).clamp(0, 239),
                        (self.bg_screens[0].offset.1 as u32 + y).clamp(0, 239),
                    );

                    if p.0[3] != 0x00 {
                        pixel = p;
                    }
                }

                if self.dispcnt.screen_display_bg1() {
                    let p = self.bg_screens[1].frame.get_pixel(
                        (self.bg_screens[1].offset.0 as u32 + x).clamp(0, 239),
                        (self.bg_screens[1].offset.1 as u32 + y).clamp(0, 239),
                    );

                    if p.0[3] != 0x00 {
                        pixel = p;
                    }
                }

                if self.dispcnt.screen_display_bg2() {
                    let p = self.effect_bg_screens[0].bg.frame.get_pixel(
                        (self.effect_bg_screens[0].bg.offset.0 as u32 + x).clamp(0, 239),
                        (self.effect_bg_screens[0].bg.offset.1 as u32 + y).clamp(0, 239),
                    );

                    if p.0[3] != 0x00 {
                        pixel = p;
                    }
                }

                if self.dispcnt.screen_display_bg3() {
                    let p = self.effect_bg_screens[1].bg.frame.get_pixel(
                        (self.effect_bg_screens[1].bg.offset.0 as u32 + x).clamp(0, 239),
                        (self.effect_bg_screens[1].bg.offset.1 as u32 + y).clamp(0, 239),
                    );

                    if p.0[3] != 0x00 {
                        pixel = p;
                    }
                }
            }

            let p = self.sprite_screen.frame.get_pixel(x, y);

            if p.0[3] != 0x00 {
                pixel = p;
            }

            unsafe {
                self.current_frame.unsafe_put_pixel(x, y, pixel);
            }
        }

        Ok(())
    }

    pub fn read_dispcnt(&self) -> Result<u16> {
        Ok(self.dispcnt.0)
    }
    pub fn write_dispcnt(&mut self, val: u16) -> Result<()> {
        self.dispcnt = DispCnt(val);
        //trace!("WRITE DISPCNT: {}({:?})", val, self.dispcnt);

        Ok(())
    }

    pub fn read_green_swap(&self) -> Result<u16> {
        Ok(if self.green_swap { 1 } else { 0 })
    }
    pub fn write_green_swap(&mut self, val: u16) -> Result<()> {
        self.green_swap = val > 0;

        Ok(())
    }

    pub fn read_dispstat(&self) -> Result<u16> {
        Ok(self.dispstat.0)
    }
    pub fn write_dispstat(&mut self, val: u16) -> Result<()> {
        // TODO: mask readonly
        self.dispstat = DispStat(val);
        //trace!("WRITE DISPSTAT: {}({:?})", val, self.dispstat);

        Ok(())
    }

    pub fn read_v_count(&self) -> Result<u16> {
        Ok(self.v_count as u16)
    }

    pub fn read_bg_0_cnt(&self) -> Result<u16> {
        Ok(self.bg_screens[0].cnt.0)
    }
    pub fn write_bg_0_cnt(&mut self, val: u16) -> Result<()> {
        self.bg_screens[0].cnt = BgCnt(val);
        //trace!("WRITE BG0CNT: {}({:?})", val, self.bg_screens[0].cnt);

        Ok(())
    }

    pub fn read_bg_1_cnt(&self) -> Result<u16> {
        Ok(self.bg_screens[1].cnt.0)
    }
    pub fn write_bg_1_cnt(&mut self, val: u16) -> Result<()> {
        self.bg_screens[1].cnt = BgCnt(val);
        //trace!("WRITE BG1CNT: {}({:?})", val, self.bg_screens[1].cnt);

        Ok(())
    }

    pub fn read_bg_2_cnt(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[0].bg.cnt.0)
    }
    pub fn write_bg_2_cnt(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[0].bg.cnt = BgCnt(val);
        //trace!(
        //    "WRITE BG2CNT: {}({:?})",
        //    val,
        //    self.effect_bg_screens[0].bg.cnt
        //);

        Ok(())
    }

    pub fn read_bg_3_cnt(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[1].bg.cnt.0)
    }
    pub fn write_bg_3_cnt(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[1].bg.cnt = BgCnt(val);
        //trace!(
        //    "WRITE BG3CNT: {}({:?})",
        //    val,
        //    self.effect_bg_screens[1].bg.cnt
        //);

        Ok(())
    }

    pub fn read_bg_0_offset_x(&self) -> Result<u16> {
        Ok(self.bg_screens[0].offset.0 & 0x1FF)
    }
    pub fn write_bg_0_offset_x(&mut self, val: u16) -> Result<()> {
        self.bg_screens[0].offset.0 = val & 0x1FF;
        //trace!(
        //    "WRITE BG0 OFFSET X: {}({:?})",
        //    val,
        //    self.bg_screens[0].offset.0
        //);

        Ok(())
    }
    pub fn read_bg_0_offset_y(&self) -> Result<u16> {
        Ok(self.bg_screens[0].offset.1 & 0x1FF)
    }
    pub fn write_bg_0_offset_y(&mut self, val: u16) -> Result<()> {
        self.bg_screens[0].offset.1 = val & 0x1FF;
        //trace!(
        //    "WRITE BG0 OFFSET Y: {}({:?})",
        //    val,
        //    self.bg_screens[0].offset.1
        //);

        Ok(())
    }

    pub fn read_bg_1_offset_x(&self) -> Result<u16> {
        Ok(self.bg_screens[1].offset.0 & 0x1FF)
    }
    pub fn write_bg_1_offset_x(&mut self, val: u16) -> Result<()> {
        self.bg_screens[1].offset.0 = val & 0x1FF;
        //trace!(
        //    "WRITE BG1 OFFSET X: {}({:?})",
        //    val,
        //    self.bg_screens[1].offset.0
        //);

        Ok(())
    }
    pub fn read_bg_1_offset_y(&self) -> Result<u16> {
        Ok(self.bg_screens[1].offset.1 & 0x1FF)
    }
    pub fn write_bg_1_offset_y(&mut self, val: u16) -> Result<()> {
        self.bg_screens[1].offset.1 = val & 0x1FF;
        //trace!(
        //    "WRITE BG1 OFFSET Y: {}({:?})",
        //    val,
        //    self.bg_screens[1].offset.1
        //);

        Ok(())
    }

    pub fn read_bg_2_offset_x(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[0].bg.offset.0 & 0x1FF)
    }
    pub fn write_bg_2_offset_x(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[0].bg.offset.0 = val & 0x1FF;

        //trace!(
        //    "WRITE BG2 OFFSET X: {}({:?})",
        //    val,
        //    self.effect_bg_screens[0].bg.offset.0
        //);

        Ok(())
    }
    pub fn read_bg_2_offset_y(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[0].bg.offset.1 & 0x1FF)
    }
    pub fn write_bg_2_offset_y(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[0].bg.offset.1 = val & 0x1FF;

        //trace!(
        //    "WRITE BG2 OFFSET Y: {}({:?})",
        //    val,
        //    self.effect_bg_screens[0].bg.offset.1
        //);

        Ok(())
    }

    pub fn read_bg_3_offset_x(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[1].bg.offset.0 & 0x1FF)
    }

    pub fn write_bg_3_offset_x(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[1].bg.offset.0 = val & 0x1FF;

        //trace!(
        //    "WRITE BG3 OFFSET X: {}({:?})",
        //    val,
        //    self.effect_bg_screens[1].bg.offset.0
        //);

        Ok(())
    }
    pub fn read_bg_3_offset_y(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[1].bg.offset.1 & 0x1FF)
    }
    pub fn write_bg_3_offset_y(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[1].bg.offset.1 = val & 0x1FF;

        //trace!(
        //    "WRITE BG3 OFFSET Y: {}({:?})",
        //    val,
        //    self.effect_bg_screens[1].bg.offset.1
        //);

        Ok(())
    }

    pub fn read_bg_2_x_l(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[0].x_l)
    }
    pub fn write_bg_2_x_l(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[0].x_l = val;
        Ok(())
    }
    pub fn read_bg_2_x_h(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[0].x_h)
    }
    pub fn write_bg_2_x_h(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[0].x_h = val;
        Ok(())
    }
    pub fn read_bg_2_y_l(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[0].y_l)
    }
    pub fn write_bg_2_y_l(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[0].y_l = val;
        Ok(())
    }
    pub fn read_bg_2_y_h(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[0].y_h)
    }
    pub fn write_bg_2_y_h(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[0].y_h = val;
        Ok(())
    }

    pub fn read_bg_2_p_a(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[0].pa.0)
    }
    pub fn write_bg_2_p_a(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[0].pa = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_2_p_b(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[0].pb.0)
    }
    pub fn write_bg_2_p_b(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[0].pb = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_2_p_c(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[0].pc.0)
    }
    pub fn write_bg_2_p_c(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[0].pc = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_2_p_d(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[0].pd.0)
    }
    pub fn write_bg_2_p_d(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[0].pd = BgRotationScalingParam(val);
        Ok(())
    }

    pub fn read_bg_3_x_l(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[1].x_l)
    }
    pub fn write_bg_3_x_l(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[1].x_l = val;
        Ok(())
    }
    pub fn read_bg_3_x_h(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[1].x_h)
    }
    pub fn write_bg_3_x_h(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[1].x_h = val;
        Ok(())
    }
    pub fn read_bg_3_y_l(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[1].y_l)
    }
    pub fn write_bg_3_y_l(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[1].y_l = val;
        Ok(())
    }
    pub fn read_bg_3_y_h(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[1].y_h)
    }
    pub fn write_bg_3_y_h(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[1].y_h = val;
        Ok(())
    }

    pub fn read_bg_3_p_a(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[1].pa.0)
    }
    pub fn write_bg_3_p_a(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[1].pa = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_3_p_b(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[1].pb.0)
    }
    pub fn write_bg_3_p_b(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[1].pb = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_3_p_c(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[1].pc.0)
    }
    pub fn write_bg_3_p_c(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[1].pc = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_3_p_d(&self) -> Result<u16> {
        Ok(self.effect_bg_screens[1].pd.0)
    }
    pub fn write_bg_3_p_d(&mut self, val: u16) -> Result<()> {
        self.effect_bg_screens[1].pd = BgRotationScalingParam(val);
        Ok(())
    }

    pub fn read_win_0_h(&self) -> Result<u16> {
        Ok(self.wins[0].h.0)
    }
    pub fn write_win_0_h(&mut self, val: u16) -> Result<()> {
        self.wins[0].h = HorizontalDimension(val);
        Ok(())
    }
    pub fn read_win_1_h(&self) -> Result<u16> {
        Ok(self.wins[1].h.0)
    }
    pub fn write_win_1_h(&mut self, val: u16) -> Result<()> {
        self.wins[1].h = HorizontalDimension(val);
        Ok(())
    }
    pub fn read_win_0_v(&self) -> Result<u16> {
        Ok(self.wins[0].v.0)
    }
    pub fn write_win_0_v(&mut self, val: u16) -> Result<()> {
        self.wins[0].v = VerticalDimension(val);
        Ok(())
    }
    pub fn read_win_1_v(&self) -> Result<u16> {
        Ok(self.wins[1].v.0)
    }
    pub fn write_win_1_v(&mut self, val: u16) -> Result<()> {
        self.wins[1].v = VerticalDimension(val);
        Ok(())
    }

    pub fn read_win_in(&self) -> Result<u16> {
        Ok(self.win_in.0)
    }
    pub fn write_win_in(&mut self, val: u16) -> Result<()> {
        self.win_in = WinIn(val);
        Ok(())
    }
    pub fn read_win_out(&self) -> Result<u16> {
        Ok(self.win_out.0)
    }
    pub fn write_win_out(&mut self, val: u16) -> Result<()> {
        self.win_out = WinOut(val);
        Ok(())
    }

    pub fn read_mosaic(&self) -> Result<u16> {
        Ok(self.mosaic.0)
    }
    pub fn write_mosaic(&mut self, val: u16) -> Result<()> {
        self.mosaic = Mosaic(val);
        Ok(())
    }

    pub fn read_bld_cnt(&self) -> Result<u16> {
        Ok(self.bld_cnt.0)
    }
    pub fn write_bld_cnt(&mut self, val: u16) -> Result<()> {
        self.bld_cnt = BldCnt(val);
        Ok(())
    }
    pub fn read_bld_alpha(&self) -> Result<u16> {
        Ok(self.bld_alpha.0)
    }
    pub fn write_bld_alpha(&mut self, val: u16) -> Result<()> {
        self.bld_alpha = BldAlpha(val);
        Ok(())
    }
    pub fn read_bld_y(&self) -> Result<u16> {
        Ok(self.bld_y as u16)
    }
    pub fn write_bld_y(&mut self, val: u16) -> Result<()> {
        self.bld_y = val as u8;
        Ok(())
    }

    pub fn render(&self) -> Vec<u8> {
        self.current_frame.clone().into_raw()
    }
}

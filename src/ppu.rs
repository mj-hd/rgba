use std::cell::{RefCell, RefMut};

use anyhow::Result;
use bitfield::bitfield;
use image::{ImageBuffer, Rgba};
use log::trace;

enum BgMode {
    Mode0,
    Mode1,
    Mode2,
    Mode3,
    Mode4,
    Mode5,
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct DispCnt(u16);
    impl Debug;
    obj_window_display, _: 15;
    window_1_display, _: 14;
    window_0_display, _: 13;
    screen_display_obj, _: 12;
    screen_display_bg3, _: 11;
    screen_display_bg2, _: 10;
    screen_display_bg1, _: 9;
    screen_display_bg0, _: 8;
    force_blank, _: 7;
    obj_character_vram_mapping, _: 6;
    h_blank_interval_free, _: 5;
    display_frame_select, _: 4;
    cgb_mode, _: 3;
    _bg_mode, _: 2, 0;
}

impl DispCnt {
    fn bg_mode(&self) -> BgMode {
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
    struct BgCnt(u16);
    impl Debug;
    _screen_size, _: 15, 14;
    display_area_overflow, _: 13;
    screen_base_block, _: 12, 8;
    colors_palette, _: 7;
    mosaic, _: 6;
    char_base_block, _: 3, 2;
    bg_priority, _: 1, 0;
}

impl BgCnt {
    fn screen_size(&self) -> (usize, usize) {
        match self._screen_size() {
            0b00 => (256, 256),
            0b01 => (512, 256),
            0b10 => (256, 512),
            0b11 => (512, 512),
            _ => panic!("unknown screen size"),
        }
    }
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct BgReferencePoint(u32);
    impl Debug;
    sign, set_sign: 27;
    int, set_int: 26, 8;
    frac, set_frac: 7, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct BgRotationScalingParam(u16);
    impl Debug;
    sign, set_sign: 15;
    int, set_int: 14, 8;
    frac, set_frac: 7, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct HorizontalDimension(u16);
    impl Debug;
    left_most, set_left_most: 15, 8;
    right_most, set_right_most: 7, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct VerticalDimension(u16);
    impl Debug;
    top_most, set_top_most: 15, 8;
    bottom_most, set_bottom_most: 7, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct WinIn(u16);
    impl Debug;
    win_1_color_special_effect, _: 13;
    win_1_obj_enable, _: 12;
    win_1_bg3_enable, _: 11;
    win_1_bg2_enable, _: 10;
    win_1_bg1_enable, _: 9;
    win_1_bg0_enable, _: 8;
    win_0_color_special_effect, _: 5;
    win_0_obj_enable, _: 4;
    win_0_bg3_enable, _: 3;
    win_0_bg2_enable, _: 2;
    win_0_bg1_enable, _: 1;
    win_0_bg0_enable, _: 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct WinOut(u16);
    impl Debug;
    obj_win_color_special_effect, _: 13;
    obj_win_obj_enable, _: 12;
    obj_win_bg3_enable, _: 11;
    obj_win_bg2_enable, _: 10;
    obj_win_bg1_enable, _: 9;
    obj_win_bg0_enable, _: 8;
    outside_color_special_effect, _: 5;
    outside_obj_enable, _: 4;
    outside_bg3_enable, _: 3;
    outside_bg2_enable, _: 2;
    outside_bg1_enable, _: 1;
    outside_bg0_enable, _: 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct Mosaic(u16);
    impl Debug;
    obj_mosaic_v_size, _: 15, 12;
    obj_mosaic_h_size, _: 11, 7;
    bg_mosaic_v_size, _: 7, 4;
    bg_mosaic_h_size, _: 3, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct BldCnt(u16);
    impl Debug;
    bd_2_target_pixel, _: 13;
    obj_2_target_pixel, _: 12;
    bg3_2_target_pixel, _: 11;
    bg2_2_target_pixel, _: 10;
    bg1_2_target_pixel, _: 9;
    bg0_2_target_pixel, _: 8;
    color_special_effect, _: 7, 6;
    bd_1_target_pixel, _: 5;
    obj_1_target_pixel, _: 4;
    bg3_1_target_pixel, _: 3;
    bg2_1_target_pixel, _: 2;
    bg1_1_target_pixel, _: 1;
    bg0_1_target_pixel, _: 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct BldAlpha(u16);
    impl Debug;
    evb_coef, _: 12, 8;
    eva_coef, _: 4, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct Color(u16);
    impl Debug;
    u8, blue, _: 14, 10;
    u8, green, _: 9, 5;
    u8, red, _: 4, 0;
}

impl Color {
    fn to_pixel(&self) -> Rgba<u8> {
        Rgba([self.red() * 8, self.green() * 8, self.blue() * 8, 0xFF])
    }
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct Oam0(u16);
    impl Debug;
    shape, _: 15, 14;
    colors_palettes, _: 13;
    obj_mosaic, _: 12;
    obj_mode, _: 11, 10;
    obj_disable_double_size, _: 9;
    rotation_scaling, _: 8;
    y, _: 7, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct Oam1(u16);
    impl Debug;
    obj_size, _: 15, 14;
    v_flip, _: 13;
    h_flip, _: 12;
    rotation_scaling_parameter_selection, _: 11, 9;
    rotation_scaling, _: 8;
    x, _: 7, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct Oam2(u16);
    impl Debug;
    palette_number, _: 15, 12;
    priority_relative_to_bg, _: 11, 10;
    character_name, _: 9, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct TextBgScreen(u16);
    impl Debug;
    palette_number, _: 15, 12;
    v_flip, _: 11;
    h_flip, _: 10;
    tile_number, _: 9, 0;
}

pub struct Vram {
    palette: Box<[u8; 0x400]>,
    vram: Box<[u8; 0x1_8000]>,
    sprite: Box<[u8; 0x400]>,
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
                self.palette[(addr - 0x0500_0000) as usize] = val;

                Ok(())
            }
            0x0500_0400..=0x0500_07FF => {
                self.palette[(addr - 0x0500_0400) as usize] = val;

                Ok(())
            }
            0x0600_0000..=0x0601_7FFF => {
                self.vram[(addr - 0x0600_0000) as usize] = val;

                trace!("WRITE VRAM: ({:08X})={:02X}", addr, val);

                Ok(())
            }
            0x0602_0000..=0x0602_7FFF => {
                self.vram[(addr - 0x0602_0000) as usize] = val;

                trace!("WRITE VRAM: ({:08X})={:02X}", addr, val);

                Ok(())
            }
            0x0700_0000..=0x0700_3FFF => {
                self.sprite[(addr - 0x0700_0000) as usize] = val;

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
            sprite: Box::new([0; 0x400]),
        }
    }
}

struct Bg {
    frame: Box<ImageBuffer<Rgba<u8>, Vec<u8>>>,
    cnt: BgCnt,
    offset: (u16, u16),
}

impl Default for Bg {
    fn default() -> Self {
        Self {
            frame: Box::new(ImageBuffer::new(512, 512)),
            cnt: BgCnt(0),
            offset: (0, 0),
        }
    }
}

trait BgRender {
    fn render_tile_screen(&mut self, vram: RefMut<Vram>, pos: (u32, u32)) -> Result<()>;
}

impl Bg {
    fn render_tile_screen_256colors(&mut self, vram: RefMut<Vram>, pos: (u32, u32)) -> Result<()> {
        // 8bpp

        let (x, y) = pos;

        let tile_index = x / 8 + (y / 8) * 32;

        let priority = self.cnt.bg_priority();
        let map_base_addr = 0x0600_0000 + self.cnt.screen_base_block() as u32 * 2 * 1024;
        let char_base_addr = 0x0600_0000 + self.cnt.char_base_block() as u32 * 16 * 1024;
        let screen_size = self.cnt.screen_size();

        let map = TextBgScreen(vram.read_vram_16(map_base_addr + tile_index * 2)?);
        let tile_addr = char_base_addr + (map.tile_number() as u32 * 64);
        let tile_row = vram.read_vram_32(tile_addr + (y % 8) * 8)?;

        // left pixel
        let color_offset = (tile_row & 0xFF) as u32;

        let palette_base_addr = 0x0500_0000;

        let color = Color(vram.read_vram_16(palette_base_addr + color_offset)?);

        if color_offset == 0 {
            self.frame.put_pixel(x, y, Rgba([0, 0, 0, 0]));
        } else {
            trace!(
                "256TILE: ({},{}) map({:08X})={:08X}, tile({:08X}), color({})={:8X}",
                x,
                y,
                map_base_addr + tile_index * 2,
                map.0,
                tile_addr,
                color_offset,
                color.0
            );
            self.frame.put_pixel(x, y, color.to_pixel());
        }

        Ok(())
    }

    fn render_tile_screen_16colors(&mut self, vram: RefMut<Vram>, pos: (u32, u32)) -> Result<()> {
        // 4bpp

        let (x, y) = pos;

        let tile_index = x / 8 + (y / 8) * 32;

        let priority = self.cnt.bg_priority();
        let map_base_addr = 0x0600_0000 + self.cnt.screen_base_block() as u32 * 2 * 1024;
        let char_base_addr = 0x0600_0000 + self.cnt.char_base_block() as u32 * 16 * 1024;
        let screen_size = self.cnt.screen_size();

        let map = TextBgScreen(vram.read_vram_16(map_base_addr + tile_index * 2)?);
        let tile_addr = char_base_addr + (map.tile_number() as u32 * 32);
        let tile_row = vram.read_vram_32(tile_addr + (y % 8) * 4)?;

        let palette_base_addr = 0x0500_0000u32 + (map.palette_number() as u32) * 2 * 16;

        let color_index = ((tile_row >> (4 * (x % 8))) & 0xF) as usize;
        let color = Color(vram.read_vram_16(palette_base_addr + color_index as u32 * 2)?);

        if color_index == 0 {
            self.frame.put_pixel(x, y, Rgba([0, 0, 0, 0]));
        } else {
            trace!(
                "16TILE: ({},{}) map({:08X})={:08X}, tile({:08X}), color({})={:8X}",
                x,
                y,
                map_base_addr + tile_index * 2,
                map.0,
                tile_addr,
                color_index,
                color.0
            );
            self.frame.put_pixel(x, y, color.to_pixel());
        }

        Ok(())
    }
}

impl BgRender for Bg {
    fn render_tile_screen(&mut self, vram: RefMut<Vram>, pos: (u32, u32)) -> Result<()> {
        if self.cnt.colors_palette() {
            self.render_tile_screen_256colors(vram, pos)
        } else {
            self.render_tile_screen_16colors(vram, pos)
        }
    }
}

#[derive(Default)]
struct EffectBg {
    bg: Bg,

    x_l: u16,
    x_h: u16,
    y_l: u16,
    y_h: u16,

    pa: BgRotationScalingParam,
    pb: BgRotationScalingParam,
    pc: BgRotationScalingParam,
    pd: BgRotationScalingParam,
}

trait EffectBgRender {
    fn render_bitmap_screen_full(&mut self, vram: RefMut<Vram>, pos: (u32, u32)) -> Result<()>;
    fn render_bitmap_screen_256(
        &mut self,
        dispcnt: DispCnt,
        vram: RefMut<Vram>,
        pos: (u32, u32),
    ) -> Result<()>;
}

impl BgRender for EffectBg {
    fn render_tile_screen(&mut self, vram: RefMut<Vram>, pos: (u32, u32)) -> Result<()> {
        self.bg.render_tile_screen(vram, pos)
    }
}

impl EffectBgRender for EffectBg {
    fn render_bitmap_screen_full(&mut self, vram: RefMut<Vram>, pos: (u32, u32)) -> Result<()> {
        // Bitmap Mode 240x160 pixels, 32768 colors

        let (x, y) = pos;

        let index = y * 480 + x * 2;

        let base_addr = 0x0600_0000;
        let color = Color(vram.read_vram_16(base_addr + index)?);

        self.bg.frame.put_pixel(x, y, color.to_pixel());

        Ok(())
    }

    fn render_bitmap_screen_256(
        &mut self,
        dispcnt: DispCnt,
        vram: RefMut<Vram>,
        pos: (u32, u32),
    ) -> Result<()> {
        // Bitmap Mode 240x160 pixels, 256 colors

        let (x, y) = pos;

        let index = y * 240 + x;

        let base_addr = 0x0600_0000
            + if dispcnt.display_frame_select() {
                0xA000
            } else {
                0x0000
            };
        let color_index = vram.read_vram_8(base_addr + index)?;

        // TODO 共通化
        let palette_base_addr = 0x0500_0000;
        let color = Color(vram.read_vram_16(palette_base_addr + color_index as u32)?);

        if color_index == 0 {
            self.bg.frame.put_pixel(x, y, Rgba([0, 0, 0, 0]));
        } else {
            self.bg.frame.put_pixel(x, y, color.to_pixel());
        }

        Ok(())
    }
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
    pub vram: RefCell<Vram>,

    stalls: u8,
    h_count: u16,

    dispcnt: DispCnt,
    pub dispstat: DispStat,
    green_swap: bool,
    v_count: u8,

    bgs: [Bg; 2],
    effect_bgs: [EffectBg; 2],
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
            vram: RefCell::new(Default::default()),
            dispcnt: Default::default(),
            green_swap: Default::default(),
            dispstat: Default::default(),
            stalls: 4,
            v_count: 0,
            h_count: 0,
            bgs: [Default::default(), Default::default()],
            effect_bgs: [Default::default(), Default::default()],
            wins: [Default::default(), Default::default()],
            current_frame: Box::new(ImageBuffer::new(512, 512)),
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
        }

        self.h_count += 1;

        if self.h_count == 308 {
            self.composite()?;

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
                    self.bgs[0].render_tile_screen(self.vram.borrow_mut(), pos)?;
                }

                if self.dispcnt.screen_display_bg1() {
                    self.bgs[1].render_tile_screen(self.vram.borrow_mut(), pos)?;
                }

                if self.dispcnt.screen_display_bg2() {
                    self.effect_bgs[0]
                        .bg
                        .render_tile_screen(self.vram.borrow_mut(), pos)?;
                }

                if self.dispcnt.screen_display_bg3() {
                    self.effect_bgs[1]
                        .bg
                        .render_tile_screen(self.vram.borrow_mut(), pos)?;
                }

                Ok(())
            }
            BgMode::Mode1 => Ok(()),
            BgMode::Mode3 => {
                if self.dispcnt.screen_display_bg2() {
                    self.effect_bgs[0].render_bitmap_screen_full(self.vram.borrow_mut(), pos)?;
                }

                Ok(())
            }
            BgMode::Mode4 => {
                if self.dispcnt.screen_display_bg2() {
                    self.effect_bgs[0].render_bitmap_screen_256(
                        self.dispcnt,
                        self.vram.borrow_mut(),
                        pos,
                    )?;
                }

                Ok(())
            }
            BgMode::Mode5 => Ok(()),
            _ => Ok(()),
        }
    }

    fn composite(&mut self) -> Result<()> {
        let y = self.v_count as u32;

        // TODO 共通化
        let palette_base_addr = 0x0500_0000;
        let bg_pixel = Color(self.vram.borrow_mut().read_vram_16(palette_base_addr)?).to_pixel();

        for x in 0..512 {
            let mut pixel = bg_pixel;

            if !self.dispcnt.force_blank() {
                if self.dispcnt.screen_display_bg0() {
                    let p = self.bgs[0].frame.get_pixel(
                        self.bgs[0].offset.0 as u32 + x,
                        self.bgs[0].offset.1 as u32 + y,
                    );

                    if p.0[3] != 0x00 {
                        pixel = *p;
                    }
                }

                if self.dispcnt.screen_display_bg1() {
                    let p = self.bgs[1].frame.get_pixel(
                        self.bgs[1].offset.0 as u32 + x,
                        self.bgs[1].offset.1 as u32 + y,
                    );

                    if p.0[3] != 0x00 {
                        pixel = *p;
                    }
                }

                if self.dispcnt.screen_display_bg2() {
                    let p = self.effect_bgs[0].bg.frame.get_pixel(
                        self.effect_bgs[0].bg.offset.0 as u32 + x,
                        self.effect_bgs[0].bg.offset.1 as u32 + y,
                    );

                    if p.0[3] != 0x00 {
                        pixel = *p;
                    }
                }

                if self.dispcnt.screen_display_bg3() {
                    let p = self.effect_bgs[1].bg.frame.get_pixel(
                        self.effect_bgs[1].bg.offset.0 as u32 + x,
                        self.effect_bgs[1].bg.offset.1 as u32 + y,
                    );

                    if p.0[3] != 0x00 {
                        pixel = *p;
                    }
                }
            }

            self.current_frame.put_pixel(x, y, pixel);
        }

        Ok(())
    }

    fn dimensional_character_mapping(&self) -> u8 {
        if self.dispcnt.obj_character_vram_mapping() {
            // One Dimensional Character Mapping
            0
        } else {
            // Two Dimensional Character Mapping
            0
        }
    }

    pub fn read_dispcnt(&self) -> Result<u16> {
        Ok(self.dispcnt.0)
    }
    pub fn write_dispcnt(&mut self, val: u16) -> Result<()> {
        self.dispcnt = DispCnt(val);
        trace!("WRITE DISPCNT: {}({:?})", val, self.dispcnt);

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
        trace!("WRITE DISPSTAT: {}({:?})", val, self.dispstat);

        Ok(())
    }

    pub fn read_v_count(&self) -> Result<u16> {
        Ok(self.v_count as u16)
    }

    pub fn read_bg_0_cnt(&self) -> Result<u16> {
        Ok(self.bgs[0].cnt.0)
    }
    pub fn write_bg_0_cnt(&mut self, val: u16) -> Result<()> {
        self.bgs[0].cnt = BgCnt(val);
        trace!("WRITE BG0CNT: {}({:?})", val, self.bgs[0].cnt);

        Ok(())
    }

    pub fn read_bg_1_cnt(&self) -> Result<u16> {
        Ok(self.bgs[1].cnt.0)
    }
    pub fn write_bg_1_cnt(&mut self, val: u16) -> Result<()> {
        self.bgs[1].cnt = BgCnt(val);
        trace!("WRITE BG1CNT: {}({:?})", val, self.bgs[1].cnt);

        Ok(())
    }

    pub fn read_bg_2_cnt(&self) -> Result<u16> {
        Ok(self.effect_bgs[0].bg.cnt.0)
    }
    pub fn write_bg_2_cnt(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[0].bg.cnt = BgCnt(val);
        trace!("WRITE BG2CNT: {}({:?})", val, self.effect_bgs[0].bg.cnt);

        Ok(())
    }

    pub fn read_bg_3_cnt(&self) -> Result<u16> {
        Ok(self.effect_bgs[1].bg.cnt.0)
    }
    pub fn write_bg_3_cnt(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[1].bg.cnt = BgCnt(val);
        trace!("WRITE BG3CNT: {}({:?})", val, self.effect_bgs[1].bg.cnt);

        Ok(())
    }

    pub fn read_bg_0_offset_x(&self) -> Result<u16> {
        Ok(self.bgs[0].offset.0 & 0x1FF)
    }
    pub fn write_bg_0_offset_x(&mut self, val: u16) -> Result<()> {
        self.bgs[0].offset.0 = val & 0x1FF;
        trace!("WRITE BG0 OFFSET X: {}({:?})", val, self.bgs[0].offset.0);

        Ok(())
    }
    pub fn read_bg_0_offset_y(&self) -> Result<u16> {
        Ok(self.bgs[0].offset.1 & 0x1FF)
    }
    pub fn write_bg_0_offset_y(&mut self, val: u16) -> Result<()> {
        self.bgs[0].offset.1 = val & 0x1FF;
        trace!("WRITE BG0 OFFSET Y: {}({:?})", val, self.bgs[0].offset.1);

        Ok(())
    }

    pub fn read_bg_1_offset_x(&self) -> Result<u16> {
        Ok(self.bgs[1].offset.0 & 0x1FF)
    }
    pub fn write_bg_1_offset_x(&mut self, val: u16) -> Result<()> {
        self.bgs[1].offset.0 = val & 0x1FF;
        trace!("WRITE BG1 OFFSET X: {}({:?})", val, self.bgs[1].offset.0);

        Ok(())
    }
    pub fn read_bg_1_offset_y(&self) -> Result<u16> {
        Ok(self.bgs[1].offset.1 & 0x1FF)
    }
    pub fn write_bg_1_offset_y(&mut self, val: u16) -> Result<()> {
        self.bgs[1].offset.1 = val & 0x1FF;
        trace!("WRITE BG1 OFFSET Y: {}({:?})", val, self.bgs[1].offset.1);

        Ok(())
    }

    pub fn read_bg_2_offset_x(&self) -> Result<u16> {
        Ok(self.effect_bgs[0].bg.offset.0 & 0x1FF)
    }
    pub fn write_bg_2_offset_x(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[0].bg.offset.0 = val & 0x1FF;

        Ok(())
    }
    pub fn read_bg_2_offset_y(&self) -> Result<u16> {
        Ok(self.effect_bgs[0].bg.offset.1 & 0x1FF)
    }
    pub fn write_bg_2_offset_y(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[0].bg.offset.1 = val & 0x1FF;

        Ok(())
    }

    pub fn read_bg_3_offset_x(&self) -> Result<u16> {
        Ok(self.effect_bgs[1].bg.offset.0 & 0x1FF)
    }

    pub fn write_bg_3_offset_x(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[1].bg.offset.0 = val & 0x1FF;

        Ok(())
    }
    pub fn read_bg_3_offset_y(&self) -> Result<u16> {
        Ok(self.effect_bgs[1].bg.offset.1 & 0x1FF)
    }
    pub fn write_bg_3_offset_y(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[1].bg.offset.1 = val & 0x1FF;

        Ok(())
    }

    pub fn read_bg_2_x_l(&self) -> Result<u16> {
        Ok(self.effect_bgs[0].x_l)
    }
    pub fn write_bg_2_x_l(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[0].x_l = val;
        Ok(())
    }
    pub fn read_bg_2_x_h(&self) -> Result<u16> {
        Ok(self.effect_bgs[0].x_h)
    }
    pub fn write_bg_2_x_h(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[0].x_h = val;
        Ok(())
    }
    pub fn read_bg_2_y_l(&self) -> Result<u16> {
        Ok(self.effect_bgs[0].y_l)
    }
    pub fn write_bg_2_y_l(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[0].y_l = val;
        Ok(())
    }
    pub fn read_bg_2_y_h(&self) -> Result<u16> {
        Ok(self.effect_bgs[0].y_h)
    }
    pub fn write_bg_2_y_h(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[0].y_h = val;
        Ok(())
    }

    pub fn read_bg_2_p_a(&self) -> Result<u16> {
        Ok(self.effect_bgs[0].pa.0)
    }
    pub fn write_bg_2_p_a(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[0].pa = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_2_p_b(&self) -> Result<u16> {
        Ok(self.effect_bgs[0].pb.0)
    }
    pub fn write_bg_2_p_b(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[0].pb = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_2_p_c(&self) -> Result<u16> {
        Ok(self.effect_bgs[0].pc.0)
    }
    pub fn write_bg_2_p_c(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[0].pc = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_2_p_d(&self) -> Result<u16> {
        Ok(self.effect_bgs[0].pd.0)
    }
    pub fn write_bg_2_p_d(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[0].pd = BgRotationScalingParam(val);
        Ok(())
    }

    pub fn read_bg_3_x_l(&self) -> Result<u16> {
        Ok(self.effect_bgs[1].x_l)
    }
    pub fn write_bg_3_x_l(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[1].x_l = val;
        Ok(())
    }
    pub fn read_bg_3_x_h(&self) -> Result<u16> {
        Ok(self.effect_bgs[1].x_h)
    }
    pub fn write_bg_3_x_h(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[1].x_h = val;
        Ok(())
    }
    pub fn read_bg_3_y_l(&self) -> Result<u16> {
        Ok(self.effect_bgs[1].y_l)
    }
    pub fn write_bg_3_y_l(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[1].y_l = val;
        Ok(())
    }
    pub fn read_bg_3_y_h(&self) -> Result<u16> {
        Ok(self.effect_bgs[1].y_h)
    }
    pub fn write_bg_3_y_h(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[1].y_h = val;
        Ok(())
    }

    pub fn read_bg_3_p_a(&self) -> Result<u16> {
        Ok(self.effect_bgs[1].pa.0)
    }
    pub fn write_bg_3_p_a(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[1].pa = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_3_p_b(&self) -> Result<u16> {
        Ok(self.effect_bgs[1].pb.0)
    }
    pub fn write_bg_3_p_b(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[1].pb = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_3_p_c(&self) -> Result<u16> {
        Ok(self.effect_bgs[1].pc.0)
    }
    pub fn write_bg_3_p_c(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[1].pc = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_3_p_d(&self) -> Result<u16> {
        Ok(self.effect_bgs[1].pd.0)
    }
    pub fn write_bg_3_p_d(&mut self, val: u16) -> Result<()> {
        self.effect_bgs[1].pd = BgRotationScalingParam(val);
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

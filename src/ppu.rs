use anyhow::Result;
use bitfield::bitfield;
use image::{ImageBuffer, Rgba};

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
    bg_mode, _: 2, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct DispStat(u16);
    impl Debug;
    v_count_settings, _: 8, 8;
    v_counter_irq_enable, _: 5;
    h_blank_irq_enable, _: 4;
    v_blank_irq_enable, _: 3;
    v_counter, _: 2;
    h_blank, _: 1;
    v_blank, _: 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct BgCnt(u16);
    impl Debug;
    screen_size, _: 2, 14;
    display_area_overflow, _: 13;
    screen_base_block, _: 5, 8;
    colors_palette, _: 7;
    mosaic, _: 6;
    char_base_block, _: 2, 2;
    bg_priority, _: 2, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct BgReferencePoint(u32);
    impl Debug;
    sign, set_sign: 27;
    int, set_int: 8, 19;
    frac, set_frac: 8, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct BgRotationScalingParam(u16);
    impl Debug;
    sign, set_sign: 15;
    int, set_int: 7, 8;
    frac, set_frac: 8, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct HorizontalDimension(u16);
    impl Debug;
    left_most, set_left_most: 8, 8;
    right_most, set_right_most: 8, 0;
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct VerticalDimension(u16);
    impl Debug;
    top_most, set_top_most: 8, 8;
    bottom_most, set_bottom_most: 8, 0;
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
    obj_mosaic_v_size, _: 4, 12;
    obj_mosaic_h_size, _: 4, 8;
    bg_mosaic_v_size, _: 4, 4;
    bg_mosaic_h_size, _: 4, 0;
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
    color_special_effect, _: 2, 6;
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
    evb_coef, _: 5, 8;
    eva_coef, _: 5, 0;
}

enum Mode {
    Mode0,
    Mode1,
    Mode2,
    Mode3,
}

pub struct Ppu {
    mode: Mode,

    palette: Box<[u8; 0x400]>,
    vram: Box<[u8; 0x1_8000]>,
    sprite: Box<[u8; 0x400]>,

    dispcnt: DispCnt,
    green_swap: bool,
    dispstat: DispStat,
    v_count: u8,

    bg_0_cnt: BgCnt,
    bg_1_cnt: BgCnt,
    bg_2_cnt: BgCnt,
    bg_3_cnt: BgCnt,

    bg_0_offset_x: u16,
    bg_0_offset_y: u16,
    bg_1_offset_x: u16,
    bg_1_offset_y: u16,
    bg_2_offset_x: u16,
    bg_2_offset_y: u16,
    bg_3_offset_x: u16,
    bg_3_offset_y: u16,

    bg_2_x_l: u16,
    bg_2_x_h: u16,
    bg_2_y_l: u16,
    bg_2_y_h: u16,

    bg_2_p_a: BgRotationScalingParam,
    bg_2_p_b: BgRotationScalingParam,
    bg_2_p_c: BgRotationScalingParam,
    bg_2_p_d: BgRotationScalingParam,

    bg_3_x_l: u16,
    bg_3_x_h: u16,
    bg_3_y_l: u16,
    bg_3_y_h: u16,

    bg_3_p_a: BgRotationScalingParam,
    bg_3_p_b: BgRotationScalingParam,
    bg_3_p_c: BgRotationScalingParam,
    bg_3_p_d: BgRotationScalingParam,

    win_0_h: HorizontalDimension,
    win_1_h: HorizontalDimension,
    win_0_v: VerticalDimension,
    win_1_v: VerticalDimension,

    win_in: WinIn,
    win_out: WinOut,

    mosaic: Mosaic,

    bld_cnt: BldCnt,
    bld_alpha: BldAlpha,
    bld_y: u8,

    current_frame: ImageBuffer<Rgba<u8>, Vec<u8>>,
}

impl Ppu {
    pub fn new() -> Self {
        Self {
            mode: Mode::Mode0,
            palette: Box::new([0; 0x400]),
            vram: Box::new([0; 0x1_8000]),
            sprite: Box::new([0; 0x400]),
            dispcnt: Default::default(),
            green_swap: Default::default(),
            dispstat: Default::default(),
            v_count: Default::default(),
            bg_0_cnt: Default::default(),
            bg_1_cnt: Default::default(),
            bg_2_cnt: Default::default(),
            bg_3_cnt: Default::default(),
            current_frame: ImageBuffer::new(240, 160),
            bg_0_offset_x: Default::default(),
            bg_0_offset_y: Default::default(),
            bg_1_offset_x: Default::default(),
            bg_1_offset_y: Default::default(),
            bg_2_offset_x: Default::default(),
            bg_2_offset_y: Default::default(),
            bg_3_offset_x: Default::default(),
            bg_3_offset_y: Default::default(),
            bg_2_x_l: Default::default(),
            bg_2_x_h: Default::default(),
            bg_2_y_l: Default::default(),
            bg_2_y_h: Default::default(),
            bg_2_p_a: Default::default(),
            bg_2_p_b: Default::default(),
            bg_2_p_c: Default::default(),
            bg_2_p_d: Default::default(),
            bg_3_x_l: Default::default(),
            bg_3_x_h: Default::default(),
            bg_3_y_l: Default::default(),
            bg_3_y_h: Default::default(),
            bg_3_p_a: Default::default(),
            bg_3_p_b: Default::default(),
            bg_3_p_c: Default::default(),
            bg_3_p_d: Default::default(),
            win_0_h: Default::default(),
            win_1_h: Default::default(),
            win_0_v: Default::default(),
            win_1_v: Default::default(),
            win_in: Default::default(),
            win_out: Default::default(),
            mosaic: Default::default(),
            bld_cnt: Default::default(),
            bld_alpha: Default::default(),
            bld_y: Default::default(),
        }
    }

    pub fn tick(&mut self) -> Result<()> {
        Ok(())
    }

    pub fn read_vram_8(&self, addr: u32) -> Result<u8> {
        match addr {
            0x0500_0000..=0x0500_3FFF => Ok(self.palette[(addr - 0x0500_0000) as usize]),
            0x0600_0000..=0x0601_7FFF => Ok(self.vram[(addr - 0x0600_0000) as usize]),
            0x0700_0000..=0x0700_3FFF => Ok(self.sprite[(addr - 0x0700_0000) as usize]),
            _ => Ok(0),
        }
    }

    pub fn write_vram_8(&mut self, addr: u32, val: u8) -> Result<()> {
        match addr {
            0x0500_0000..=0x0500_3FFF => {
                self.palette[(addr - 0x0500_0000) as usize] = val;

                Ok(())
            }
            0x0600_0000..=0x0601_7FFF => {
                self.vram[(addr - 0x0600_0000) as usize] = val;

                Ok(())
            }
            0x0700_0000..=0x0700_3FFF => {
                self.sprite[(addr - 0x0700_0000) as usize] = val;

                Ok(())
            }
            _ => Ok(()),
        }
    }

    pub fn read_dispcnt(&self) -> Result<u16> {
        Ok(self.dispcnt.0)
    }
    pub fn write_dispcnt(&mut self, val: u16) -> Result<()> {
        self.dispcnt = DispCnt(val);

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

        Ok(())
    }

    pub fn read_v_count(&self) -> Result<u16> {
        Ok(self.v_count as u16)
    }

    pub fn read_bg_0_cnt(&self) -> Result<u16> {
        Ok(self.bg_0_cnt.0)
    }
    pub fn write_bg_0_cnt(&mut self, val: u16) -> Result<()> {
        self.bg_0_cnt = BgCnt(val);

        Ok(())
    }

    pub fn read_bg_1_cnt(&self) -> Result<u16> {
        Ok(self.bg_1_cnt.0)
    }
    pub fn write_bg_1_cnt(&mut self, val: u16) -> Result<()> {
        self.bg_1_cnt = BgCnt(val);

        Ok(())
    }

    pub fn read_bg_2_cnt(&self) -> Result<u16> {
        Ok(self.bg_2_cnt.0)
    }
    pub fn write_bg_2_cnt(&mut self, val: u16) -> Result<()> {
        self.bg_2_cnt = BgCnt(val);

        Ok(())
    }

    pub fn read_bg_3_cnt(&self) -> Result<u16> {
        Ok(self.bg_3_cnt.0)
    }
    pub fn write_bg_3_cnt(&mut self, val: u16) -> Result<()> {
        self.bg_3_cnt = BgCnt(val);

        Ok(())
    }

    pub fn read_bg_0_offset_x(&self) -> Result<u16> {
        Ok(self.bg_0_offset_x & 0x1FF)
    }
    pub fn write_bg_0_offset_x(&mut self, val: u16) -> Result<()> {
        self.bg_0_offset_x = val & 0x1FF;

        Ok(())
    }
    pub fn read_bg_0_offset_y(&self) -> Result<u16> {
        Ok(self.bg_0_offset_y & 0x1FF)
    }
    pub fn write_bg_0_offset_y(&mut self, val: u16) -> Result<()> {
        self.bg_0_offset_y = val & 0x1FF;

        Ok(())
    }

    pub fn read_bg_1_offset_x(&self) -> Result<u16> {
        Ok(self.bg_1_offset_x & 0x1FF)
    }
    pub fn write_bg_1_offset_x(&mut self, val: u16) -> Result<()> {
        self.bg_1_offset_x = val & 0x1FF;

        Ok(())
    }
    pub fn read_bg_1_offset_y(&self) -> Result<u16> {
        Ok(self.bg_1_offset_y & 0x1FF)
    }
    pub fn write_bg_1_offset_y(&mut self, val: u16) -> Result<()> {
        self.bg_1_offset_y = val & 0x1FF;

        Ok(())
    }

    pub fn read_bg_2_offset_x(&self) -> Result<u16> {
        Ok(self.bg_2_offset_x & 0x1FF)
    }
    pub fn write_bg_2_offset_x(&mut self, val: u16) -> Result<()> {
        self.bg_2_offset_x = val & 0x1FF;

        Ok(())
    }
    pub fn read_bg_2_offset_y(&self) -> Result<u16> {
        Ok(self.bg_2_offset_y & 0x1FF)
    }
    pub fn write_bg_2_offset_y(&mut self, val: u16) -> Result<()> {
        self.bg_2_offset_y = val & 0x1FF;

        Ok(())
    }

    pub fn read_bg_3_offset_x(&self) -> Result<u16> {
        Ok(self.bg_3_offset_x & 0x1FF)
    }
    pub fn write_bg_3_offset_x(&mut self, val: u16) -> Result<()> {
        self.bg_3_offset_x = val & 0x1FF;

        Ok(())
    }
    pub fn read_bg_3_offset_y(&self) -> Result<u16> {
        Ok(self.bg_3_offset_y & 0x1FF)
    }
    pub fn write_bg_3_offset_y(&mut self, val: u16) -> Result<()> {
        self.bg_3_offset_y = val & 0x1FF;

        Ok(())
    }

    pub fn read_bg_2_x_l(&self) -> Result<u16> {
        Ok(self.bg_2_x_l)
    }
    pub fn write_bg_2_x_l(&mut self, val: u16) -> Result<()> {
        self.bg_2_x_l = val;
        Ok(())
    }
    pub fn read_bg_2_x_h(&self) -> Result<u16> {
        Ok(self.bg_2_x_h)
    }
    pub fn write_bg_2_x_h(&mut self, val: u16) -> Result<()> {
        self.bg_2_x_h = val;
        Ok(())
    }
    pub fn read_bg_2_y_l(&self) -> Result<u16> {
        Ok(self.bg_2_y_l)
    }
    pub fn write_bg_2_y_l(&mut self, val: u16) -> Result<()> {
        self.bg_2_y_l = val;
        Ok(())
    }
    pub fn read_bg_2_y_h(&self) -> Result<u16> {
        Ok(self.bg_2_y_h)
    }
    pub fn write_bg_2_y_h(&mut self, val: u16) -> Result<()> {
        self.bg_2_y_h = val;
        Ok(())
    }

    pub fn read_bg_2_p_a(&self) -> Result<u16> {
        Ok(self.bg_2_p_a.0)
    }
    pub fn write_bg_2_p_a(&mut self, val: u16) -> Result<()> {
        self.bg_2_p_a = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_2_p_b(&self) -> Result<u16> {
        Ok(self.bg_2_p_b.0)
    }
    pub fn write_bg_2_p_b(&mut self, val: u16) -> Result<()> {
        self.bg_2_p_b = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_2_p_c(&self) -> Result<u16> {
        Ok(self.bg_2_p_c.0)
    }
    pub fn write_bg_2_p_c(&mut self, val: u16) -> Result<()> {
        self.bg_2_p_c = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_2_p_d(&self) -> Result<u16> {
        Ok(self.bg_2_p_d.0)
    }
    pub fn write_bg_2_p_d(&mut self, val: u16) -> Result<()> {
        self.bg_2_p_d = BgRotationScalingParam(val);
        Ok(())
    }

    pub fn read_bg_3_x_l(&self) -> Result<u16> {
        Ok(self.bg_3_x_l)
    }
    pub fn write_bg_3_x_l(&mut self, val: u16) -> Result<()> {
        self.bg_3_x_l = val;
        Ok(())
    }
    pub fn read_bg_3_x_h(&self) -> Result<u16> {
        Ok(self.bg_3_x_h)
    }
    pub fn write_bg_3_x_h(&mut self, val: u16) -> Result<()> {
        self.bg_3_x_h = val;
        Ok(())
    }
    pub fn read_bg_3_y_l(&self) -> Result<u16> {
        Ok(self.bg_3_y_l)
    }
    pub fn write_bg_3_y_l(&mut self, val: u16) -> Result<()> {
        self.bg_3_y_l = val;
        Ok(())
    }
    pub fn read_bg_3_y_h(&self) -> Result<u16> {
        Ok(self.bg_3_y_h)
    }
    pub fn write_bg_3_y_h(&mut self, val: u16) -> Result<()> {
        self.bg_3_y_h = val;
        Ok(())
    }

    pub fn read_bg_3_p_a(&self) -> Result<u16> {
        Ok(self.bg_3_p_a.0)
    }
    pub fn write_bg_3_p_a(&mut self, val: u16) -> Result<()> {
        self.bg_3_p_a = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_3_p_b(&self) -> Result<u16> {
        Ok(self.bg_3_p_b.0)
    }
    pub fn write_bg_3_p_b(&mut self, val: u16) -> Result<()> {
        self.bg_3_p_b = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_3_p_c(&self) -> Result<u16> {
        Ok(self.bg_3_p_c.0)
    }
    pub fn write_bg_3_p_c(&mut self, val: u16) -> Result<()> {
        self.bg_3_p_c = BgRotationScalingParam(val);
        Ok(())
    }
    pub fn read_bg_3_p_d(&self) -> Result<u16> {
        Ok(self.bg_3_p_d.0)
    }
    pub fn write_bg_3_p_d(&mut self, val: u16) -> Result<()> {
        self.bg_3_p_d = BgRotationScalingParam(val);
        Ok(())
    }

    pub fn read_win_0_h(&self) -> Result<u16> {
        Ok(self.win_0_h.0)
    }
    pub fn write_win_0_h(&mut self, val: u16) -> Result<()> {
        self.win_0_h = HorizontalDimension(val);
        Ok(())
    }
    pub fn read_win_1_h(&self) -> Result<u16> {
        Ok(self.win_1_h.0)
    }
    pub fn write_win_1_h(&mut self, val: u16) -> Result<()> {
        self.win_1_h = HorizontalDimension(val);
        Ok(())
    }
    pub fn read_win_0_v(&self) -> Result<u16> {
        Ok(self.win_0_v.0)
    }
    pub fn write_win_0_v(&mut self, val: u16) -> Result<()> {
        self.win_0_v = VerticalDimension(val);
        Ok(())
    }
    pub fn read_win_1_v(&self) -> Result<u16> {
        Ok(self.win_1_v.0)
    }
    pub fn write_win_1_v(&mut self, val: u16) -> Result<()> {
        self.win_1_v = VerticalDimension(val);
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

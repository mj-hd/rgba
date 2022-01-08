use anyhow::Result;
use bitfield::bitfield;
use image::Rgba;
use log::trace;

use super::{
    palette::{Color, Palette},
    ppu::{BgCnt, BgRotationScalingParam, DispCnt, Vram},
    tile::{Tile, TileContext},
    PixelFormat, Surface,
};

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct TextBgScreen(u16);
    impl Debug;
    palette_number, _: 15, 12;
    v_flip, _: 11;
    h_flip, _: 10;
    tile_number, _: 9, 0;
}

impl TextBgScreen {
    fn to_tile(&self, pixel_format: PixelFormat, char_base_block: u16) -> Tile {
        let palette_offset = match pixel_format {
            PixelFormat::Bpp4 => self.palette_number() * 2 * 16,
            PixelFormat::Bpp8 => 0,
        };

        Tile::new(
            TileContext {
                pixel_format,
                base_addr: 0x0600_0000 + char_base_block as u32 * 16 * 1024,
                palette: Palette::new_bg(palette_offset),
            },
            self.tile_number(),
        )
    }
}

pub struct BgScreen {
    pub frame: Box<Surface>,
    pub cnt: BgCnt,
    pub offset: (u16, u16),
}

impl Default for BgScreen {
    fn default() -> Self {
        Self {
            frame: Box::new(Surface::new(512, 512)),
            cnt: BgCnt(0),
            offset: (0, 0),
        }
    }
}

impl BgScreen {
    pub fn render_tile_screen(&mut self, vram: &mut Vram, pos: (u32, u32)) -> Result<()> {
        let (x, y) = pos;

        let map_index = x / 8 + (y / 8) * 32;
        let map_base_addr = 0x0600_0000 + self.cnt.screen_base_block() as u32 * 2 * 1024;
        let map = TextBgScreen(vram.read_vram_16(map_base_addr + map_index * 2)?);

        let tile = map.to_tile(self.cnt.pixel_format(), self.cnt.char_base_block());

        let surface = tile.rasterize(vram)?;

        self.frame.draw(&surface, (pos.0, (pos.1 / 8) * 8));

        Ok(())
    }
}

#[derive(Default)]
pub struct EffectBgScreen {
    pub bg: BgScreen,

    pub x_l: u16,
    pub x_h: u16,
    pub y_l: u16,
    pub y_h: u16,

    pub pa: BgRotationScalingParam,
    pub pb: BgRotationScalingParam,
    pub pc: BgRotationScalingParam,
    pub pd: BgRotationScalingParam,
}

impl EffectBgScreen {
    pub fn render_bitmap_screen_full(
        &mut self,
        vram: &mut Vram,
        dispcnt: DispCnt,
        pos: (u32, u32),
        size: (u32, u32),
    ) -> Result<()> {
        // Bitmap Mode size(240x160, 160x128) pixels, 32768 colors

        let (x, y) = pos;

        let index = y * size.0 * 2 + x * 2;

        let base_addr = 0x0600_0000
            + if dispcnt.display_frame_select() {
                0xA000
            } else {
                0x0000
            };

        let color = Color::new(vram.read_vram_16(base_addr + index)?);

        self.bg.frame.put_pixel(x, y, color.to_pixel());

        Ok(())
    }

    pub fn render_bitmap_screen_256(
        &mut self,
        dispcnt: DispCnt,
        vram: &mut Vram,
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

        let palette = Palette::new_bg(0);
        let color = palette.get_color(vram, color_index)?;

        self.bg.frame.put_pixel(x, y, color.to_pixel());

        Ok(())
    }
}

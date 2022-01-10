use anyhow::Result;
use bitfield::bitfield;

use super::palette::Color;
use super::palette::Palette;
use super::ppu::BgMode;
use super::ppu::DispCnt;
use super::ppu::Vram;
use super::tile::Tile;
use super::tile::TileContext;
use super::PixelFormat;
use super::Surface;

pub struct Oam {
    oam0: Oam0,
    oam1: Oam1,
    oam2: Oam2,
}

impl Oam {
    fn from_vram(vram: &mut Vram, index: u8) -> Result<Option<Self>> {
        let base_addr = 0x0700_0000u32;
        let offset = (index as u32) * 8;

        let oam0_addr = base_addr + offset;
        let oam0 = Oam0(vram.read_vram_16(oam0_addr)?);

        if oam0.disabled() {
            return Ok(None);
        }

        let oam1_addr = base_addr + offset + 2;
        let oam1 = Oam1(vram.read_vram_16(oam1_addr)?);
        let oam2_addr = base_addr + offset + 4;
        let oam2 = Oam2(vram.read_vram_16(oam2_addr)?);

        Ok(Some(Self { oam0, oam1, oam2 }))
    }

    fn size(&self) -> (u32, u32) {
        let size = match self.oam1.obj_size() {
            0 => (8, 8),
            1 => (16, 16),
            2 => (32, 32),
            3 => (64, 64),
            _ => panic!("unexpected size"),
        };

        match self.oam0.shape() {
            0 => size,
            1 => (size.0, size.1 / 2),
            2 => (size.0 / 2, size.1),
            _ => panic!("unexpected shape"),
        }
    }

    fn pos(&self) -> (u32, u32) {
        (self.oam1.x() as u32, self.oam0.y() as u32)
    }

    fn to_tile_context(&self) -> TileContext {
        let palette_offset = match self.oam0.pixel_format() {
            PixelFormat::Bpp4 => self.oam2.palette_number() * 2 * 16,
            PixelFormat::Bpp8 => 0,
        } as u16;

        TileContext {
            pixel_format: self.oam0.pixel_format(),
            base_addr: 0x0601_0000,
            palette: Palette::new_oam(palette_offset),
        }
    }

    fn base_tile_number(&self, bg_mode: BgMode) -> u16 {
        self.oam2.character_name()
            / match self.oam0.pixel_format() {
                PixelFormat::Bpp4 => 1,
                PixelFormat::Bpp8 => 2,
            }
            / match bg_mode {
                BgMode::Mode3 | BgMode::Mode4 | BgMode::Mode5 => 2,
                _ => 1,
            }
    }

    fn doubled(&self) -> bool {
        self.oam0.doubled()
    }
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct Oam0(u16);
    impl Debug;
    shape, _: 15, 14;
    _colors_palettes, _: 13;
    obj_mosaic, _: 12;
    obj_mode, _: 11, 10;
    obj_disable_double_size, _: 9;
    rotation_scaling, _: 8;
    y, _: 7, 0;
}

impl Oam0 {
    fn pixel_format(&self) -> PixelFormat {
        match self._colors_palettes() {
            false => PixelFormat::Bpp4,
            true => PixelFormat::Bpp8,
        }
    }

    fn doubled(&self) -> bool {
        self.rotation_scaling() && self.obj_disable_double_size()
    }

    fn disabled(&self) -> bool {
        !self.rotation_scaling() && self.obj_disable_double_size()
    }
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

pub struct SpriteScreen {
    pub frame: Box<Surface>,
}

impl Default for SpriteScreen {
    fn default() -> Self {
        Self {
            frame: Box::new(Surface::new(512, 512)),
        }
    }
}

impl SpriteScreen {
    pub fn clear(&mut self, y: u32) {
        if y < self.frame.height() {
            for x in 0..(self.frame.width()) {
                self.frame.put_pixel(x, y, Color::Transparent.to_pixel())
            }
        }
    }

    pub fn render_sprites(&mut self, vram: &mut Vram, dispcnt: DispCnt) -> Result<()> {
        for i in 0..128 {
            let oam = match Oam::from_vram(vram, i)? {
                None => continue,
                Some(oam) => oam,
            };

            let doubled = oam.doubled();
            let size = oam.size();
            let oam_pos = oam.pos();

            let tile_ctx = oam.to_tile_context();
            let tile_base_number = oam.base_tile_number(dispcnt.bg_mode());
            let tiles = self.dimensional_tiles(
                tile_ctx,
                dispcnt.obj_character_vram_mapping(),
                tile_base_number,
                size,
            );

            for (offset, tile) in tiles.into_iter() {
                let surface = tile.rasterize(vram)?;
                self.frame
                    .draw(&surface, (oam_pos.0 + offset.0, oam_pos.1 + offset.1))?;
            }
        }

        Ok(())
    }

    fn dimensional_tiles(
        &self,
        ctx: TileContext,
        one_dimensional: bool,
        base_number: u16,
        size: (u32, u32),
    ) -> Vec<((u32, u32), Tile)> {
        let mut result = vec![];
        let width = size.0 / 8;
        let height = size.1 / 8;

        let wrap_width = if one_dimensional {
            width
        } else {
            match ctx.pixel_format {
                PixelFormat::Bpp4 => 32,
                PixelFormat::Bpp8 => 16,
            }
        };

        for x in 0..width {
            for y in 0..height {
                let offset = (x + y * wrap_width) as u16;
                let tile = Tile::new(ctx, base_number as u16 + offset);

                result.push(((x * 8, y * 8), tile));
            }
        }

        result
    }
}

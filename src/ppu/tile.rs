use anyhow::Result;

use super::{palette::Palette, ppu::Vram, PixelFormat, Surface};

pub struct Tile {
    ctx: TileContext,
    index: u16,
}

#[derive(Clone, Copy)]
pub struct TileContext {
    pub pixel_format: PixelFormat,
    pub base_addr: u32,
    pub palette: Palette,
}

impl Tile {
    pub fn new(ctx: TileContext, index: u16) -> Self {
        Self { ctx, index }
    }

    pub fn rasterize(&self, vram: &mut Vram) -> Result<Surface> {
        // dots: 8x8
        // color depth: 4bpp, 8bpp
        //   4bpp: 16 colors, 16 palettes
        //   8bpp: 256 colors, 1 palette

        match self.ctx.pixel_format {
            PixelFormat::Bpp4 => self.rasterize_4bpp(vram),
            PixelFormat::Bpp8 => self.rasterize_8bpp(vram),
        }
    }

    #[inline]
    fn rasterize_4bpp(&self, vram: &mut Vram) -> Result<Surface> {
        let bytes_per_row = 4;
        let bytes_per_tile = bytes_per_row * 8;

        let tile_addr = self.ctx.base_addr + (self.index as u32 * bytes_per_tile);

        let mut result = Surface::new(8, 8);

        for y in 0..8 {
            let addr = tile_addr + y * bytes_per_row;

            let mut row = vram.read_vram_32(addr)?;

            for x in 0..8 {
                let color_offset = (row & 0xF) as u8;
                let color = self.ctx.palette.get_color(vram, color_offset)?;

                result.put_pixel(x, y, color.to_pixel());

                row >>= 4;
            }
        }

        Ok(result)
    }

    fn rasterize_8bpp(&self, vram: &mut Vram) -> Result<Surface> {
        let bytes_per_row = 8;
        let bytes_per_tile = bytes_per_row * 8;

        let tile_addr = self.ctx.base_addr + (self.index as u32 * bytes_per_tile);

        let mut result = Surface::new(8, 8);

        for y in 0..8 {
            for x in 0..8 {
                let addr = tile_addr + x + bytes_per_row * y;
                let color_offset = vram.read_vram_8(addr)?;
                let color = self.ctx.palette.get_color(vram, color_offset)?;

                result.put_pixel(x, y, color.to_pixel());
            }
        }

        Ok(result)
    }
}

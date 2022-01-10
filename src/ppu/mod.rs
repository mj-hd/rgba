use anyhow::Result;
use image::{GenericImage, GenericImageView, ImageBuffer, Rgba};

type Pixel = Rgba<u8>;
pub struct Surface(ImageBuffer<Pixel, Vec<u8>>);

impl Surface {
    pub fn new(width: u32, height: u32) -> Self {
        Surface(ImageBuffer::new(width, height))
    }

    pub fn put_pixel(&mut self, x: u32, y: u32, pixel: Pixel) {
        unsafe {
            self.0.unsafe_put_pixel(x, y, pixel);
        }
    }

    pub fn get_pixel(&mut self, x: u32, y: u32) -> Pixel {
        unsafe { self.0.unsafe_get_pixel(x, y) }
    }

    pub fn draw(&mut self, target: &Surface, pos: (u32, u32)) -> Result<()> {
        self.0.copy_from(&target.0, pos.0, pos.1)?;

        Ok(())
    }

    pub fn width(&self) -> u32 {
        self.0.width()
    }

    pub fn height(&self) -> u32 {
        self.0.height()
    }
}

#[derive(Clone, Copy)]
pub enum PixelFormat {
    Bpp4,
    Bpp8,
}

mod oam;

mod bg;
mod palette;
pub mod ppu;
mod tile;

use image::{ImageBuffer, Rgba};

type Pixel = Rgba<u8>;
pub struct Surface(ImageBuffer<Pixel, Vec<u8>>);

impl Surface {
    pub fn new(width: u32, height: u32) -> Self {
        Surface(ImageBuffer::new(width, height))
    }

    pub fn put_pixel(&mut self, x: u32, y: u32, pixel: Pixel) {
        self.0.put_pixel(x, y, pixel);
    }

    pub fn get_pixel(&mut self, x: u32, y: u32) -> &Pixel {
        self.0.get_pixel(x, y)
    }

    pub fn draw(&mut self, target: &Surface, pos: (u32, u32)) {
        for (x, y, pixel) in target.0.enumerate_pixels() {
            self.0.put_pixel(pos.0 + x, pos.1 + y, *pixel);
        }
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

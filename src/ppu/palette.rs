use anyhow::Result;
use bitfield::bitfield;
use image::Rgba;

use super::ppu::Vram;

pub enum Color {
    Transparent,
    Opaque(ColorData),
}

impl Color {
    pub fn new(val: u16) -> Self {
        Self::Opaque(ColorData(val))
    }
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct ColorData(u16);
    impl Debug;
    u8, blue, _: 14, 10;
    u8, green, _: 9, 5;
    u8, red, _: 4, 0;
}

impl Color {
    pub fn to_pixel(&self) -> Rgba<u8> {
        match self {
            Color::Transparent => Rgba([0, 0, 0, 0]),
            Color::Opaque(data) => Rgba([data.red() * 8, data.green() * 8, data.blue() * 8, 0xFF]),
        }
    }
}

#[derive(Clone, Copy)]
pub struct Palette {
    base_addr: u32,
}

impl Palette {
    pub fn new_bg(offset: u16) -> Self {
        Self {
            base_addr: 0x0500_0000 + offset as u32,
        }
    }

    pub fn new_oam(offset: u16) -> Self {
        Self {
            base_addr: 0x0500_0200 + offset as u32,
        }
    }

    pub fn get_color(&self, vram: &Vram, index: u8) -> Result<Color> {
        if index == 0 {
            Ok(Color::Transparent)
        } else {
            let data = ColorData(vram.read_vram_16(self.base_addr + index as u32 * 2)?);
            Ok(Color::Opaque(data))
        }
    }

    pub fn get_bg_color(&self, vram: &Vram) -> Result<Color> {
        let data = ColorData(vram.read_vram_16(self.base_addr)?);
        Ok(Color::Opaque(data))
    }
}

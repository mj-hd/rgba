use anyhow::Result;

use crate::{bus::Bus, cpu::cpu_gba::Cpu, keypad::KeyType, ppu::ppu::Ppu, rom::Rom};

pub struct Gba {
    pub cpu: Cpu,
}

impl Gba {
    pub fn new(rom: Box<Rom>) -> Self {
        let ppu = Ppu::new();
        let bus = Bus::new(rom, ppu);
        let cpu = Cpu::new(bus);
        Gba { cpu }
    }

    pub fn reset(&mut self, skip_bios: bool) -> Result<()> {
        if skip_bios {
            self.cpu.reset_skip_bios()?;
        } else {
            self.cpu.reset()?;
        }

        Ok(())
    }

    pub fn tick(&mut self) -> Result<()> {
        self.cpu.tick()?;

        Ok(())
    }

    pub fn key_press(&mut self, key: KeyType) {
        self.cpu.bus.keypad.press(key);
    }

    pub fn key_release(&mut self, key: KeyType) {
        self.cpu.bus.keypad.release(key);
    }

    pub fn render(&mut self) -> Result<Vec<u8>> {
        Ok(self.cpu.bus.ppu.render())
    }
}

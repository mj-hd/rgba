use anyhow::Result;

use crate::{bus::Bus, cpu_gba::Cpu, ppu::Ppu, rom::Rom};

pub struct Gba {
    pub cpu: Cpu,
}

impl Gba {
    pub fn new(rom: Rom) -> Self {
        let ppu = Ppu::new();
        let bus = Bus::new(rom, ppu);
        let cpu = Cpu::new(bus);
        Gba { cpu }
    }

    pub fn reset(&mut self) -> Result<()> {
        self.cpu.reset()?;

        Ok(())
    }

    pub fn tick(&mut self) -> Result<()> {
        self.cpu.tick()?;

        Ok(())
    }

    pub fn render(&mut self) -> Result<Vec<u8>> {
        Ok(self.cpu.bus.ppu.render())
    }
}

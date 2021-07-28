use anyhow::Result;

use crate::rom::Rom;

pub struct Bus {
    wram: [u8; 0x4_0000],
    rom: Rom,
}

impl Bus {
    pub fn new(rom: Rom) -> Self {
        Self {
            wram: [0; 0x4_0000],
            rom,
        }
    }

    pub fn tick(&mut self) -> Result<()> {
        // TODO
        Ok(())
    }

    pub fn read_8(&self, addr: u32) -> Result<u8> {
        match addr {
            0x0200_0000..=0x0203_FFFF => Ok(self.wram[(addr - 0x0200_0000) as usize]),
            0x0800_0000..=0x0FFF_FFFF => self.rom.read_8(addr),
            _ => Ok(0),
        }
    }

    pub fn read_16(&self, addr: u32) -> Result<u16> {
        let low = self.read_8(addr)?;
        let high = self.read_8(addr + 1)?;

        Ok((high as u16) << 8 | low as u16)
    }

    pub fn read_32(&self, addr: u32) -> Result<u32> {
        let lowest = self.read_8(addr)?;
        let lower = self.read_8(addr + 1)?;
        let higher = self.read_8(addr + 2)?;
        let highest = self.read_8(addr + 3)?;

        Ok((highest as u32) << 24 | (higher as u32) << 16 | (lower as u32) << 8 | lowest as u32)
    }

    pub fn write_8(&mut self, addr: u32, val: u8) -> Result<()> {
        match addr {
            0x0200_0000..=0x0203_FFFF => {
                self.wram[(addr - 0x0200_0000) as usize] = val;

                Ok(())
            }
            0x0800_0000..=0x0FFF_FFFF => self.rom.write_8(addr, val),
            _ => Ok(()),
        }
    }

    pub fn write_16(&mut self, addr: u32, val: u16) -> Result<()> {
        self.write_8(addr, val as u8)?;
        self.write_8(addr, (val >> 8) as u8)?;

        Ok(())
    }

    pub fn write_32(&mut self, addr: u32, val: u32) -> Result<()> {
        self.write_8(addr, val as u8)?;
        self.write_8(addr, (val >> 8) as u8)?;
        self.write_8(addr, (val >> 16) as u8)?;
        self.write_8(addr, (val >> 24) as u8)?;

        Ok(())
    }
}

use core::fmt;
use std::{
    fmt::{Debug, Formatter},
    fs::File,
    io::{BufReader, Read},
};

use anyhow::{bail, Result};
use log::debug;

pub struct Rom {
    data: Vec<u8>,
    sram: [u8; 0x0001_0000],
}

impl Default for Rom {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            sram: [0; 0x0001_0000],
        }
    }
}

impl Debug for Rom {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Rom")
            .field("data", &self.data.len())
            .finish()
    }
}

impl Rom {
    pub fn new(reader: &mut BufReader<File>) -> Result<Rom> {
        let mut rom = Rom::default();

        reader.read_to_end(&mut rom.data)?;

        rom.data.resize(0x0200_0000, 0);

        if !rom.is_valid() {
            bail!("failed to validate checksum");
        }

        Ok(rom)
    }

    pub fn read_8(&self, addr: u32) -> Result<u8> {
        match addr {
            0x0800_0000..=0x09FF_FFFF => {
                Ok(unsafe { *self.data.get_unchecked((addr - 0x0800_0000) as usize) })
            }
            0x0A00_0000..=0x0BFF_FFFF => {
                Ok(unsafe { *self.data.get_unchecked((addr - 0x0A00_0000) as usize) })
            }
            0x0C00_0000..=0x0DFF_FFFF => {
                Ok(unsafe { *self.data.get_unchecked((addr - 0x0C00_0000) as usize) })
            }
            0x0E00_0000..=0x0E00_FFFF => {
                Ok(unsafe { *self.sram.get_unchecked((addr - 0x0E00_0000) as usize) })
            }
            _ => Ok(0),
        }
    }

    pub fn write_8(&mut self, addr: u32, val: u8) -> Result<()> {
        match addr {
            0x0E00_0000..=0x0E00_FFFF => {
                self.sram[(addr - 0x0E00_0000) as usize] = val;
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn is_valid(&self) -> bool {
        let checksum = self.data[0x0BD];

        let actual = self.data[0x0A0..=0x0BC]
            .iter()
            .fold(0, |acc: u8, x| acc.wrapping_sub(*x))
            .wrapping_sub(0x19);

        debug!("checksum: {:#02X} = {:#02X}", actual, checksum);

        checksum == actual
    }
}

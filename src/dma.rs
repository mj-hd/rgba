use anyhow::{bail, Result};
use bitfield::bitfield;
use log::trace;

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct Control(u16);
    impl Debug;
    dma_enable, set_dma_enable: 15;
    irq_upon_end_of_word_count, _: 14;
    dma_start_timing, _: 13, 12;
    game_pak_drqa, _: 11;
    dma_transfer_type, _: 10;
    dma_repeat, _: 9;
    source_addr_control, _: 8, 7;
    dest_addr_control, _: 6, 5;
}

pub enum DmasResult {
    Nop,
    Transfer(usize, u32, u32),
    Irq(usize),
}

#[derive(PartialEq, Eq)]
pub enum _DmaResult {
    Nop,
    Transfer(usize, u32, u32),
    Irq,
}

#[derive(Clone, Copy)]
pub struct Dma {
    active: bool,
    large: bool,

    control: Control,

    reload_src_addr: u32,
    reload_dst_addr: u32,
    reload_count: u16,
    current_src_addr: u32,
    current_dst_addr: u32,
    count: u16,
}

impl Dma {
    fn new(large: bool) -> Self {
        Dma {
            active: false,
            large,
            control: Default::default(),
            reload_src_addr: 0,
            reload_dst_addr: 0,
            reload_count: 0,
            current_src_addr: 0,
            current_dst_addr: 0,
            count: 0,
        }
    }

    pub fn tick(&mut self, v_blank: bool, h_blank: bool, special: bool) -> Result<_DmaResult> {
        if !self.active && self.control.dma_enable() {
            match self.control.dma_start_timing() {
                // Immediately
                0 => self.start(),
                // VBlank
                1 => {
                    if v_blank {
                        trace!("DMA V-BLANK STARTED");
                        self.start();
                    }
                }
                // HBlank
                2 => {
                    if h_blank {
                        trace!("DMA H-BLANK STARTED");
                        self.start();
                    }
                }
                // Special
                3 => {
                    if special {
                        trace!("DMA SPECIAL STARTED");
                        self.start();
                    }
                }
                _ => bail!("unexpected dma start timing"),
            };
        }

        if !self.active {
            return Ok(_DmaResult::Nop);
        }

        let (count, c) = self.count.overflowing_sub(1);
        self.count = count;

        if c {
            trace!("DMA STOPPED");

            if self.control.dma_repeat() {
                self.reload();

                trace!("DMA RELOADED");
            } else {
                self.control.set_dma_enable(false);
            }

            self.active = false;

            return Ok(_DmaResult::Irq);
        }

        let size = match self.control.dma_transfer_type() {
            // 16bit
            false => 2,
            // 32bit
            true => 4,
        };

        let result =
            _DmaResult::Transfer(size as usize, self.current_src_addr, self.current_dst_addr);

        trace!(
            "DMA TRANSFER: ({:08X})=>({:08X})",
            self.current_src_addr,
            self.current_dst_addr
        );

        self.current_src_addr = match self.control.source_addr_control() {
            // Increment
            0 => self.current_src_addr.wrapping_add(size),
            // Decrement
            1 => self.current_src_addr.wrapping_sub(size),
            // Fixed
            2 => self.current_src_addr,
            _ => bail!("unexpected source addr control"),
        };

        self.current_dst_addr = match self.control.dest_addr_control() {
            // Increment, Increment/Reload
            0 | 3 => self.current_dst_addr.wrapping_add(size),
            // Decrement
            1 => self.current_dst_addr.wrapping_sub(size),
            // Fixed
            2 => self.current_dst_addr,
            _ => bail!("unexpected source addr control"),
        };

        Ok(result)
    }

    fn reload(&mut self) {
        self.count = if self.reload_count == 0 {
            if self.large {
                0xFFFF
            } else {
                0x3FFF
            }
        } else {
            self.reload_count
        };

        // Increment/Reload
        if self.control.dest_addr_control() == 3 {
            self.current_dst_addr = self.reload_dst_addr;
        }
    }

    fn start(&mut self) {
        self.active = true;
        self.reload();
    }

    pub fn read_control(&self) -> Result<u16> {
        Ok(self.control.0)
    }

    pub fn write_control(&mut self, val: u16) -> Result<()> {
        let orig = self.control;
        self.control = Control(val);

        trace!("DMA CONTROL: {:?}", self.control);

        if !orig.dma_enable() && self.control.dma_enable() {
            self.current_src_addr = self.reload_src_addr;
            self.current_dst_addr = self.reload_dst_addr;
            self.reload();
            trace!("DMA ENABLED");
        }

        Ok(())
    }

    pub fn write_start_addr_hi(&mut self, val: u16) -> Result<()> {
        self.reload_src_addr &= 0x0000_FFFF;
        self.reload_src_addr |= (val as u32) << 16;

        trace!("DMA SRC ADDR HI: {:04X}", val);

        Ok(())
    }

    pub fn write_start_addr_lo(&mut self, val: u16) -> Result<()> {
        self.reload_src_addr &= 0xFFFF_0000;
        self.reload_src_addr |= val as u32;

        trace!("DMA SRC ADDR LO: {:04X}", val);

        Ok(())
    }

    pub fn write_dest_addr_hi(&mut self, val: u16) -> Result<()> {
        self.reload_dst_addr &= 0x0000_FFFF;
        self.reload_dst_addr |= (val as u32) << 16;

        trace!("DMA DEST ADDR HI: {:04X}", val);

        Ok(())
    }

    pub fn write_dest_addr_lo(&mut self, val: u16) -> Result<()> {
        self.reload_dst_addr &= 0xFFFF_0000;
        self.reload_dst_addr |= val as u32;

        trace!("DMA DEST ADDR LO: {:04X}", val);

        Ok(())
    }

    pub fn write_count(&mut self, val: u16) -> Result<()> {
        self.reload_count = val;

        trace!("DMA COUNT: {}", val);

        Ok(())
    }
}

pub struct Dmas {
    dmas: [Dma; 4],
}

impl Dmas {
    pub fn new() -> Self {
        Dmas {
            dmas: [
                Dma::new(false),
                Dma::new(false),
                Dma::new(false),
                Dma::new(true),
            ],
        }
    }

    pub fn tick(
        &mut self,
        v_blank: bool,
        h_blank: bool,
        sound_fifo: bool,
        video_capture: bool,
    ) -> Result<DmasResult> {
        for i in 0..4 {
            let dma = &mut self.dmas[i];

            let result = match i {
                0 => dma.tick(v_blank, h_blank, false)?,
                1 | 2 => dma.tick(v_blank, h_blank, sound_fifo)?,
                3 => dma.tick(v_blank, h_blank, video_capture)?,
                _ => bail!("dmas out of range"),
            };

            if result != _DmaResult::Nop {
                return Ok(match result {
                    _DmaResult::Transfer(size, src, dst) => DmasResult::Transfer(size, src, dst),
                    _DmaResult::Irq => DmasResult::Irq(i),
                    _DmaResult::Nop => DmasResult::Nop,
                });
            }
        }

        Ok(DmasResult::Nop)
    }

    pub fn write_16(&mut self, addr: u32, val: u16) -> Result<()> {
        match addr {
            0x0400_00B0 => self.dmas[0].write_start_addr_lo(val),
            0x0400_00B2 => self.dmas[0].write_start_addr_hi(val),
            0x0400_00B4 => self.dmas[0].write_dest_addr_lo(val),
            0x0400_00B6 => self.dmas[0].write_dest_addr_hi(val),
            0x0400_00B8 => self.dmas[0].write_count(val),
            0x0400_00BA => self.dmas[0].write_control(val),
            0x0400_00BC => self.dmas[1].write_start_addr_lo(val),
            0x0400_00BE => self.dmas[1].write_start_addr_hi(val),
            0x0400_00C0 => self.dmas[1].write_dest_addr_lo(val),
            0x0400_00C2 => self.dmas[1].write_dest_addr_hi(val),
            0x0400_00C4 => self.dmas[1].write_count(val),
            0x0400_00C6 => self.dmas[1].write_control(val),
            0x0400_00C8 => self.dmas[2].write_start_addr_lo(val),
            0x0400_00CA => self.dmas[2].write_start_addr_hi(val),
            0x0400_00CC => self.dmas[2].write_dest_addr_lo(val),
            0x0400_00CE => self.dmas[2].write_dest_addr_hi(val),
            0x0400_00D0 => self.dmas[2].write_count(val),
            0x0400_00D2 => self.dmas[2].write_control(val),
            0x0400_00D4 => self.dmas[3].write_start_addr_lo(val),
            0x0400_00D6 => self.dmas[3].write_start_addr_hi(val),
            0x0400_00D8 => self.dmas[3].write_dest_addr_lo(val),
            0x0400_00DA => self.dmas[3].write_dest_addr_hi(val),
            0x0400_00DC => self.dmas[3].write_count(val),
            0x0400_00DE => self.dmas[3].write_control(val),
            _ => bail!("unexpected dmas write"),
        }
    }

    #[inline]
    pub fn read_16(&self, addr: u32) -> Result<u16> {
        match addr {
            0x0400_00BA => self.dmas[0].read_control(),
            0x0400_00C6 => self.dmas[1].read_control(),
            0x0400_00D2 => self.dmas[2].read_control(),
            0x0400_00DE => self.dmas[3].read_control(),
            0x0400_00B0..=0x0400_00DF => Ok(0),
            _ => bail!("unexpected dmas read"),
        }
    }

    pub fn active(&self) -> bool {
        self.dmas.iter().any(|dma| dma.active)
    }
}

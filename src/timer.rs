use anyhow::{bail, Result};
use bitfield::bitfield;

bitfield! {
    #[derive(Default, Clone, Copy)]
    struct Control(u16);
    impl Debug;
    start_stop, _: 7;
    ie, _: 6;
    cascade, _: 2;
    prescaler, _: 1, 0;
}

#[derive(Clone, Copy)]
pub struct Timer {
    control: Control,
    counter: u32,
    value: u16,
    reload_value: u16,

    pub irq: bool,
}

impl Timer {
    pub fn new() -> Self {
        Timer {
            control: Default::default(),
            counter: 0,
            value: 0,
            reload_value: 0,
            irq: false,
        }
    }

    pub fn read_value(&self) -> Result<u16> {
        Ok(self.value)
    }

    pub fn write_value(&mut self, val: u16) -> Result<()> {
        self.reload_value = val;

        Ok(())
    }

    pub fn read_control(&self) -> Result<u16> {
        Ok(self.control.0)
    }

    pub fn write_control(&mut self, val: u16) -> Result<()> {
        let orig = self.control;

        self.control = Control(val);

        if !orig.start_stop() && self.control.start_stop() {
            self.value = self.reload_value;
        }

        Ok(())
    }
}

pub struct Timers {
    timers: [Timer; 4],
}

impl Timers {
    pub fn new() -> Self {
        Timers {
            timers: [Timer::new(); 4],
        }
    }

    pub fn tick(&mut self) -> Result<()> {
        let mut overflowed = false;

        for i in 0..4 {
            let timer = &mut self.timers[i];

            if !timer.control.start_stop() {
                continue;
            }

            timer.counter += 1;

            let mut should_increment = false;

            if i != 0 && timer.control.cascade() {
                should_increment = overflowed;
            } else {
                let prescaler = match timer.control.prescaler() {
                    0b00 => 1,
                    0b01 => 64,
                    0b10 => 256,
                    0b11 => 1024,
                    _ => bail!("unexpected prescaler"),
                };

                if timer.counter % prescaler == 0 {
                    timer.counter = 0;
                    should_increment = true;
                }
            }

            if should_increment {
                let (value, c) = timer.value.overflowing_add(1);
                timer.value = value;

                if c {
                    if timer.control.ie() {
                        timer.irq = true;
                    }

                    timer.value = timer.reload_value;
                }

                overflowed = c;
            }
        }

        Ok(())
    }

    #[inline]
    pub fn read_16(&self, addr: u32) -> Result<u16> {
        match addr {
            0x0400_0100 => self.timers[0].read_value(),
            0x0400_0102 => self.timers[0].read_control(),
            0x0400_0104 => self.timers[1].read_value(),
            0x0400_0106 => self.timers[1].read_control(),
            0x0400_0108 => self.timers[2].read_value(),
            0x0400_010A => self.timers[2].read_control(),
            0x0400_010C => self.timers[3].read_value(),
            0x0400_010E => self.timers[3].read_control(),
            _ => bail!("unexpected addr timers"),
        }
    }

    pub fn write_16(&mut self, addr: u32, val: u16) -> Result<()> {
        match addr {
            0x0400_0100 => self.timers[0].write_value(val),
            0x0400_0102 => self.timers[0].write_control(val),
            0x0400_0104 => self.timers[1].write_value(val),
            0x0400_0106 => self.timers[1].write_control(val),
            0x0400_0108 => self.timers[2].write_value(val),
            0x0400_010A => self.timers[2].write_control(val),
            0x0400_010C => self.timers[3].write_value(val),
            0x0400_010E => self.timers[3].write_control(val),
            _ => bail!("unexpected addr timers"),
        }
    }

    // TODO: リファクタしたい
    pub fn irq(&self, i: usize) -> bool {
        self.timers[i].irq
    }
}

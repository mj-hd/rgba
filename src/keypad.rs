use anyhow::Result;
use bitfield::bitfield;
use log::trace;

#[derive(Debug, Clone, Copy)]
pub enum KeyType {
    A,
    B,
    L,
    R,
    Down,
    Up,
    Left,
    Right,
    Start,
    Select,
}

bitfield! {
    #[derive(Clone, Copy)]
    pub struct KeyInput(u16);
    l, set_l: 9;
    r, set_r: 8;
    down, set_down: 7;
    up, set_up: 6;
    left, set_left: 5;
    right, set_right: 4;
    start, set_start: 3;
    select, set_select: 2;
    b, set_b: 1;
    a, set_a: 0;
}

enum KeyCond {
    And,
    Or,
}

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct KeyCnt(u16);
    _irq_cond, _: 15;
    irq_enable, _: 14;
    enable_l, _: 9;
    enable_r, _: 8;
    enable_down, _: 7;
    enable_up, _: 6;
    enable_left, _: 5;
    enable_right, _: 4;
    enable_start, _: 3;
    enable_select, _: 2;
    enable_b, _: 1;
    enable_a, _: 0;
}

impl KeyCnt {
    fn irq_cond(&self) -> KeyCond {
        match self._irq_cond() {
            false => KeyCond::Or,
            true => KeyCond::And,
        }
    }
}

pub struct KeyPad {
    cnt: KeyCnt,
    input: KeyInput,

    pub irq: bool,
}

impl KeyPad {
    pub fn new() -> Self {
        Self {
            input: KeyInput(0x3FF),
            cnt: Default::default(),
            irq: false,
        }
    }

    pub fn tick(&mut self) -> Result<()> {
        if !self.cnt.irq_enable() {
            self.irq = false;
            return Ok(());
        }

        let input = !self.input.0 & 0x3FF;
        let enable = self.cnt.0 & 0x3FF;

        let result = input & enable;

        match self.cnt.irq_cond() {
            KeyCond::Or if result > 0 => {
                trace!("KEYPAD IRQ REQUESTED");
                self.irq = true;
            }
            KeyCond::And if result == enable => {
                trace!("KEYPAD IRQ REQUESTED");
                self.irq = true;
            }
            _ => {
                self.irq = false;
            }
        }

        Ok(())
    }

    pub fn press(&mut self, key: KeyType) {
        trace!("KEY PRESSED: {:?}", key);
        match key {
            KeyType::A => self.input.set_a(false),
            KeyType::B => self.input.set_b(false),
            KeyType::L => self.input.set_l(false),
            KeyType::R => self.input.set_r(false),
            KeyType::Left => self.input.set_left(false),
            KeyType::Right => self.input.set_right(false),
            KeyType::Down => self.input.set_down(false),
            KeyType::Up => self.input.set_up(false),
            KeyType::Start => self.input.set_start(false),
            KeyType::Select => self.input.set_select(false),
        }
    }

    pub fn release(&mut self, key: KeyType) {
        trace!("KEY RELEASED: {:?}", key);
        match key {
            KeyType::A => self.input.set_a(true),
            KeyType::B => self.input.set_b(true),
            KeyType::L => self.input.set_l(true),
            KeyType::R => self.input.set_r(true),
            KeyType::Left => self.input.set_left(true),
            KeyType::Right => self.input.set_right(true),
            KeyType::Down => self.input.set_down(true),
            KeyType::Up => self.input.set_up(true),
            KeyType::Start => self.input.set_start(true),
            KeyType::Select => self.input.set_select(true),
        }
    }

    pub fn read_input(&self) -> Result<u16> {
        Ok(self.input.0)
    }

    pub fn write_input(&mut self, val: u16) -> Result<()> {
        self.input = KeyInput(val);

        trace!("KEYPAD INPUT WROTE: {:04X}", val);

        Ok(())
    }

    pub fn read_cnt(&self) -> Result<u16> {
        Ok(self.cnt.0)
    }

    pub fn write_cnt(&mut self, val: u16) -> Result<()> {
        self.cnt = KeyCnt(val);

        trace!("KEYPAD CNT WROTE: {:04X}", val);

        Ok(())
    }
}

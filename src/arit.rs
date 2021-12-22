pub trait IsOverflowAdd {
    fn is_overflow_add(&self, right: Self) -> bool;
}

impl IsOverflowAdd for u32 {
    fn is_overflow_add(&self, right: u32) -> bool {
        let result = self.wrapping_add(right);

        let s1 = (self >> 31) > 0;
        let s2 = (right >> 31) > 0;
        let s3 = (result >> 31) > 0;

        (s1 && s2 && !s3) || (!s1 && !s2 && s3)
    }
}

impl IsOverflowAdd for u16 {
    fn is_overflow_add(&self, right: u16) -> bool {
        let result = self.wrapping_add(right);

        let s1 = (self >> 15) > 0;
        let s2 = (right >> 15) > 0;
        let s3 = (result >> 15) > 0;

        (s1 && s2 && !s3) || (!s1 && !s2 && s3)
    }
}

impl IsOverflowAdd for u8 {
    fn is_overflow_add(&self, right: u8) -> bool {
        let result = self.wrapping_add(right);

        let s1 = (self >> 7) > 0;
        let s2 = (right >> 7) > 0;
        let s3 = (result >> 7) > 0;

        (s1 && s2 && !s3) || (!s1 && !s2 && s3)
    }
}

pub trait IsOverflowSub {
    fn is_overflow_sub(&self, right: Self) -> bool;
}

impl IsOverflowSub for u32 {
    fn is_overflow_sub(&self, right: u32) -> bool {
        let result = self.wrapping_sub(right);

        let s1 = (self >> 31) > 0;
        let s2 = (right >> 31) > 0;
        let s3 = (result >> 31) > 0;

        (!s1 && s2 && s3) || (s1 && !s2 && !s3)
    }
}

impl IsOverflowSub for u16 {
    fn is_overflow_sub(&self, right: u16) -> bool {
        let result = self.wrapping_sub(right);

        let s1 = (self >> 15) > 0;
        let s2 = (right >> 15) > 0;
        let s3 = (result >> 15) > 0;

        (!s1 && s2 && s3) || (s1 && !s2 && !s3)
    }
}

impl IsOverflowSub for u8 {
    fn is_overflow_sub(&self, right: u8) -> bool {
        let result = self.wrapping_sub(right);

        let s1 = (self >> 7) > 0;
        let s2 = (right >> 7) > 0;
        let s3 = (result >> 7) > 0;

        (!s1 && s2 && s3) || (s1 && !s2 && !s3)
    }
}

pub trait IntoI10 {
    fn into_i10(&self) -> i16;
}

impl IntoI10 for u16 {
    fn into_i10(&self) -> i16 {
        let mut result = self & 0x3FF;

        if self & 0x0400 > 1 {
            result |= 0xFC00;
        }

        result as i16
    }
}

pub trait IntoI24 {
    fn into_i24(&self) -> i32;
}

impl IntoI24 for u32 {
    fn into_i24(&self) -> i32 {
        let mut result = self & 0x00FF_FFFF;

        if self & 0x0080_0000 > 0 {
            result |= 0xFF00_0000;
        }

        result as i32
    }
}

pub struct ShiftResult(pub u32, pub bool);

pub trait Shift {
    fn lsl(self, shifter: u32, c: bool) -> ShiftResult;
    fn lsr(self, shifter: u32, zero_shift: bool) -> ShiftResult;
    fn asr(self, shifter: u32, zero_shift: bool) -> ShiftResult;
    fn ror(self, shifter: u32, c: bool, zero_shift: bool) -> ShiftResult;
}

impl Shift for u32 {
    fn lsl(self, shifter: u32, c: bool) -> ShiftResult {
        if shifter == 0 {
            ShiftResult(self, c)
        } else {
            ShiftResult(
                self.checked_shl(shifter).unwrap_or(0),
                if shifter <= 32 {
                    self.checked_shr(32 - shifter).unwrap_or(0) & 1 > 0
                } else {
                    false
                },
            )
        }
    }

    fn lsr(self, shifter: u32, zero_shift: bool) -> ShiftResult {
        let shifter = if zero_shift { 32 } else { shifter };
        ShiftResult(
            self.checked_shr(shifter).unwrap_or(0),
            self.checked_shr(shifter.saturating_sub(1)).unwrap_or(0) & 1 > 0,
        )
    }

    fn asr(self, shifter: u32, zero_shift: bool) -> ShiftResult {
        if zero_shift || shifter >= 32 {
            let c = self >> 31 > 0;
            return ShiftResult(if c { !0 } else { 0 }, c);
        }

        ShiftResult(
            (self as i32).checked_shr(shifter).unwrap_or(0) as u32,
            (self as i32)
                .checked_shr(shifter.saturating_sub(1))
                .unwrap_or(0)
                & 1
                > 0,
        )
    }

    fn ror(self, shifter: u32, c: bool, zero_shift: bool) -> ShiftResult {
        if zero_shift {
            let c = c as u32;
            ShiftResult((c << 31) | (self >> 1), self & 1 == 1)
        } else {
            ShiftResult(
                self.rotate_right(shifter),
                self.checked_shr(shifter.saturating_sub(1)).unwrap_or(0) & 1 == 1,
            )
        }
    }
}

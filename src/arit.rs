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

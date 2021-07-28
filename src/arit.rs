pub fn is_overflow_pl(left: u32, right: u32) -> bool {
    let result = left.wrapping_add(right);

    let s1 = (left >> 31) > 0;
    let s2 = (right >> 31) > 0;
    let s3 = (result >> 31) > 0;

    (s1 && s2 && !s3) || (!s1 && !s2 && s3)
}

pub fn is_overflow_ng(left: u32, right: u32) -> bool {
    let result = left.wrapping_sub(right);

    let s1 = (left >> 31) > 0;
    let s2 = (right >> 31) > 0;
    let s3 = (result >> 31) > 0;

    (!s1 && s2 && s3) || (s1 && !s2 && !s3)
}

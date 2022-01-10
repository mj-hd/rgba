use crate::cpu::Mode;
use std::ops::{BitAnd, Not, Shr};

use bitfield::bitfield;
use num_derive::FromPrimitive;
use num_traits::{
    ops::overflowing::{OverflowingAdd, OverflowingSub},
    FromPrimitive,
};

use crate::arit::{IsOverflowAdd, IsOverflowSub};

bitfield! {
    #[derive(Default, Clone, Copy)]
    pub struct Psr(u32);
    impl Debug;
    pub n, set_n: 31;
    pub z, set_z: 30;
    pub c, set_c: 29;
    pub v, set_v: 28;
    pub i, set_i: 7;
    pub f, set_f: 6;
    pub t, set_t: 5;
    pub into Mode, mode, set_mode: 4, 0;
}

impl Psr {
    pub fn set_nz_by<T>(&mut self, result: T)
    where
        T: Eq
            + Copy
            + Ord
            + Shr<usize, Output = T>
            + BitAnd<Output = T>
            + Not<Output = T>
            + Default,
    {
        self.set_n(result & !(!T::default() >> 1) > T::default());
        self.set_z(result == T::default());
    }

    pub fn set_pl_nzcv_by<T>(&mut self, left: T, right: T)
    where
        T: OverflowingAdd
            + IsOverflowAdd
            + Shr<usize, Output = T>
            + BitAnd<Output = T>
            + Not<Output = T>
            + Copy
            + Ord
            + Default,
    {
        self.set_pl_nzc_by(left, right);
        self.set_pl_v_by(left, right);
    }

    pub fn set_pl_nzc_by<T>(&mut self, left: T, right: T)
    where
        T: OverflowingAdd
            + Copy
            + Ord
            + Shr<usize, Output = T>
            + BitAnd<Output = T>
            + Not<Output = T>
            + Default,
    {
        let (result, c) = left.overflowing_add(&right);
        self.set_nz_by(result);
        self.set_c(c);
    }

    pub fn set_ng_nzcv_by<T>(&mut self, left: T, right: T)
    where
        T: OverflowingSub
            + IsOverflowSub
            + Copy
            + Ord
            + Shr<usize, Output = T>
            + BitAnd<Output = T>
            + Not<Output = T>
            + Default,
    {
        self.set_ng_nzc_by(left, right);
        self.set_ng_v_by(left, right);
    }

    pub fn set_ng_nzc_by<T>(&mut self, left: T, right: T)
    where
        T: OverflowingSub
            + Copy
            + Ord
            + Shr<usize, Output = T>
            + BitAnd<Output = T>
            + Not<Output = T>
            + Default,
    {
        let (result, c) = left.overflowing_sub(&right);
        self.set_nz_by(result);
        self.set_c(!c);
    }

    pub fn set_pl_v_by<T>(&mut self, left: T, right: T)
    where
        T: IsOverflowAdd,
    {
        let v = left.is_overflow_add(right);
        self.set_v(v);
    }

    pub fn set_ng_v_by<T>(&mut self, left: T, right: T)
    where
        T: IsOverflowSub,
    {
        let v = left.is_overflow_sub(right);
        self.set_v(v);
    }
}

#[derive(Debug, FromPrimitive)]
pub enum Cond {
    Eq,
    Ne,
    CsHs,
    CcLo,
    Mi,
    Pl,
    Vs,
    Vc,
    Hi,
    Ls,
    Ge,
    Lt,
    Gt,
    Le,
    Al,
    Nv,
}

impl From<u32> for Cond {
    fn from(n: u32) -> Cond {
        FromPrimitive::from_u32(n).unwrap_or(Cond::Eq)
    }
}

impl Cond {
    pub fn guard(&self, cpsr: Psr) -> bool {
        match self {
            Cond::Eq => cpsr.z(),
            Cond::Ne => !cpsr.z(),
            Cond::CsHs => cpsr.c(),
            Cond::CcLo => !cpsr.c(),
            Cond::Mi => cpsr.n(),
            Cond::Pl => !cpsr.n(),
            Cond::Vs => cpsr.v(),
            Cond::Vc => !cpsr.v(),
            Cond::Hi => cpsr.c() && !cpsr.z(),
            Cond::Ls => !cpsr.c() || cpsr.z(),
            Cond::Ge => cpsr.n() == cpsr.v(),
            Cond::Lt => cpsr.n() != cpsr.v(),
            Cond::Gt => !cpsr.z() && (cpsr.n() == cpsr.v()),
            Cond::Le => cpsr.z() || (cpsr.n() != cpsr.v()),
            Cond::Al => true,
            Cond::Nv => false,
        }
    }
}

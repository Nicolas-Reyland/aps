// Either struct

use std::fmt;

#[derive(Debug, PartialEq, Hash)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<L, R> fmt::Display for Either<L, R>
where
    L: fmt::Display,
    R: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Either::Left(x) => x.fmt(f),
            Either::Right(x) => x.fmt(f),
        }
    }
}


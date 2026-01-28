use std::io::Write;

use anyhow::Result;
use serde::Serialize;

pub trait CompilerPass {
    fn num_errors(&self) -> u32;
    fn inc_error(&mut self);
    fn has_error(&self) -> bool {
        self.num_errors() > 0
    }
}

pub trait Writable {
    fn write<T: Write>(&self, writer: &mut Writer<T>, eol: bool) -> Result<()>;
}

pub struct Writer<'a, T: Write> {
    writer: &'a mut T,
    tabc: u8,
}

impl<'a, T: Write> Writer<'a, T> {
    pub fn new(writer: &'a mut T) -> Self {
        Self { writer, tabc: 0 }
    }
    pub fn tabs(&mut self) -> Result<()> {
        for _ in 0..self.tabc {
            write!(self, "\t")?;
        }
        Ok(())
    }
    pub fn tabc(&self) -> u8 {
        self.tabc
    }
    pub fn inctabs(&mut self) {
        self.tabc += 1;
    }
    pub fn dectabs(&mut self) {
        self.tabc -= 1;
    }
}

impl<T: Write> Write for Writer<'_, T> {
    fn by_ref(&mut self) -> &mut Self
    where
        Self: Sized,
    {
        self
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }

    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.writer.write(buf)
    }

    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        self.writer.write_all(buf)
    }

    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> std::io::Result<()> {
        self.writer.write_fmt(args)
    }

    fn write_vectored(&mut self, bufs: &[std::io::IoSlice<'_>]) -> std::io::Result<usize> {
        self.writer.write_vectored(bufs)
    }
}

#[derive(Debug, Clone, Copy, Serialize, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct NodeId(pub u32);

#[derive(Clone, Copy, Serialize, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct NodeRef(pub usize);

static mut _ID: u32 = 0;

impl NodeId {
    pub fn next() -> Self {
        unsafe {
            _ID += 1;
            Self(_ID)
        }
    }
}

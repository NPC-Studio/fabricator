use std::{error::Error as StdError, fmt, sync::Arc};

use crate::{
    callback::Callback,
    closure::Closure,
    debug::LineNumber,
    error::{Error, ExternError, RawGc, RuntimeError},
    string::SharedStr,
};

#[derive(Debug, Clone)]
pub struct VmError<'gc> {
    pub error: Error<'gc>,
    pub backtrace: Arc<[BacktraceFrame<'gc>]>,
}

impl<'gc> fmt::Display for VmError<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.error)?;
        write!(f, "VM backtrace:")?;
        for (i, frame) in self.backtrace.iter().rev().enumerate() {
            writeln!(f)?;
            write!(f, "{:>4}: ", i)?;
            match frame {
                BacktraceFrame::Closure(closure_frame) => {
                    write!(
                        f,
                        "{}:{}",
                        closure_frame.chunk_name(),
                        closure_frame.line_number()
                    )?;
                }
                BacktraceFrame::Callback(callback) => {
                    write!(f, "<callback {:p}>", callback.into_inner())?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum CallError {
    Runtime(RuntimeError),
    Vm {
        error: ExternError,
        backtrace: Arc<[ExternBacktraceFrame]>,
    },
}

impl fmt::Display for CallError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CallError::Runtime(runtime_err) => write!(f, "runtime error: {runtime_err}"),
            CallError::Vm { error, backtrace } => {
                writeln!(f, "{}", error)?;
                write!(f, "VM backtrace:")?;
                for (i, frame) in backtrace.iter().rev().enumerate() {
                    writeln!(f)?;
                    write!(f, "{:>4}: ", i)?;
                    match frame {
                        ExternBacktraceFrame::Closure(closure_frame) => {
                            write!(
                                f,
                                "{}:{}",
                                closure_frame.chunk_name, closure_frame.line_number
                            )?;
                        }
                        ExternBacktraceFrame::Callback(callback) => {
                            write!(f, "<callback {:p}>", callback)?;
                        }
                    }
                }
                Ok(())
            }
        }
    }
}

impl StdError for CallError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            CallError::Runtime(runtime_error) => Some(runtime_error.as_ref()),
            CallError::Vm { error, .. } => error.source(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ClosureBacktraceFrame<'gc> {
    pub closure: Closure<'gc>,
    pub instruction: usize,
}

impl<'gc> ClosureBacktraceFrame<'gc> {
    pub fn chunk_name(&self) -> &SharedStr {
        self.closure.prototype().chunk().name()
    }

    pub fn line_number(&self) -> LineNumber {
        let chunk = self.closure.prototype().chunk();
        let prototype = self.closure.prototype();
        let bytecode = prototype.bytecode();
        let span = bytecode.span(self.instruction);
        chunk.line_number(span.start())
    }

    pub fn to_extern(&self) -> ExternClosureBacktraceFrame {
        ExternClosureBacktraceFrame {
            closure: RawGc::new(self.closure.into_inner()),
            instruction: self.instruction,
            line_number: self.line_number(),
            chunk_name: self.chunk_name().clone(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum BacktraceFrame<'gc> {
    Closure(ClosureBacktraceFrame<'gc>),
    Callback(Callback<'gc>),
}

impl<'gc> BacktraceFrame<'gc> {
    pub fn to_extern(&self) -> ExternBacktraceFrame {
        match self {
            BacktraceFrame::Closure(closure_backtrace_frame) => {
                ExternBacktraceFrame::Closure(closure_backtrace_frame.to_extern())
            }
            BacktraceFrame::Callback(callback) => {
                ExternBacktraceFrame::Callback(RawGc::new(callback.into_inner()))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternClosureBacktraceFrame {
    pub closure: RawGc,
    pub instruction: usize,
    pub line_number: LineNumber,
    pub chunk_name: SharedStr,
}

#[derive(Debug, Clone)]
pub enum ExternBacktraceFrame {
    Closure(ExternClosureBacktraceFrame),
    Callback(RawGc),
}

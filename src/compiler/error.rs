//! A basic skeleton type for all errors about user input that are generated
//! by the compiler.  The [`CompilerError`] contains information about the
//! location within source code that caused the error and an inner error type
//! that is a more refined and compiler stage specific error type.
//!
//! Because every error generated by the compiler must be associated with a
//! subset of the input source code, that is lifted into this global wrapper
//! type and more specific error information is reserved of the inner error.
//!
//! This also defines the CompilerDisplay trait which all inner errors must
//! implement.  The CompilerDisplay trait handles converting an Error type to
//! a human readable format and also handles converting internal representations
//! to human readable representations (e.g. the StringId is converted to the
//! actual string).

#![macro_use]
use crate::StringTable;

use super::{CompilerDisplay, CompilerDisplayError, SourceMap, Span};

/// Represents all errors that are generated from within the Compiler
/// module and its submodules.
///
/// This type captures common metadata which is necessarily present for
/// all errors which are caused by input source code.  E.g. the line #
/// that the error occurs on. This also handles formatting all error messages
/// with the universal metadata along with the inner metadata.
///
/// The inner error allows metadata which is specific to a submodule within
/// the compiler. E.g., the errors themselves are submodule specific and
/// are stored in the `inner` field.
#[derive(Clone, Debug, PartialEq)]
pub struct CompilerError<IE: CompilerDisplay> {
    /// The span of source code that caused this error
    span: Span,

    /// An inner error value that contains more specific error information
    inner: IE,
}

impl<IE> CompilerError<IE>
where
    IE: CompilerDisplay,
{
    pub fn new(span: Span, inner: IE) -> Self {
        CompilerError { span, inner }
    }

    /// Moves the Line number and Inner error out of the wrapping [CompilerError].
    /// This is to allow conversion of one type of [CompilerError] to another type, by
    /// converting the `inner` type into a new type and then creating a new [CompilerError]
    /// wrapper.
    pub fn take(self) -> (Span, IE) {
        (self.span, self.inner)
    }
}

impl<IE> CompilerDisplay for CompilerError<IE>
where
    IE: CompilerDisplay,
{
    /// For each source code file, format the line number so that
    /// If the span covers one line then format as "L{line}"
    /// If the span covers multiple then format as: "L{min}-{max}"
    ///
    /// If the span covers only one file, then format as "{Lines}"
    /// If the span covers multiple files, format as "{File}:{Lines}"
    fn fmt(&self, sm: &SourceMap, st: &StringTable) -> Result<String, CompilerDisplayError> {
        let inner = self.inner.fmt(sm, st)?;

        let lines_by_file = sm.lines_in_span(self.span).into_iter().map(|(f, lines)| {
            let line = format_line_set(&lines).expect("Span covers no indexed source code");
            (f, line)
        });

        let formatted_span = if lines_by_file.len() == 1 {
            lines_by_file.map(|(_, lines)| lines).collect()
        } else {
            lines_by_file
                .map(|(f, lines)| format!("{:?}:{}", f, lines))
                .collect::<Vec<_>>()
                .join("; ")
        };

        Ok(format!("{}: {}", formatted_span, inner))
    }
}

/// Take a set of line numbers and format into a string that describes the range
/// of lines.
///
/// If there is 1 line, then format as `L<line number>`
/// If there are multiple lines, then format sa `L<min line>-<max line`
fn format_line_set(lines: &[u32]) -> Option<String> {
    if lines.len() > 0 {
        let min = lines.iter().min().unwrap(); // unwrap b/c if the len > 1 and we cannot find min/max something serious is wrong
        let max = lines.iter().max().unwrap();

        if min < max {
            Some(format!("L{}-{}", min, max))
        } else {
            // If min == max then formatting as `min-min` would be pointless
            Some(format!("L{}", min))
        }
    } else {
        None
    }
}

/// Helper macro to get rid of repitition of boilerplate code.
//#[macro_export]
macro_rules! err {
    ($span:expr, $kind: expr) => {
        Err(CompilerError::new($span, $kind))
    };
}

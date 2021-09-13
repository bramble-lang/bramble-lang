# Displaying Error Messages with String IDs
## Problem
The String Pool design means that, outside of the lexer, the compiler never looks
at or uses the actual string value of any token; it only has the String ID. When
generating error messages, then, the compiler cannot put the string value for 
a token into the error message. So, there needs to be some way for the compiler
to provide the appropriate string ids for any tokens or names which need to be
put into an error message and for those string IDs to be converted to a human
readable string value.

Ultimately, every token, node, IR type the compiler uses will need someway to be
displayed for humans.

## Restrictions
The goal with the string pool is to allow the compiler to work without having to
use string data for anything, so, the compiler (outside of the lexer) shall never
use or be given the string pool.  It can only communicate using String IDs.

Conversion to and from String IDs is an interface boundary action: the human provided
code or the human target error messages exist on the outside of the compiler and
the interface is meant to convert provided text to string ids and convert information
meant for human consumption from string ids to the correct value.

## Concerns
The biggest problem is probably Paths, because a path is a set of StringIds, while
everything else is a single StringId.

## Commons
What are the data which are common across all or most errors in the compiler?

- line #
- source code token
- col #
- File name
- 

The things which are common across all errors that are thrown within the Compiler
module should be pulled into a single type and then things which are specific
to submodules (if they exist) should be provided in enums or submodule specific
structs which are composed of the supra-common data.

## Designs
### Converter Value
Provide a type which stores a string ID given by the compiler, then at the UI level
that type can be converted to a String Value by calling a function and giving the
function the string pool.

One reason to use this could be to provide a Converter type for every IR type
so there could be custom formatting, because the IR type that is associated with
an error (or any kind of internal to UI communication) is lost when just using
the StringID.

```rust
struct Converter {
    sid: StringId
}

impl Converter {
    /// Compiler creates this
    pub fn new(StringId) -> Converter

    /// At the UI level, call this and provide the String Pool and it will attempt
    // to convert the StringID to a String
    pub fn format(&self, &StringTable) -> Result<String>
}
```

One advantage about this rough idea is that I believe it provides a solution for
Paths (which contain a vector of string ids).  So, a converter could actually be
created for Paths and one for other things or a converter could store a vec
of string ids and provide formatting.

### Error Value Stores String IDs
Every error message is its own type and takes the required number of StringIDs
as part of its data.  Then there is a format function which takes a StringTable
and converts string ids to string values.

```rust
struct BindMismatch {
    lhs: StringId,
    expected: Type,
    actual: Type,
}

impl BindMismatch {
    fn format(st: &StringTable) -> Result<String> {
        format!("{} is of type {} but got {}", st.get(self.lhs), expected, actual)
    }
}
```

or use an enum to cover a bunch of different errors with less boilerplate. The
principal is the same.

To deal with paths, error messages that will display a path can take a vector
of string ids rather than just one string id.

One question about this method is how many distinct error messages I have.  If there
are a lot, then creating one error type/variant for each one could be very tedious.

(I may only have 66 total error points in the compiler?  Trying to get an estimate
that does not include error values in my tests)

## Decision
I think the second option is simpler.  Not only is it simpler, but the step of
making different types/variants for each error message is also somehting that would
be done for the first choice. So, the second choice makes the most sense.
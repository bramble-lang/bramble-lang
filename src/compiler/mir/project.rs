//! Represents and entire Bramble program, including imported libraries,
//! in MIR form.


/// Uniquely identifies an item that exists in the static memory of a program
/// e.g., a function or static variable.
struct DefId;

/// The representation of the full program, including imports
struct MirProject;

/// Represents definitions of static items within this project
/// This includes: functions and static variables and static constants.
/// This also includes static strings.
struct Definitions;


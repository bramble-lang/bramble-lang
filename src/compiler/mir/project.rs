/*! Represents and entire Bramble program, including imported libraries,
in MIR form.
*/

/// Represents everything involved in compiling the current target compilation
/// unit (executable, library, etc.).
/// 
/// This corresponds to the Root of the Canonical Path
struct MirProgram;

/// The representation of the full program, including imports
/// 
/// This corresponds to the Project component of the Canonical Path
struct MirProject;

/// Represents definitions of static items within this project
/// This includes: functions and static variables and static constants.
/// This also includes static strings.
struct Definitions;

/// Uniquely identifies an item that exists in the static memory of a program
/// e.g., a function or static variable.
struct DefId;

/// A static item, this could be a function or data.  Data in turn can be a
/// variable or a constant.
struct StaticItem;
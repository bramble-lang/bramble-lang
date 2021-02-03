/**
 * Hardware specific input validation and analysis.
 *
 * The following tasks are performed by this submodule
 * 1. Compute size of all variables, and custom types.
 * 2. Compute the stack frame size and assign locations within the stack frame for
 * all variables and function parameters.  This will also check for cycles in
 * struct definitions and structs which have infinite size.
 * 3. Build a global table of all custom defined types and compute the size of
 * all fields and assign relative offsets within the memory assigned to the struct
 * for those fields to reside.
 * 4. Construct a string pool of all string literals.
 */
pub(super) mod layout;
pub(super) mod scope;
pub(super) mod set_reg;
pub(super) mod stack;
pub(super) mod stringpool;
pub(super) mod struct_definition;
pub(super) mod struct_table;
pub(super) mod symbol_table;

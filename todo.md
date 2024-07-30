### TODOS
* Binary op on i32 MAX value should give an error or warning 
* be able to convert from i32 to i64
* Check the Obj is type functions
* Struct functions expressions can be made Rc<LumiExpr>? Might save a clone
* Extend obj and object_type comparisons
* Be able to add and remove items from a list

* Expr::Call needs to also output stuff, only outputs Obj::Null so it's useless for WASM
* file input/output functions

### BUGS
* 

# structs
When evaluating a declared variable inside a struct with the same name as a variable outside of the struct, it gets the wrong value from the env.

# standard library
String Manipulation Functions: Functions for manipulating strings, such as concatenation, substring extraction, searching, replacing, and formatting.

Mathematical Functions: Standard mathematical functions like trigonometric functions (sin, cos, tan), exponential and logarithmic functions (exp, log), and basic arithmetic operations.

Data Structure Functions: Functions for working with common data structures like arrays, lists, sets, dictionaries/maps, and stacks/queues. This includes operations for insertion, deletion, updating, and accessing elements.

File Input/Output Functions: Functions for reading from and writing to files, creating directories, listing directory contents, and managing file metadata.

Input/Output Functions: Functions for interacting with the standard input/output streams, such as reading user input and printing output.

Error Handling Functions: Functions for reporting errors, handling exceptions, and managing error states.

Concurrency and Synchronization Functions: Functions for creating and managing threads, synchronization primitives like locks and semaphores, and high-level concurrency abstractions like futures and async/await.

Networking Functions: Functions for making network requests, setting up servers, and handling network protocols.

Time and Date Functions: Functions for working with dates, times, time zones, and durations.

System Interaction Functions: Functions for interacting with the operating system, such as executing external commands, accessing environment variables, and managing processes.

Utility Functions: Miscellaneous utility functions that provide common functionality, such as sorting, searching, parsing, formatting, and serialization/deserialization.
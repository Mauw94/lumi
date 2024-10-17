### TODOS
* create VSCode extension for syntax highlighting
* implement dictionaries
* implement including another lumi file (be able to pass current env when including)
* implement parsing from string to int
* Add error handling E.G. read_file can return a result or an error, when an error is returned we can handle it without completely shutting down the current process
    - TODO: use namespace4.lumi as example, if read_file goes wrong the rest of the code should still execute.
    - Be able to evaluate result of a function in lumi code using the eval keyword and then checking for a result or error.
* Give appropriate error when not including a namespace but still calling one of its functions
* be able to call something like this docs [function_name], to get docs about a function and how to use it
* When calling a functions docs, give also examples of the parameters usages
* Add tests for some builtin functions
* Add docs that functions calls are also possible like var.abc() and not only abc(var)
* Add default structs.
* be able to convert from i32 to i64
* Check the Obj is type functions
* Struct functions expressions can be made Rc<LumiExpr>? Might save a clone
* Extend obj and object_type comparisons
* Expr::Call needs to also output stuff, only outputs Obj::Null so it's useless for WASM
* file input/output functions

### BUGS
left = 3
right = 2
something -> left * right

* This throws an error (A number was expected). The left hand side of the evaluation is seen as 'something' and not as 'left', therefore giving us a Null.
something: int
something = left * right
* This does work. But it should also work as in the example above.

# structs
When evaluating a declared variable inside a struct with the same name as a variable outside of the struct, it gets the wrong value from the env.

# standard library
String Manipulation Functions: Functions for manipulating strings, such as concatenation, substring extraction, searching, replacing, and formatting.

Mathematical Functions: Standard mathematical functions like trigonometric functions (sin, cos, tan), exponential and logarithmic functions (exp, log), and basic arithmetic operations.

Data Structure Functions: Functions for working with common data structures like arrays, lists, sets, dictionaries/maps, and stacks/queues. This includes operations for insertion, deletion, updating, and ac  cessing elements.

File Input/Output Functions: Functions for reading from and writing to files, creating directories, listing directory contents, and managing file metadata.

Input/Output Functions: Functions for interacting with the standard input/output streams, such as reading user input and printing output.

Error Handling Functions: Functions for reporting errors, handling exceptions, and managing error states.

Concurrency and Synchronization Functions: Functions for creating and managing threads, synchronization primitives like locks and semaphores, and high-level concurrency abstractions like futures and async/await.

Networking Functions: Functions for making network requests, setting up servers, and handling network protocols.

Time and Date Functions: Functions for working with dates, times, time zones, and durations.

System Interaction Functions: Functions for interacting with the operating system, such as executing external commands, accessing environment variables, and managing processes.

Utility Functions: Miscellaneous utility functions that provide common functionality, such as sorting, searching, parsing, formatting, and serialization/deserialization.
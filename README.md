# Lumi

## What's in it?

* Dynamically typed
* Declare variables with `->`
* Stuff that introduces scopes: functions, loops
* Everything is an expression
* Built-in functions
* Structs
* Lists `[a, b, c]`
* Dictionaries `{a: b, c: e}` *not yet implemented*
* For loops `i: 1 to 5 step 1`
* Functions `fn add(x, b) { return a + b }`

## Build wasm-pack
To build the project as WASM, run the following command:
* run command wasm-pack build --target web

The wasm files need are found inside the pkg folder.

## Add custom lumi syntax highlighting extension to vscode
* Go to the root dir, then copy the syntax folder to the vscode extensions folder
* cp -r syntax ~/.vscode/extensions (on macOS)

## Syntactic suger
When working with a list of numbers we can use the ```every``` keyword. 

e.g. 
```
a: list -> [1, 2, 3, 4, 5]
b -> a every * 2
```

All the elements in ```a``` will now be multiplied by 2 and collected in a new vec ```b```.
Other operators supported with the ```every``` keyword are +, -, / and *

## Variables and assignments
Declare with ```->``` and assign with ```=```
```x -> 2``` works, but if you want to declare a variable with a type you can do ```x: int -> 2```.
Declaring without a type works somewhat as in TypeScript.   
Internally the object type will be ```none``` (```any``` in TypeScript) and you can then assign any other type to the variable.  

After declaring ```x -> 2``` we can still do this ```x = "test123"```.  
If we try ```x: int -> 2``` and then ```x = "test"``` a type mismatch error will be thrown.

Declaring a variable without a value also works. When trying to use this variable it will return the default value for the assigned type.
*Declaring a variable without a type and without a value does not work.*
```
lumi> x: int
lumi> print x
0
```

You can retrieve a value from a list by index:
```
lumi> x: list -> ["a", "b", "c"]
lumi> print x[0]
"a"
```

### Different data types
* Lists: `x: list -> [1, 2, 3, 4, 5]`
* Dictionaries: `{a: b, c: e}` *not yet implemented*
* Numbers:
    * int: `x: int -> 2` 
        * small (i16)
        * big (i32)
        * long (i64)
    * float: `x: float -> 2.0`
* Strings: `x: str -> "test"`
* Booleans: `x: bool -> true` 
* Functions: `fn test() {}`
* Structs: `struct test() {}`

            
*int type is automatically created at runtime, no need to specify which int type it will have to be.*
*Thus,* `x: int -> 32768` *will be an i32. Smaller than 32767 will be an i16* *and* `x: int -> 2147483648` *will be an i64.*

### Control flow
```
x: list -> ["a", "b", "c"]

if (len(x) == 3) {
    print "Length of x is 3".
} else {
    print "Length of x is not 3."
}
```

### Functions
```
fn add(a, b) {
    return a + b
}

x -> 2
y -> 3
print add(x, y)
```

### Or use built-in functions such as:
```
lumi> time()
Current time is 2024-03-28 21:14:16
```

```
lumi> x: list -> [1, 2, 3]
lumi> len(x)
3
```

```
lumi> x: int -> 100
lumi> print string(x)
"100"
```

```
lumi> a: str -> "Hello World"
lumi> b -> replace_str(a, "World", "hehe xd")
lumi> print b
"Hello hehe xd"
```

An overview of all built-in functions can be retrieved by calling ```built_in()```. An overview of all available namespaces by calling ```namespaces()```.
When including a namespace and then calling ```built_in()``` we will also see the added functions from that included namespace. 
After the function we can see of which library the function is a part of.

* ```vars (stdlib)```
* ```read_file (fileio)```


# Built-in functions docs
### StdLib functions
These functions are not called on an object e.g. ```a: int -> 45 string(a)``` or ```a: str -> "hello world" b -> concat_str(a, "test123")```
* time
* vars
* built_in
* namespace
* typeof
* substr
* namespaces
* string
* contains_str
* replace_str
* concat_str 

## Functions for Lists
These functions are directly called on the list object e.g. ```a: list -> [1, 2, 3] a.pop()```
* ```sum()```
    - Sums contents of a list. Can also be used on a list containing only strings.
* ```len()```
    - Gives the length of a list.
* ```push()```
    - Adds an item to the end of the list.
* ```last()```
    - Returns the last item of the list without removing it from the list.
* ```first()```
    - Returns the first item of the list without removing it from the list.
* ```pop()```
    - Returns the last item of the list and removes it from the list.
* ```slice(param1, param2)```
    - Slices a part of the list without removing the elements. *param1 excluded, param2 included*

### Or include a namespace for more functionality, such as the FileIO namespace
These can be used like ```include fileio``` and all of its funtionality will be loaded into the current top environment.
When you don't need the namespace anymore and want to clear it up we can call ```exclude fileio``` and the namespaces will be unloaded from the environment.
*****
An example of this can be found in /examples/namespace[1-4].lumi


### List specific functions usages
```
lumi> test: list -> [1, 2, 3, 4, 5]
lumi> print test.first()
1
```

```
lumi> test: list -> ["a", "d", "f", "g", "z"]
lumi> another_var -> test.last()
lumi> print another_var
"z"
```

```
lumi> test: list -> [1, 2, 3, 4, 5]
lumi> test.push(6)
lumi> print test.pop()
6
```

### Loops
```
i: 1 to 5 step 1 {
    print i
}
```

Return statements work, and will break out of the current loop/function.
```
i: 1 to 10 step 2 {
    print i
    if (i == 5) {
        return;
    }
}
```

### Structs
```
struct test() {
    a: int -> 2

    fn do(b) {
        return b
    }
}

print test.a
print test.do("test123")
```

## More examples

```
lumi> 3 * -5 --2
-13
```

```
lumi> a -> 2 | a: int -> 2
lumi> print a
2
```

```
lumi> abc: list -> [1, 2, 3, 4, 5]
lumi> abc.sum()
15
```

```
lumi> test -> [14, 23, 58, 7, 18]
lumi> b: int -> test[2]
lumi> print b
58
```

### Script example
```
a: int -> 2
b: int -> 3

fn test(param, another) {
    print param + another
}

test(a, b)
```

### This also works
```
lumi> fn test(a) { print a }
lumi> test("hehexd")
"hehexd"
```

```
lumi> i: 1 to 3 step 1 { print "a" }
"a"
"a"
"a"
```


### Known bugs and/or shortcomings
The return statement inside an IF statement inside a function will not work properly. (yet)

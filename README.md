README will be edited upon as we plow through this adventure.
We shall see where this will bring us : )

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
* Numbers
    ** int: `x: int -> 2`
    ** float: `x: float -> 2.0`
* Strings: `x: str -> "test"`
* Booleans: `x: bool -> true` 
* Functions: `fn test() {}`
* Structs: `struct test() {}`

### Control flow
```
x: list -> ["a", "b", "c"]

if (len(x) == 3) {
    print "Length of x is 3".
} else {
    print "Length if x is not 3."
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
lumi> 1 to 3 step 1 { print "a" }
"a"
"a"
"a"
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

### Known bugs and/or shortcomings
The return statement inside an IF statement inside a function will not work properly. (yet)
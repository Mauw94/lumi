README will be edited upon as we plow through this adventure.
We shall see where this will bring us : )

# A tour


## Variables and assignments
Declare with ```->``` and assign with ```=```
```x -> 2``` works, but if you want to declare a variable with a type you can do ```x: int -> 2```.
Declaring without a type works somewhat as in TypeScript. Internally the object type will be ```none``` (```any``` in TypeScript) and you can then assign any other type to the variable.
After declaring ```x -> 2``` we can still do this ```x = "test123"```.
If we try ```x: int -> 2``` and then ```x = "test"``` a type mismatch error will be thrown.

Declaring a variable without a value also works. When trying to use this variable it will return the default value for the assigned type.
Declaring a variable without type and value does not work.
```x: int```
```print x``` will return ```0```


### Different types
list: ```x: list -> [1, 2, 3, 4, 5]```
int: ```x: int -> 2```
float: ```x: float -> 2.0```
str: ```x: str -> "test"```
bool: ```x: bool -> true``` 




print b[1]

fn test(arr) {
    print arr[2] * 2
}

i: 1 to 5 step 1 {
    print i
}

array: list -> [1,2,3,4]
result: int

result = test(array)
print result

return; breaks current loop/fn

struct test(a, b)

## Examples

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

### Known bugs and/or shortcomings
The return statement inside an IF statement inside a function will not work properly. (yet)

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

## Features?
* Dynamically typed (somewhat, there's some restrictions to keep it clean-ish)
* Everything is an expression
* Built-in functions
* Structs
* Lists `[a, b, c]`
* Dictionaries `{a: b, c: e}`
* For loops `1 to 5 step 1`

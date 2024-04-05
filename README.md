README will be edited upon as we plow through this adventure.
We shall see where this will bring us : )

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
lumi> 1 to 5 every (*2)
[2, 4, 6, 8, 10]
```

### Or use built-in functions such as:
```
lumi> time()
Current time is 2024-03-28 21:14:16
```

## Features?
* Dynamically typed 
* Everything is an expression
* Built-in functions
* Structs
* Lists `[a, b, c]`
* Dictionaries `{a: b, b: c}`
* For loops `1 to 5 step 1`

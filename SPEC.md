# Mist Spec

Mist is a stack machine based programming language, that means that all operations either push, pop or do both to operate with data on the stack

Take note that mist does not currently implement scopes, all "locals" and function definitions are global! 

Take note that writing incorrect code currently leads to undefined behaviour (in execution time)

Take note that bit operations currently do not exist

## Type system
Currently, the language only has a few core types which are not very flexible due to the current implementation, custom types are planed in the future

Types found in the Language are:
- value: values are any kind of data, it's a blanket type, is planed on being obsoleted in the future
- number: signed 64-bit number
- string
- bool (is sometimes implicitly cast from numbers)
- locals (locals function as their own type due to local-set behaviors, this has lead to some weird bugs, needs to get obsoleted in the future)
- sequences (basically blocks of code)

Only numbers can be operated on with the following operators:
- `+`
- `-`
- `*`
- `/`
- `%`
- `>`, `<`, `>=` and `<=`

## Stack machine
Stack machines operate by manipulating the stack by pushing and popping values.

Let's assume we want to add 2 numbers together, first we need to push our 2 numbers on the stack, then we can apply our addition operation.
That would look the following `1 2 +`. `+` will pop the previous 2 values, compute the sum and finally push the result, so `3`.
In Mist, you push values on the stack by just writing them, functions are operators work the same.

Let's look at something slightly more advanced.
```c
// formatted like this for readability

1 2 + // this evaluates to 3
4 8 + // this evaluates to 12
    + // therefore, this is `3 12 +` evaluating to 15
```

### Other operations:
- `.` - duplicates the current value on the stack
```c
5 . * // evaluates to 25
```
- `n @` - duplicates the `n`-th value back in the stack, `n` must be a number. Take note that n starts at 0, `0 @` is equivalent to `.`
```c
2 3 1 @ + // evaluates to 5
```
- `#` - drops the current value on the stack
```c
1 2 # // evaluates to 1
```
- `~` - swaps the last 2 values on the stack
```c
1 2 ~ // evaluates to 2 1
```

## Functions
Functions are defined as follows:
```c
(
    //CODE OF FUNCTION
) $NAME
```
Example:
```c
(. *) $square

5 square // evaluates to 25
```

## "Locals" 
(Locals are currently globals, oops...)
### Setting:
```rust
42 // first we push our desired value on the stack
'fancy // then our "local" name
: // this operation consumes both pushed values and sets the "local" to the value we supplied
```
### Getting:
"Locals" can just be read out by using them as value `'localname`

## if & else
```c
1 1 = // evaluates to true
if // pops one value of the stack (must be a bool or a number)
    "This will always run" // pushes string on stack
else // else's are optional
    "This will never run" // pushes string on stack
end
println // we run println after the if as both branches push a string
```

## loops
Let's look at this example countdown from 10 to 1 (inclusive)
```rust
10 'i : // lets set a variable to i
('i 0 !=) // we will create a condition for the loop to run here, it is a similar syntax to function definition but we dont provide a name
loop // we start out loop here
    'i str println // lets print the step we are at
    'i 1 - 'i : // 'i = 'i - 1 
end
```

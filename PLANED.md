# Planed stuff
## for syntax
(end exclusive)
```rust
0 100 'i for
    'i str println
end
```

## stack block
```c
1337
stack
    1 2 3 4 5 6 7 8 9 10
    push // for extracting a value
end

str println // 10
str println // 1337
```

## copy syntax
```rust
1 2
copy
    + // combines 1 + 2, pushes result on stack
end
// evaluates to `1 2 3`, `1 2` are kept
0 3 'i for
    'i = assert
end
```

## queue syntax
```rust
3 queue // the queue needs to be 3 deep as copy the previous 2 making the original 2 be gone 
    1 1 // initial conditions
    0 40 '_ for // compute 42th fibonacci number (we skip the first 2 as we already "computed" them beforehand)
        1 @ 1 @ + // (seek back twice by 1 => `a b` 1 @ `a b a` 1 @ `a b a b` + -> `a b c`)
    end
    push // take 1 value from the queue and pushes it on the underlying stack
end 
```

## namespaces (scopes)

## custom types
### overload-able operations

## arrays

## less 1-char as keywords

## syntax errors

## limited static checking

## compilation?
### bytecodei?
### native?

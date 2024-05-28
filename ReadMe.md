# COL226 Assignment 6 : An Extended Lambda Calculus
## Poojan C Shah 2022CS11594
An end-to-end implementation : The code written in a text editor goes through
tokenizing (lexer), parsing to form abstract syntax trees (parser), then compiled into opcodes for the target architecture (compiler) and then finally 
run to provide the output (execution using SECD Machine)

## How to run this project ?
To generate the lexer-parser, first run `make init` .  Then write your code
in any text file, say `code.txt`. Generate the output in terminal using 
`make run file=code.txt` 

## Features
Our language supports the following features : 
### Arithmetic and Logical expressions
### Functions as First Class Citizens
### Anonymous Functions
### Recursive Functions
### Function Abstractions and Applications
### Pairs and Projections, Control Flow
### Nested Definitions and Static Scoping

The abstract grammar is given by the following : 
```
e ::=
    | x | n | b
    | e1 + e2 | e1-e2 | e1*e2 
    | e1 and e2 | e1 or e2 | not e'
    | e1 = e2 | e1 > e2
    | if e1 then e2 else e3
    | let x = e1 in e2
    | let rec f = fun x -> e1 in e2
    | fun x -> e'
    | (e1 e2)
    | (e1,e2)
    | first e' | second e'
```

## Test Cases

### tc0 - nested definitions

```
let x = true in 
let y = false in
let z = x and y in
x or y and not (z)

```
```
true
```

### tc1 - scoping in functions

```
let y = 3 in
let f = fun x -> (x+y) in
let y = 14 in
f y
```

```
17
```
Notice how the first occurence of y is bound to a different scope.

### tc2 - Functions as first class citizens

```
let f = fun x -> fun y -> (x*x + y*y) in
let g = f 12 in
g 10
```

```
244
```

### tc3 - variable bindings

```
(fun x -> (x*x + ((fun x -> (x+1)) 2)) (fun x -> (x-1) 5))
```

```
19
```

### tc4 - recursive functions
```
let rec fact = 
    fun n -> 
        if (n = 0) then 1 
        else (n * (fact (n - 1))) 
in (fact 5)
```

```
120
```

### tc5 - pairs and tuples
```
let f = fun x -> (x,(x,x)) in
f (f 1)
```

```
((1,(1,1)),((1,(1,1)),(1,(1,1))))
```

### tc6 - tuples and recursion
```
let rec list  = fun n ->
    if (n = 1) then ((0,1))
    else ( ( (list (n-1)) , n*n))

in (list 10)
```

```
((((((((((0,1),4),9),16),25),36),49),64),81),100)
```

### tc7 - double recursion

```

let rec fact = 
    fun n ->
    if (n = 1) then 1
    else (n * (fact (n-1)))
in
(let rec list = 
    fun n ->
    if (n = 1) then ((0,1))
    else ( ( (list (n-1)) , (fact n)))

in (list 10))
```

```
((((((((((0,1),2),6),24),120),720),5040),40320),362880),3628800)
```
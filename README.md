# Group
- Sarathy Selvam, PID: 730770538
- Lakshin Ganesha, PID: 730757493
- Sushant Potu, PID: 730768373

```erlang
c(chain_server).
chain_server:start().

>>> {add, 10, 5}.
(serv1) add 10 5 = 15

>>> [1, 2, 3].
(serv2) sum of [1,2,3] = 6

>>> {error, "test error"}.
(serv3) Error: "test error"

>>> hello.
(serv3) Not handled: hello

>>> halt.
(serv1) halting
(serv2) halting
(serv3) Unhandled message count: 1
(serv3) halting
ok
```

## Arithmetic Operations on serv1

Addition
Input: {add, 5, 3}.
Expected: (serv1) add 5 3 = 8

Subtraction
Input: {sub, 20, 8}.
Expected: (serv1) sub 20 8 = 12

Multiplication
Input: {mult, 3, 4}.
Expected: (serv1) mult 3 4 = 12

 Division
Input: {div, 20, 4}.
Expected: (serv1) div 20 4 = 5.0

Division by Zero
Input: {div, 10, 0}.
Expected: (serv1) div 10 0 = Error: Division by zero

Negation
Input: {neg, 5}.
Expected: (serv1) neg 5 = -5

Square Root
Input: {sqrt, 16}.
Expected: (serv1) sqrt 16 = 4.0

Negative Square Root
Input: {sqrt, -4}.
Expected: (serv1) sqrt -4 = Error: Negative square root

## List Operations on serv2

Integer List Sum
Input: [1, 2, 3, 4].
Expected: (serv2) sum of [1,2,3,4] = 10

Float List Product
Input: [2.0, 3.0, 4.0].
Expected: (serv2) product of [2.0,3.0,4.0] = 24.0

Mixed Integer List
Input: [5, 10, 15].
Expected: (serv2) sum of [5,10,15] = 30

## Error Handling on serv3

Error Message
Input: {error, "test error"}.
Expected: (serv3) Error: "test error"

Another Error
Input: {error, "connection failed"}.
Expected: (serv3) Error: "connection failed"

## Unhandled Messages on serv3

Unknown Tuple
Input: {unknown, msg}.
Expected: (serv3) Not handled: {unknown,msg}

Random Atom
Input: random_atom.
Expected: (serv3) Not handled: random_atom

Another Atom
Input: hello.
Expected: (serv3) Not handled: hello

## Halt Sequence

Testing Cascade
Input: halt.

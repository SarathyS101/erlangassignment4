# Group
- Sarathy Selvam, PID: 730770538
- Lakshin Ganesha, PID: 730757493
- Sushant Potu, PID: 730768373

```erlang
c(chain_server).
chain_server:start().

>>> {add, 10, 5}.
(serv1) add 10 + 5 = 15

>>> [1, 2, 3].
(serv2) Sum of int list = 6

>>> {error, "test error"}.
(serv3) Error caught: "test error"

>>> hello.
(serv3) Not handled: hello

>>> halt.
(serv1) halting
(serv2) halting
(serv3) halting
(serv3) Unhandled message count: 1
```

## Arithmetic Operations on serv1

- Input: {add, 5, 3}.
- Output: (serv1) Add: 5 + 3 = 8

- Input: sub, 20, 8}. 
- Output: (serv1) Subtract: 20 - 8 = 12

- Input: {mult, 3, 4}.
- Output: (serv1) Multiply: 3 * 4 = 12

- Input: {'div', 20, 4}.
- Output: (serv1) Divide: 20 / 4 = 5.0

- Input: {'div', 10, 0}.
- Output: (serv1) Error: Division by zero

- Input: {neg, 5}.
- Output: (serv1) Negate: 5 = -5
 
- Input:{sqrt, 16}.
- Output: (serv1) Sqrt of 16 = 4.0

- Input: {sqrt, -4}.
- Output: (serv1) Error: Cannot calculate sqrt of negative number

## List Operations on serv2

- Input: [1, 2, 3, 4].
- Output: (serv2) Sum of int list = 10

- Input: [2.0, 3.0, 4.0].
- Output: (serv2) Product of Float list = 24.0

- Input: [5, 10.0, 15].
- Output: (serv2) Sum of mixed = 30

## Error Messages on serv3
 
- Input: {error, "test error"}.
- Output: (serv3) Error caught: "test error"

- Input: {error, "connection failed"}.
- Output: (serv3) Error caught: "connection failed"

### Unhandled Messages on serv3

- Input: {unknown, msg}.
- Output: (serv3) Not handled: {unknown,msg}

- Input: random_atom.
- Output: (serv3) Not handled: random_atom

- Input: hello.
- Output: (serv3) Not handled: hello

## Halt Sequence

- Input: halt.
- Output: (serv1) Halting.
(serv2) Halting.
(serv3) Halting. Total unhandled messages: 3

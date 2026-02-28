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
```

## Arithmetic Operations on serv1

{add, 5, 3}.
(serv1) Add: 5 + 3 = 8

>>> {sub, 20, 8}. 
(serv1) Subtract: 20 - 8 = 12

>>> {mult, 3, 4}.
(serv1) Multiply: 3 * 4 = 12

>>> {'div', 20, 4}.
(serv1) Divide: 20 / 4 = 5.0

>>> {'div', 10, 0}.
(serv1) Error: Division by zero

>>> {neg, 5}.
(serv1) Negate: 5 = -5
 
>>> {sqrt, 16}.
(serv1) Sqrt of 16 = 4.0

>>> {sqrt, -4}.
(serv1) Error: Cannot calculate sqrt of negative number

## List Operations on serv2

>>> [1, 2, 3, 4].
(serv2) Integer list sum = 10

>>> [2.0, 3.0, 4.0].
(serv2) Float list product = 24.0

>>> [5, 10, 15].
(serv2) Integer list sum = 30

 ## Unhandled Messages on serv3
 
>>> {error, "test error"}.
(serv3) Error caught: "test error"

>>> {error, "connection failed"}.
(serv3) Error caught: "connection failed"

>>> {unknown, msg}.
(serv3) Not handled: {unknown,msg}
>>> random_atom.
(serv3) Not handled: random_atom

>>> hello.
(serv3) Not handled: hello

## Halt Sequence

>>> halt.
(serv1) Halting.
(serv2) Halting.
(serv3) Halting. Total unhandled messages: 3

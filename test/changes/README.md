# Scala-AM Incremental Benchmark Suite

This directory contains the Scala-AM Incremental Benchmark suite.
This suite contains programs annotated with changes, originally taken from the main benchmark suite.

To execute these programs in a standard Scheme interpreter, macros can be used.

* To execute the original program: 
```scheme 
(define-syntax <change>
  (syntax-rules ()
    ((<change> x y) x)))
```
* To execute the modified program:
```scheme 
(define-syntax <change>
  (syntax-rules ()
    ((<change> x y) y)))
```
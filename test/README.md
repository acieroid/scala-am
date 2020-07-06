# Scala-AM benchmark suite

This directory contains the Scala-AM benchmark suite. This benchmark suite currently focuses on
the R5RS Scheme language. Benchmarks are also present for an extension of this language with constructs
for concurrency.

This file contains an overview of the structure of the benchmark suite, as well as
information on particular subsets of this benchmark suite.

## ConcurrentScheme
This folder contains benchmark programs for a concurrent Scheme.
Benchmarks are present for different flavours of concurrency:
* actors,
* concurrent threads with locks,
* futures with atomic variables (atoms).

For futures/threads, the folder 'variations' contains several benchmark programs, each of which is written
multiple times using a different number of futures resp. threads. Some of benchmarks for futures
are based on/rewritten from the benchmarks from threads.

### Actors

The following code is in Scheme-with-Actors. No implementation of this scalaam.language
exists, but it can be easily run on Racket provided the following macros:

```scheme
(define log-thread (thread (lambda ()
                             (let loop ()
                               (printf "~a" (thread-receive))
                               (loop)))))

(define-syntax self
  (syntax-id-rules ()
    [self (current-thread)]))
(define-syntax-rule (send act msg arg1 ...)
  (thread-send act (cons 'msg (list arg1 ...))))
(define-syntax bind-lists
  (syntax-rules ()
    [(bind-lists (x y ...) l body ...)
     (let ((x (car l)))
       (bind-lists (y ...) (rest l) body ...))]
    [(bind-lists () l body ...)
     (begin body ...)]))
(define-syntax-rule (actor name (state ...) (msg (arg ...) body ...) ...)
  (lambda (m state ...)
    (log "actor ~a received ~a~n" name m)
    (case (car m)
      ((msg) (bind-lists (arg ...) (cdr m) body ...))
      ...)))
(define-syntax-rule (become act v1 ...)
  (begin
    (log "becoming ~a with ~a~n" act (list v1 ...))
    (act (thread-receive) v1 ...)))
(define-syntax-rule (log fmt v ...)
  (thread-send log-thread (format fmt v ...)))
(define-syntax-rule (terminate)
  (begin
    (log "terminating ~a~n" (current-thread))
    (kill-thread (current-thread))))
(define-syntax-rule (create act v1 ...)
  (begin
    (log "creating ~a with ~a ~n" act (list v1 ...))
    (thread (lambda () (act (thread-receive) v1 ...)))))
```
#### Savina
#### Soter

### Futures
All files in this directory were taken/modified from https://github.com/acieroid/scala-am/tree/modularthrds/thrds/suite .
Date: Fri 29/03/2019

* Benchmarks easily modifiable to use futures instead of theards:
    * matmul, trapr
    * fact (without fact-thrd-ref)
    * atoms, stm (without definition of atomic functions)

* Benchmarks modified to use futures instead of theards:
    * bchain, count, dekker, fact, mcarlo, mceval, minimax, msort, pc, pp, rng, stm, sudoku

* Benchmarks now using lists instead of vectors:
    * bchain, pp, sudoku

* Benchmarks using references (to be replaced by atoms):
    * qsort, tsp, sieve, ringbuf, pps, pp, phild, pc, mg, nbody, life, fact, dekker, count, atoms, abp

* Benchmarks now using atoms instead of references:
    * actors, bchain, dekker, fact, pc, pp, rng, stm

* Benchmarks using locks (to be implemented on top of atoms):
    * abp, actors, atoms, bchain, count, fact, life, pc, phild, pp, tsp, stm
 
Lock prelude:   
```scheme
(define (new-lock) (atom #f))
(define (acquire lock)
  (let try ()
    (if (compare-and-set! lock #f #t)
        #t
        (try))))
(define (release lock)
  (reset! lock #f))
```
#### Variations

### Threads
#### Variations

## R5RS

### AD
All files in this and underlying folders are taken from ftp://cw.vub.ac.be/pub/courses/curriculum/AlgoDat1/programmacode/

Added tests to some files. Tests should normally evaluate to #t.
* prioq.scm should return 'Patrick .

### Gabriel
This directory includes the Gabriel benchmarks, retrieved from http://www.larcenists.org/Twobit/benchmarksAbout.html

Some benchmarks are slightly (or not-so-slightly) modified:
  - added definitions of some Scheme primitives (e.g., assq, member)
  - only run the benchmark for a single iteration, or run on smaller input
  - defines might be desugared to letrecs
  - cond desugared to ifs
  
### Gambit
All files in this and underlying folders are taken from http://github.com/gambit/gambit (2016-11-18).
Source directory: \bench\src

Adapted tests in some files. Tests should normally evaluate to #t.
* browse.scm should evaluate to 1101.

Additional remarks
* cat.scm       =>  Some necessary files are not included in the test directory yet, but the test would fail inevitably because of other reasons.
* slatex.scm    =>  Some necessary files are not included in the test directory yet, but the test would fail inevitably because of other reasons.
* tail.scm      =>  Some necessary files are not included in the test directory yet, but the test would fail inevitably because of other reasons.
* wc.scm        =>  Some necessary files are not included in the test directory yet, but the test would fail inevitably because of other reasons.

Some similar benchmarks were already included in SCALA-AM's test suite and are not included again.
* ack.scm
* boyer.scm
* conform.scm
* cpstak.scm
* dderiv.scm
* divrec.scm
* fib.scm
* nqueens.scm
* takl.scm
* trav2.scm

Some programs are not included for other reasons (non-standard primitives, ...).
* crash.scm
* dynamic.scm
* fail.scm
* fft.scm
* fibpf.scm
* gcbench.scm
* maze.scm
* mbrot.scm
* nucleic.scm
* pi.scm
* pnpoly.scm
* ray.scm
* simplex.scm
* succeed.scm
* sum1.scm
* sumfp.scm
* test.scm
* tfib.scm

### ICP

### KernighanVanwyk

### Rosetta

### SCP1
All files in this and underlying folders are taken from http://soft.vub.ac.be/SCPI/ .

Added/adapted tests to some files. Tests should normally evaluate to #t.

Note that by now new exercises may be present on this website and some older ones may have been deleted.
Also not all exercises have been included in this benchmark suite. TODO Add if necessary.

_SCP1 programs per topic_

**Procedures blocks conditions**
leap-year.scm
third-root.scm

**Recursion vs. iteration**
addition.scm
fast-multiply.scm
multiply.scm
calc-e-and-cos.scm
counter.scm
weird.scm
sim-fast-multiply.scm
draw-umbrella.scm

**Higher-Order Procedures**
print-abc.scm
simpson-integral.scm

**Lists**
add-to-end.scm
append.scm
super-list-merge-n.scm
list-compare-n.scm
grades.scm
compress-measurements.scm
sales-period.scm

**Trees**
count-tree.scm
fringe.scm
unfringe.scm
same-structure.scm
deep-map-combine.scm
apple-tree.scm
organigram.scm
fireworks.scm
university.scm
animal-classification.scm
tree-with-branches.scm
coca-cola.scm
family-budget.scm
circus.scm

**Objects**
flip.scm
flip2.scm
polynome.scm
haha.scm
scoreboard.scm
parking-counter.scm
square-and-rectangle.scm
lightbulb.scm
cashdesk-counter.scm
car-counter.scm
twitter.scm

**Destructive operations**
count-pairs.scm
ring.scm
ring-rotate.scm
find-cycles.scm
ring-copy.scm
josephus-problem.scm
count-pairs2.scm
flatten.scm
ring-squares.scm
slide-in.scm
dedouble.scm
insert.scm
all-but-interval.scm
merge.scm

### SCP1-compressed
All files in this and underlying folders are taken from http://soft.vub.ac.be/SCPI/ .

Added/adapted tests to some files. Tests should normally evaluate to #t.

### SETL

### Sigscheme
All files in this and underlying folders are taken from https://github.com/uim/sigscheme/tree/master/bench (2017-04-11).

Added/adapted tests to some files. Tests should normally evaluate to #t.
* takr.scm should return 7.
* loop.scm should evaluate to 8000.
* let-loop.scm should return 20000.
* arithint.scm should evaluate to 20001.

Some similar benchmarks were already included in SCALA-AM's test suite and are not included again.
* fib.scm
* cpstak.scm

### WeiChenRompf2019
All files in this and underlying folders are taken from 'Artifact for Staged Abstract Interpreters (OOPSLA 2019)'. <br>
Source: https://zenodo.org/record/3374032#.XahuIi97HYo <br>
Following benchmarks were already present (in original or modified form) and have been omitted: 
<ul>
  <li>blur.scm</li>
  <li>church.sch</li>
  <li>church_backup.sch</li>
  <li>sat.scm</li>
  <li>toplas98/matrix.scm</li>
</ul> 
Some benchmarks with identical names were kept since the file contents were
sufficiently different. Where needed, benchmarks were modified to make them work (e.g., some benchmarks
did not use valid Scheme syntax such as square brackets or had missing parentheses).

#### the-little-schemer
Code from the book The Little Schemer.

[The Little Schemer](http://mitpress.mit.edu/books/little-schemer)

#### toplas98
Benchmarks from _Polymorphic splitting: an effective polyvariant flow analysis_, Andrew K. Wright and Suresh Jagannathan.

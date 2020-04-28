Scala-AM: A Framework for Static Analysis of Dynamic Languages

# Goal
The goal of this artefact is to experiment with abstract machines and language
semantics. Currently, the artefact's implementation is focused towards experiments with modular analyses.
Additionally, semantics for R5RS Scheme are present.

<!-- https://github.com/badges/shields -->
![Latest build](https://github.com/acieroid/scala-am/workflows/Scala-AM%20tests%20on%20action/badge.svg)
![Nightly tests](https://github.com/acieroid/scala-am/workflows/Daily%20Scala-AM%20tests/badge.svg)

# Usage
The Scala-AM framework can be used in several ways.

## Running the test suite
The test suite of Scala-AM can be run using `sbt test`.
#+BEGIN_SRC shell
$ sbt
> scalaam/test
#+END_SRC

This repository is monitored by a CI-system. Upon every push to this repository, the test suite is run on a specific
part of the test suite. In addition, the full test suite is run over night.

## Using the JavaScript visual front-end
The framework includes a JavaScript front-end that can be used to visualise a MODF analysis in the browser.
To run this visualisation, open the file `scalaam.html` with your browser. The visualisation provides information with
regard to the work list (coloured in light blue) and currently analysed component (coloured in dark blue).
Stepping through the visualisation can be done using the space bar.

If you need to compile the code first, run the command `fastOptJS` within your sbt repl.

## Analysing a program using command line
The Scala-AM framework is built in a modular style. To run a modular analysis, you need to compose the
implementation of a specific machine and an abstract domain.

To analyze a specific program, an instance of the MODF analysis class must be created. The constructor of
this class takes a parsed version of the program to be analysed, which can be obtained as follows:
```scala
val text = scalaam.io.Reader.loadFile(path-to-file)
val prog = scalaam.language.scheme.Schemeparser.parse(text)
```
Additional preprocessing steps are performed by the modular analysis itself and hence must not be performed manually.

Now, the MODF instance can be created. For example, to analyze `prog` using a big-step MODF analysis
with full argument sensitivity and a type domain:
```scala
val machine = new ModAnalysis(prog) with BigStepSemantics
                                    with StandardSchemeModFSemantics
                                    with FullArgumentSensitivity
                                    with TypePropagationDomain
machine.analyze()
```
The `analyze` function can take an optional parameter to specify a timeout, which is obtained from a Java Duration
like `Timeout.start(duration)`. The analysis will stop approximately when the timeout has been reached, that is,
the analysis may be run a bit longer than is specified by the timeout, but never shorter unless it finishes.

Currently, no explicit result is returned by the analysis. Rather, information can be retrieved by fields of the machine,
such as the final store

# References and Relevant publications
The original idea behind Scala-AM comes from the [Abstracting Abstract Machines](http://matt.might.net/papers/vanhorn2010abstract.pdf)
literature. Since then, the work of [Effect-Driven Flow Analysis](https://doi.org/10.1007/978-3-030-11245-5_12) has been integrated.

The Scala-AM framework is described in the following publication:
  * Scala-AM: A Modular Static Analysis Framework. SCAM 2016. [pdf](http://soft.vub.ac.be/Publications/2016/vub-soft-tr-16-07.pdf), [doi](https://zenodo.org/badge/latestdoi/23603/acieroid/scala-am).
  * Building a Modular Static Analysis Framework in Scala. Scala@SPLASH 2016. [pdf](http://soft.vub.ac.be/Publications/2016/vub-soft-tr-16-13.pdf), [doi](http://doi.acm.org/10.1145/2998392.3001579).

Scala-AM has been used for evaluating static analysis approaches in the
following publications:
  * Garbage-Free Abstract Interpretation through Abstract Reference
    Counting. ECOOP 2019. [pdf](http://drops.dagstuhl.de/opus/volltexte/2019/10784/).
  * A general method for rendering static analyses for diverse concurrency
    models modular. Journal of Systems and Software, Volume 149. 2019. [pdf](https://soft.vub.ac.be/~qstieven/fwo-proposal-jss.pdf), [doi](https://doi.org/10.1016/j.jss.2018.10.001).
  * Mailbox Abstractions for Static Analysis of Actor Programs. ECOOP 2017. [pdf](http://soft.vub.ac.be/~qstieven/ecoop2017/ecoop2017actors-final.pdf), [doi](https://doi.org/10.4230/LIPIcs.ECOOP.2017.25).
  * Employing Run-time Static Analysis to Improve Concolic
    Execution. BENEVOL 2017. [pdf](http://ceur-ws.org/Vol-2047/BENEVOL_2017_paper_7.pdf).
  * Incrementalizing Abstract Interpretation. BENEVOL 2017. [pdf](http://ceur-ws.org/Vol-2047/BENEVOL_2017_paper_9.pdf).
  * Static taint analysis of event-driven scheme programs. ELS 2017. [pdf](http://soft.vub.ac.be/Publications/2017/vub-soft-tr-17-02.pdf).
  * Improving trace-based JIT optimisation using whole-program
    information. VMIL@SPLASH 2016. [pdf](http://soft.vub.ac.be/Publications/2016/vub-soft-tr-16-09.pdf), [doi](http://doi.acm.org/10.1145/2998415.2998418).
  * STRAF: A Scala Framework for Experiments in Trace-Based JIT
    Compilation. GTTSE 2015. [pdf](http://soft.vub.ac.be/Publications/2017/vub-soft-tr-17-09.pdf), [doi](https://doi.org/10.1007/978-3-319-60074-1\_10).

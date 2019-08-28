package scalaam.language.lambda

object LambdaAnalysis {
  def main(args: Array[String]) = {
    import scalaam.machine._
    import scalaam.core._
    import scalaam.graph._
    import scalaam.lattice._

    /* To run an analysis, we instanciate a number of components */

    /* We need a representation of memory addresses. We use a simple abstraction
     * that represent a memory for a variable by the name of the variable. As a
     * result, all variables that have the same name will be allocated at the
     * same memory address and their value will be joined. This is a
     * context-insensitive analysis. */

    /* We don't use timestamps, so we use ZeroCFA which is an empty timestamp. */
    val timestamp = ZeroCFA[LambdaExp]()
    val address = NameAddress
    /* We use our set lattice implementation defined in LambdaLattice.scala */
    val lattice = LambdaSetLattice[address.A, Concrete.B]()
    /* We use the semantics we defined for lambda-calculus*/
    val sem = LambdaSemantics[lattice.L, address.A, timestamp.T, LambdaExp](address.Alloc[timestamp.T, LambdaExp])
    /* We use the AAM machine asbtraction, pre-defined in Scala-AM */
    val machine = new GAAM[LambdaExp, address.A, lattice.L, timestamp.T](sem)
    /* This is the kind of graph we want to generate. In this case, we generate a
     * plain graph in the DOT format. Other possibilities include graph
     * representations that only store the information needed for a specific
     * client analysis, e.g. one that only stores error states in order to
     * statically analyze the program to locate possible errors that can be
     * reached. */
    val graph  = new DotGraph[machine.State, machine.Transition]
    /* We can then run our analysis on an example program */
    val church = """(letrec ((plus (lambda (n1 n2)
                 (lambda (f) (lambda (x) ((n1 f) ((n2 f) x))))))
         (mult (lambda (n1 n2)
                 (lambda (f) (n2 (n1 f)))))
         (pred (lambda (n)
                 (lambda (f)
                   (lambda (x)
                     (((n (lambda (g) (lambda (h) (h (g f)))))
                       (lambda (ignored) x))
                      (lambda (id) id))))))
         (sub (lambda (n1 n2)
                ((n2 pred) n1)))
         (church0 (lambda (f) (lambda (x) x)))
         (church1 (lambda (f) (lambda (x) (f x))))
         (church2 (lambda (f) (lambda (x) (f (f x)))))
         (church3 (lambda (f) (lambda (x) (f (f (f x))))))
         (church0? (lambda (n)
                     ((n (lambda (x) #f)) #t)))
         (church=? (lambda (n1 n2)
                     (if (church0? n1)
                       (church0? n2)
                       (if (church0? n2)
                         #f
                         (church=? (sub n1 church1) (sub n2 church1)))))))
  ;; multiplication distributes over addition
  (church=? (mult church2 (plus church1 church3))
            (plus (mult church2 church1) (mult church2 church3))))
"""
/* (let ((_plus0 (lambda (_n11 _n22) 
                (lambda (_f3) 
                  (lambda (_x4) 
                    (let ((_p38 (_n11 _f3))) 
                      (let ((_p39 (_n22 _f3))) 
                        (let ((_p40 (_p39 _x4))) 
                          (_p38 _p40))))))))) 
  (let ((_mult5 (lambda (_n16 _n27) 
                  (lambda (_f8) 
                    (let ((_p41 (_n16 _f8))) 
                      (_n27 _p41)))))) 
    (let ((_pred9 (lambda (_n10) 
                    (lambda (_f11) 
                      (lambda (_x12) 
                        (let ((_p43 (_n10 (lambda (_g13)
                                            (lambda (_h14) 
                                              (let ((_p42 (_g13 _f11))) 
                                                (_h14 _p42))))))) 
                          (let ((_p44 (_p43 (lambda (_ignored15) _x12)))) 
                            (_p44 (lambda (_id16) _id16))))))))) 
      (let ((_sub17 (lambda (_n118 _n219) 
                      (let ((_p45 (_n219 _pred9))) 
                        (_p45 _n118))))) 
        (let ((_church020 (lambda (_f21) 
                            (lambda (_x22) _x22)))) 
          (let ((_church123 (lambda (_f24) 
                              (lambda (_x25) (_f24 _x25))))) 
            (let ((_church226 (lambda (_f27) 
                                (lambda (_x28) 
                                  (let ((_p46 (_f27 _x28))) 
                                    (_f27 _p46)))))) 
              (let ((_church329 (lambda (_f30) 
                                  (lambda (_x31) 
                                    (let ((_p47 (_f30 _x31))) 
                                      (let ((_p48 (_f30 _p47))) 
                                        (_f30 _p48))))))) 
                (let ((_church0?32 (lambda (_n33) 
                                     (let ((_p49 (_n33 (lambda (_x34) #f)))) 
                                       (_p49 #t))))) 
                  (letrec ((_church=?35 (lambda (_n136 _n237) 
                                          (let ((_p50 (_church0?32 _n136))) 
                                            (if _p50 
                                                (_church0?32 _n237) 
                                                (let ((_p51 (_church0?32 _n237))) 
                                                  (if _p51 
                                                      #f 
                                                      (let ((_p52 (_sub17 _n136 _church123))) 
                                                        (let ((_p53 (_sub17 _n237 _church123))) 
                                                          (_church=?35 _p52 _p53)))))))))) 
                    (let ((_p54 (_plus0 _church123 _church329))) 
                      (let ((_p55 (_mult5 _church226 _p54))) 
                        (let ((_p56 (_mult5 _church226 _church123))) 
                          (let ((_p57 (_mult5 _church226 _church329))) 
                            (let ((_p58 (_plus0 _p56 _p57))) 
                              (_church=?35 _p55 _p58))))))))))))))))
 """*/
    val result = machine.run[graph.G](LambdaParser.parse(church), Timeout.seconds(20))
    /* We can then output our graph to a file to inspect it. */
    result.toFile("foo.dot")
    import Graph.GraphOps
    println(s"Total number of nodes: ${result.nodes}")
  }
}

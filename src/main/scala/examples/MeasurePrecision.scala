import scalaz._
import scalaz.Scalaz._

/**
 * This analysis computes, for every function call, the values to which the operator can point to
 */

/* TODO: rely on actions
class CallSitePrecisionAnalysis[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
    extends BaseAnalysis[Map[Exp, Abs], Exp, Abs, Addr, Time] {
  type FlowSet = Map[Exp, Abs]
  object FlowSet {
    def apply(): FlowSet = Map[Exp, Abs]().withDefaultValue(abs.bottom)
  }
  /** Initial value is the empty flow set */
  def init = FlowSet()
  /** Join just merges the information */
  def join(x: FlowSet, y: FlowSet) = x |+| y
  /** When an error occurs, nothing happens to the flowset */
  def error(err: SemanticError, current: FlowSet) = current
}
 */

/**
 * This analysis computes, for every variable reference, the values to which the
 * variable can point to.  It is independent of the expressions used, and relies
 * on the ref function to extract variable references and the values a variable
 * points to.
 */
class VarRefPrecisionAnalysis[Exp : Expression, Abs : JoinLattice, Addr : Address, Time : Timestamp]
  (ref: (Exp, Environment[Addr], Store[Addr, Abs]) => Option[(String, Position, Abs)])
    extends Analysis[Map[(String, Position), Abs], Exp, Abs, Addr, Time] {
  // val abs = implicitly[JoinLattice[Abs]]
  type FlowSet = Map[(String, Position), Abs]
  object FlowSet {
    def apply(): FlowSet = Map[(String, Position), Abs]().withDefaultValue(JoinLattice[Abs].bottom)
  }
  /** Initial value is the empty set */
  def init = FlowSet()
  /** Join just merges the information */
  def join(x: FlowSet, y: FlowSet) = x |+| y
  /** When an error occurs, nothing happens to the flowset */
  def error(err: SemanticError, current: FlowSet) = current
  /** No variables can be referenced when a continuation is being popped */
  def stepKont(v: Abs, frame: Frame, store: Store[Addr, Abs], t: Time, current: FlowSet) = current
  /** Variables can be referenced when an expression is evaluated, and we rely on ref to tell us when it is the case */
  def stepEval(e: Exp, env: Environment[Addr], store: Store[Addr, Abs], t: Time, current: FlowSet) =
    ref(e, env, store) match {
      case Some((name, pos, v)) => current + ((name, pos) -> (JoinLattice[Abs].join(current((name, pos)), v)))
      case None => current
    }
}

object MeasurePrecision {
  def main(args: Array[String]) {
    val program = Util.fileContent("foo.scm").get
    /* Changing the bound to less than 100 decrease precision */
    val lattice = new ConcreteLattice(false) // new BoundedIntLattice(1000, false)
    implicit val isLattice = lattice.isSchemeLattice
    val address = ClassicalAddress
    val timestamp = ZeroCFA
    implicit val isTimestamp = timestamp.isTimestamp
    /* TODO: we require unoptimized semantics because otherwise we'll miss some stuff. This should be fixed in the analysis */
    val sem = new BaseSchemeSemantics[lattice.L, address.A, timestamp.T](new SchemePrimitives[address.A, lattice.L])
    val machine = new AAM[SchemeExp, lattice.L, address.A, timestamp.T]
    //new AAMAACP4F[SchemeExp, lattice.L, address.A, timestamp.T](P4FKAlloc)
    val analysis = new VarRefPrecisionAnalysis[SchemeExp, lattice.L, address.A, timestamp.T]((e, env, store) => e match {
      case SchemeVar(variable) => env.lookup(variable.name).map(a => {
        (variable.name, variable.pos, store.lookupBot(a))
      })
      case _ => None
    })
    machine.analyze(sem.parse(program), sem, analysis, None) match {
      case Some(res) => res.toList.sortWith(_._1._2 < _._1._2).foreach({ case ((n, p), v) =>
        println(s"$n ($p): $v")
      })
      case None =>
        println("no output")
    }
    /*
    val output = machine.eval(sem.parse(program), sem, true, None)
    val store = output.joinedStore
    println(s"Number of states: ${output.numberOfStates}")
    // output.toDotFile("foo.dot")
    // println("Final store:")
    // println(store)
    val cardinalities: Map[address.A, Cardinality] = store.cardinalities()
    val (inf: Int, unique: Int, others: List[Int]) = cardinalities.values.foldLeft((0, 0, List[Int]()))((acc, v) => v match {
      case CardinalityInf => (acc._1 + 1, acc._2, acc._3)
      case CardinalityNumber(1) => (acc._1, acc._2 + 1, acc._3)
      case CardinalityNumber(n) => (acc._1, acc._2, n :: acc._3)
    })
    println(s"Infinite cardinalities: $inf")
    println(s"Unique cardinalities: $unique")
    if (others.isEmpty) {
      println("No others")
    } else {
      val sum = others.foldLeft(0)(_ + _)
      val avg = sum.toDouble / others.length
      val median = others.sorted.apply(others.length / 2)
      val max = others.sorted.apply(others.length - 1)
      println(s"Sum of others: $sum")
      println(s"Avg of others: $avg")
      println(s"Median of others: $median")
      println(s"Max of others: $max")
    }
     */
    }
}

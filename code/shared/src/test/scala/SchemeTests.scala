import org.scalatest._
import org.scalatest.prop._

import scalaam.core._
import scalaam.language.scheme._
import scalaam.graph._
import Graph._

abstract class SchemeTests[A <: Address, V, T, C](
  implicit val timestamp: Timestamp[T, C],
  implicit val lat: SchemeLattice[V, SchemeExp, A])
    extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val sem: Semantics[SchemeExp, A, V, T, C]
  val machine: MachineAbstraction[SchemeExp, A, V, T, C]
  val graph = ReachableStatesConditionGraph[machine.State, machine.Transition](n => n.metadata.find("halted") == Some(GraphMetadataBool(true)))

  def checkResult(program: SchemeExp, answer: V, timeout: Timeout.T = Timeout.Infinity) = {
    val result = machine.run[graph.G](program, timeout)
    if (timeout.reached) {
      cancel(s"time out")
    } else {
      val resultVals = result.findNodes(n => n.metadata.find("type") == Some(GraphMetadataString("kont"))).flatMap({ n => n.metadata.find("value") match {
        case Some(GraphMetadataValue(v : V @unchecked)) => Set(v)
        case _ => Set[V]()
      }})
      assert(!resultVals.find(v => lat.subsumes(v, answer)).isEmpty)
    }
  }
  def check(table: TableFor2[String, V]) =
    forAll (table) { (program: String, answer: V) =>
      checkResult(SchemeParser.parse(program), answer)
    }
}

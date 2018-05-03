package scalaam.machine

import scalaam.lattice.Lattice
import scalaam.language.Semantics
import scalaam.graph.{Graph, GraphElement}
import scalaam.core._

/**
 * The interface of the machine abstraction itself.
 * A machine abstraction is parameterized by:
 *   - A type of expressions Exp
 *   - A type of values V
 *   - A type of timestamp T
 *   - A type of context for the timestamp C
 */
trait MachineAbstraction[Exp, V, A <: Address, T, C] {
  implicit val timestamp: Timestamp[T, C]
  implicit val lattice: Lattice[V]

  /** The semantics used */
  val sem: Semantics[Exp, V, A, T, C]

  /** The name of the abstract machine */
  val name: String

  /** The states explored by the machine.
   States can be converted to graph nodes */
  type State <: GraphElement

  /** The transitions explored by the machine
      Transitions can be converted to graph edges */
  type Transition <: GraphElement

  /**
   * Runs the machine abstraction with its semantics on a program.
   *   - @param program is the program to run
   *   - @param graph is the initial graph, on which nodes and edges corresponding respectively to explored states and transitions are added
   *   - @param timeout is a timeout after which the run is stopped
   * When the timeout is exceeded, the run stops and the current graph is returned, but it is not safe to conclude anything from this graph.
   */
  def run(program: Exp, graph: Graph[State, Transition], timeout: Timeout.T): Graph[State, Transition]
}

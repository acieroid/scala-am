package modular.scheme

import core.Annotations.toCheck
import scalaam.core._
import scalaam.graph.{Colors, GraphElement, GraphMetadataBool, GraphMetadataMap, GraphMetadataString, GraphMetadataValue}
import scalaam.language.scheme._
import scalaam.modular._
import scalaam.util.Show

/** MODF analysis using an AAM intra-component analysis. */
abstract class SchemeSmallStepModFAnalysis(program: SchemeExp)
  extends ModAnalysis[SchemeExp](program) with GlobalStore[SchemeExp] with ReturnResult[SchemeExp] {
  // local addresses are simply made out of lexical information
  trait LocalAddr extends Address
  case class VarAddr(id: Identifier)          extends LocalAddr { def printable = true  }
  case class PtrAddr[E <: Expression](exp: E) extends LocalAddr { def printable = false }
  case class PrmAddr(name: String)            extends LocalAddr { def printable = false }
  // abstract values come from a Scala-AM Scheme lattice (a type lattice)
  implicit val lattice: SchemeLattice[Value, SchemeExp, Addr]
  // the 'result' of a component is just the return value of the function call
  type Result = Value
  lazy val emptyResult: Value = lattice.bottom
  // Some glue code to Scala-AM to reuse the primitives and environment
  // not actually used, but required by the interface of SchemeSemantics
  implicit case object TimestampAdapter extends Timestamp[IntraAnalysis,Unit] {
    def initial(s: String)       = throw new Exception("Operation not allowed!")
    def tick(cmp: IntraAnalysis) = throw new Exception("Operation not allowed!")
  }
  // The AllocAdapter makes sure the right dependencies are registered upon address allocation.
  case object AllocAdapter extends Allocator[Addr, IntraAnalysis, Unit] {
    def variable(id: Identifier, intra: IntraAnalysis): Addr         = intra.allocAddr(VarAddr(id))
    def pointer[E <: Expression](exp: E, intra: IntraAnalysis): Addr = intra.allocAddr(PtrAddr(exp))
    def primitive(name: String): Addr                                = GlobalAddr(PrmAddr(name))
  }
  lazy val schemeSemantics = new BaseSchemeSemantics[Addr, Value, IntraAnalysis, Unit](AllocAdapter)
  // setup initial environment and install the primitives in the global store
  def initialEnv: Environment[Addr] = Environment.initial(schemeSemantics.initialEnv)
  schemeSemantics.initialStore.foreach { case (a,v) => store = store + (a -> v) }
  // in ModF, components are function calls in some context
  trait IntraComponent {
    def env: Environment[Addr]
  }
  case object MainComponent extends IntraComponent {
    val env: Environment[Addr] = initialEnv
    override def toString = "main"
  }
  case class CallComponent(lambda: SchemeLambda, env: Environment[Addr], nam: Option[String], ctx: Context) extends IntraComponent {
    override def toString: String = nam match {
      case None => s"anonymous@${lambda.pos} [${ctx.toString}]"
      case Some(name) => s"$name [${ctx.toString}]"
    }
  }

  lazy val initialComponent: IntraComponent = MainComponent
  // this abstract class is parameterized by the choice of Context and allocation strategy of Contexts
  type Context
  def allocCtx(lambda: SchemeLambda, env: Environment[Addr], args: List[Value]): Context
  // defining the intra-analysis
  override def intraAnalysis(cmp: IntraComponent) = new IntraAnalysis(cmp)
  // TODO Perhaps mix in AAMUtil instead of copying the useful bits (but alleviates the need for timestamps).
  class IntraAnalysis(component: IntraComponent) extends super.IntraAnalysis(component) with GlobalStoreIntra with ReturnResultIntra {
    trait Control extends SmartHash
    case class ControlEval(exp: SchemeExp, env: Environment[Addr]) extends Control {
      override def toString = s"ev($exp)"
    }
    case class ControlKont(v: Value) extends Control {
      override def toString = s"ko($v)"
    }
    case class ControlError(err: Error) extends Control {
      override def toString = s"err($err)"
    }
    trait KAddr extends Address with SmartHash {
      def printable = true
      def primitive = false
    }
    case class KontAddr(exp: SchemeExp) extends KAddr { override def toString = s"Kont(${exp.toString.take(10)})" }
    case object HaltKontAddr extends KAddr { override def toString = "Halt" }
    case class Kont(f: Frame, next: KAddr) extends SmartHash
    implicit val kontShow: Show[Kont] = new Show[Kont] {
      def show(k: Kont) = "kont($f)"
    }
    type KStore = Store[KAddr, Set[Kont]]
    val sem = new BaseSchemeSemantics[Addr, Value, IntraAnalysis, Unit](AllocAdapter)
    val Action = sem.Action

    // State in the small-step semantics.
    case class State(control: Control, kstore: KStore, a: KAddr, ctx: IntraAnalysis) extends GraphElement with SmartHash {
      override def toString: String = control.toString

      override def label = toString
      override def color =
        if (halted) {
          Colors.Yellow
        } else {
          control match {
            case _: ControlEval  => Colors.Green
            case _: ControlKont  => Colors.Pink
            case _: ControlError => Colors.Red
          }
        }
      override def metadata =
        GraphMetadataMap(
          Map(
            "halted" -> GraphMetadataBool(halted),
            "type" -> (control match {
              case _: ControlEval  => GraphMetadataString("eval")
              case _: ControlKont  => GraphMetadataString("kont")
              case _: ControlError => GraphMetadataString("error")
            })
          ) ++ (control match {
            case ControlKont(v) => Map("value" -> GraphMetadataValue[Value](v))
            case _              => Map()
          })
        )

      /**
       * Checks if the current state is a final state. It is the case if it
       * reached the end of the computation, or an error
       */
      def halted: Boolean = control match {
        case _: ControlEval  => false
        case _: ControlKont  => a == HaltKontAddr
        case _: ControlError => true
      }

      def finished = control match {
        case _: ControlKont => a == HaltKontAddr
        case _              => false
      }

      /**
       * Integrates a set of actions (returned by the semantics, see
       * Semantics.scala), in order to generate a set of states that succeeds this
       * one.
       */
      private def integrate(a: KAddr, actions: Set[Action.A]): Set[State] =
        actions.flatMap({
          /* When a value is reached, we go to a continuation state */
          case Action.Value(v, _) =>
            Set(State(ControlKont(v), kstore, a, ctx))
          /* When a continuation needs to be pushed, push it in the continuation store */
          case Action.Push(frame, e, env, _) => {
            val next = KontAddr(e)
            Set(
              State(
                ControlEval(e, env),
                kstore.extend(next, Set(Kont(frame, a))),
                next,
                ctx
              )
            )
          }
          /* When a value needs to be evaluated, we go to an eval state */
          case Action.Eval(e, env, _) =>
            Set(State(ControlEval(e, env), kstore, a, ctx))
          /* When a function is stepped, we go to a KONT state since we DO NOT step into the function. */
          case Action.StepIn(_, (lam@SchemeLambda(args, _, _), _), _, env, _) => // env already contains bindings for the parameters of the function.
            @toCheck val context = allocCtx(lam, env, args.map(arg => store(env.lookup(arg.name).get))) // TODO Is this correct?
            val component = CallComponent(lam, env, None, context)
            val result = call(component)
            Set(State(ControlKont(result), kstore, a, ctx))
          case Action.StepIn(_, clo, _, _, _) => throw new Exception(s"Illegal StepIn: expected a function in closure: $clo.")
          /* When an error is reached, we go to an error state */
          case Action.Err(err) =>
            Set(State(ControlError(err), kstore, a, ctx))
        })

      /**
       * Computes the set of states that follow the current state
       */
      def step(): Set[State] = control match {
        /** In a eval state, call the semantic's evaluation method */
        case ControlEval(e, env) => integrate(a, sem.stepEval(e, env, StoreAdapter, ctx))
        /** In a continuation state, call the semantics' continuation method */
        case ControlKont(v) =>
          kstore.lookup(a) match {
            case Some(konts) =>
              konts.flatMap({
                case Kont(frame, next) => integrate(next, sem.stepKont(v, frame, StoreAdapter, ctx))
              })
            case None => Set()
          }
        /** In an error state, the state is not able to make a step */
        case ControlError(_) => Set()
      }
    }

    // analysis entry point
    def analyze(): Unit = {
      val state: State =  component match {
        case MainComponent => State(ControlEval(program, component.env), Store.empty[KAddr, Set[Kont]], HaltKontAddr, this)
        // TODO: cleanup explicit begin (see SchemeSemantics.evalCall).
        case CallComponent(SchemeLambda(_,body,pos),_,_,_) => State(ControlEval(SchemeBegin(body, pos), component.env), Store.empty[KAddr, Set[Kont]], HaltKontAddr, this)
      }
      var work: List[State] = List[State](state)
      var visited: List[State] = List[State]()
      var result: Value = emptyResult

      while(work.nonEmpty) {
        val state = work.head
        work = work.tail
        if (state.finished) {
          result = lattice.join(result, state.control.asInstanceOf[ControlKont].v)
          visited = state :: visited
        }
        else if (!visited.contains(state) && !state.halted) {
          val successors = state.step()
          visited = state :: visited
          work = successors.toList ::: work
        }
      }

      updateResult(result)
    }

    // primitives glue code
    // TODO[maybe]: while this should be sound, it might be more precise to not immediately write every value update to the global store ...
    case object StoreAdapter extends Store[Addr,Value] {
      def lookup(a: Addr)                       = Some(readAddr(a))
      def extend(a: Addr, v: Value)             = { writeAddr(a,v) ; this }
      // all the other operations should not be used by the primitives ...
      def content                               = throw new Exception("Operation not allowed!")
      def keys                                  = throw new Exception("Operation not allowed!")
      def restrictTo(a: Set[Addr])              = throw new Exception("Operation not allowed!")
      def forall(p: ((Addr, Value)) => Boolean) = throw new Exception("Operation not allowed!")
      def join(that: Store[Addr, Value])        = throw new Exception("Operation not allowed!")
      def subsumes(that: Store[Addr, Value])    = throw new Exception("Operation not allowed!")
    }
  }
}

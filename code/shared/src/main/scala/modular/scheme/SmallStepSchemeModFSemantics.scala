package scalaam.modular.scheme

/*

import scalaam.core._
import scalaam.graph.{Color, Colors, GraphElement, GraphMetadataBool, GraphMetadataMap, GraphMetadataString, GraphMetadataValue}
import scalaam.language.scheme._
import scalaam.util.Show

import scala.concurrent.TimeoutException

// MODF analysis using an AAM intra-component analysis.
trait SmallStepSchemeModFSemantics extends SchemeModFSemantics {
  // defining the intraAnalysis
  // TODO Perhaps mix in AAMUtil instead of copying the useful bits (but alleviates the need for timestamps).
  override def intraAnalysis(cmp: IntraComponent) = new IntraAnalysis(cmp)
  class IntraAnalysis(component: IntraComponent) extends super.IntraAnalysis(component) with SchemeModFSemanticsIntra {
    trait Control extends SmartHash
    case class ControlEval(exp: SchemeExp, env: Environment[Addr]) extends Control {
      override def toString = s"ev($exp)"
    }
    case class ControlKont(v: Value) extends Control {
      override def toString = s"ko($v)"
    }
    case class ControlCall(fval: Value, fexp: SchemeExp, args: List[(Value,SchemeExp)]) extends Control {
      override def toString = s"${fval}(${args.map(_._1)})"
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
    val Action = schemeSemantics.Action

    // State in the small-step semantics.
    case class State(control: Control, kstore: KStore, a: KAddr, ctx: IntraAnalysis) extends GraphElement with SmartHash {
      override def toString: String = control.toString

      override def label = toString
      override def color: Color = control match {
        case ControlEval(_, _)        => Colors.Blue
        case ControlCall(_, _, _)     => Colors.White
        case ControlKont(_) if halted => Colors.Grass
        case ControlKont(_)           => Colors.Yellow
        case ControlError(_)          => Colors.Pink
      }
      override def metadata =
        GraphMetadataMap(
          Map(
            "halted" -> GraphMetadataBool(halted),
            "type" -> (control match {
              case _: ControlEval  => GraphMetadataString("eval")
              case _: ControlKont  => GraphMetadataString("kont")
              case _: ControlError => GraphMetadataString("error")
              case _: ControlCall  => GraphMetadataString("call")
            })
          ) ++ (control match {
            case ControlKont(v) => Map("value" -> GraphMetadataValue[Value](v))
            case _              => Map()
          })
        )

      def halted: Boolean = control match {
        case _: ControlEval  => false
        case _: ControlCall  => false
        case _: ControlKont  => a == HaltKontAddr
        case _: ControlError => true
      }

      def finished = control match {
        case _: ControlKont => a == HaltKontAddr
        case _              => false
      }

      private def integrate(a: KAddr, actions: Set[Action.A]): Set[State] =
        actions.flatMap({
          case Action.Value(v, _) =>
            Set(State(ControlKont(v), kstore, a, ctx))
          case Action.Push(frame, e, env, _) => {
            val next = KontAddr(e)
            Set(State(ControlEval(e, env),kstore.extend(next, Set(Kont(frame, a))),next,ctx))
          }
          case Action.Eval(e, env, _) => Set(State(ControlEval(e, env), kstore, a, ctx))
          case Action.Call(fval,fexp,args,_) => Set(State(ControlCall(fval,fexp,args),kstore,a,ctx))
          case Action.StepIn(_, _, _, _, _) => throw new Exception("Illegal state: MODF should not encounter a StepIn action.")
          case Action.Err(err) => Set(State(ControlError(err), kstore, a, ctx))
        })


      def step(): Set[State] = control match {
        case ControlEval(e, env) => integrate(a, schemeSemantics.stepEval(e, env, StoreAdapter, ctx))
        case ControlKont(v) =>
          kstore.lookup(a) match {
            case Some(konts) =>
              konts.flatMap({
                case Kont(frame, next) => integrate(next, schemeSemantics.stepKont(v, frame, StoreAdapter, ctx))
              })
            case None => Set()
          }
        case ControlCall(fval,fexp,args) =>
          if(args.forall(_._1 != lattice.bottom)) {
            val fromClosures = applyClosures(fexp,fval,args.map(_._1))
            val fromPrimitives = applyPrimitives(fexp,fval,args)
            integrate(a, fromClosures ++ fromPrimitives)
          } else {
            Set(State(ControlKont(lattice.bottom),kstore,a,ctx))
          }
        case ControlError(_) => Set()
      }
    }

    private def applyClosures(fexp: SchemeExp, fval: Value, args: List[Value]): Set[Action.A] =
      lattice.getClosures(fval).map {
          case ((lambda@SchemeLambda(pars,_,_), env1), nam) if pars.length == args.length =>
            val context = allocCtx(lambda,env1,args)
            val component = CallComponent(lambda,env1,nam,context)
            val result = call(component)
            pars.zip(args).foreach { case (par,arg) => writeAddr(VarAddr(par),arg,component) }
            Action.Value(result, StoreAdapter)
          case ((SchemeLambda(pars,_,_), _), _) =>
            Action.Err(ArityError(fexp, pars.length, args.length))
          case ((lambda,env1),_) =>
            Action.Err(TypeError("operator expected to be a closure, but is not", lattice.closure((lambda, env1), None)))
      }

    private def applyPrimitives(fexp: SchemeExp, fval: Value, args: List[(Value,SchemeExp)]): Set[Action.A] =
      lattice.getPrimitives[schemeSemantics.Primitive](fval).flatMap(prim => prim.callAction(fexp, args.map(_.swap), StoreAdapter, this))

    // analysis entry point
    def analyze(timeout: Timeout.T): Unit = {
      val exp = component match {
        case MainComponent => SchemeUndefiner.undefine(List(program))
        case CallComponent(SchemeLambda(_,body,_),_,_,_) => SchemeBody(body)
      }
      val env = component match {
        case MainComponent => initialEnv
        case CallComponent(SchemeLambda(pars,_,_),lex,_,_) =>
          pars.foldLeft(lex)((acc,par) => acc.extend(par.name,allocAddr(VarAddr(par))))
      }
      val state: State = State(ControlEval(exp, env), Store.empty[KAddr, Set[Kont]], HaltKontAddr, this)
      var work: Set[State] = Set[State](state)
      var visited: Set[State] = Set[State]()
      var result: Value = lattice.bottom
      while(work.nonEmpty) {
        if (timeout.reached) throw new TimeoutException()
        val state = work.head
        work = work.tail
        if (state.finished) {
          result = lattice.join(result, state.control.asInstanceOf[ControlKont].v)
        } else if (!visited.contains(state) && !state.halted) {
          val successors = state.step()
          work ++= successors
        }
        visited += state
      }
      writeResult(result)
    }
  }
}
*/

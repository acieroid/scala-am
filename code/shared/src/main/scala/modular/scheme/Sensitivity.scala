package scalaam.modular.scheme

import scalaam.core.Identity.Position
import scalaam.primitiveCompilation.SchemePrimitives

/* Simplest (and most imprecise): no context-sensitivity */
trait NoSensitivity extends SchemeModFSemantics {
  case class ComponentContext() {
    override def toString = ""
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position): ComponentContext = ComponentContext()
}

/* Full argument sensitivity for ModF */
trait FullArgumentSensitivity extends SchemeModFSemantics {
  case class ComponentContext(args: List[Value]) {
    override def toString: String = args.mkString(",")
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position): ComponentContext = ComponentContext(args)
}

trait CallSiteSensitivity extends SchemeModFSemantics {
  case class ComponentContext(fn: Position, call: Position) {
    override def toString: String = s"$call->$fn"
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position): ComponentContext = ComponentContext(clo._1.idn.pos, call)
}

trait FullArgumentCallSiteSensitivity extends SchemeModFSemantics {
  case class ComponentContext(fn: Position, call: Position, args: List[Value]) {
    override def toString: String = {
      val argsstr = args.mkString(",")
      s"$call->$fn $argsstr"
    }
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position): ComponentContext =
    ComponentContext(clo._1.idn.pos, call, args)
}

trait PrimitiveSensitivity extends SchemeModFSemantics {
  trait ComponentContext
  case class PrimitiveContext(fn: Position, call: Position, args: List[Value]) extends ComponentContext {
    override def toString: String = {
      val argsstr = args.mkString(",")
      s"$call->$fn $argsstr"
    }
  }
  case class NoContext() extends ComponentContext {
    override def toString = ""
  }
  
  def isPrimitive(nam: Option[String]): Boolean = nam match {
    case Some(n) if SchemePrimitives.names.contains(n) => true
    case _ => false
  }

  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position): ComponentContext = {
    if (isPrimitive(nam)) {
      PrimitiveContext(clo._1.idn.pos, call, args)
    } else {
      NoContext()
    }
  }
}

object CompoundSensitivities {
  /* TODO: extends SchemeModFSemantics */
  trait Sensitivity[Value] {
    trait Context
    // TODO: Q. what is the target? The function that is called I think, but I'm not sure.
    def alloc(target: Position, callSite: Position, args: List[Value]): Context
  }

  trait CompoundSensitivity extends SchemeModFSemantics {
    val HighSensitivity: Sensitivity[Value]
    val LowSensitivity: Sensitivity[Value]
    trait ComponentContext
    case class High(ctx: HighSensitivity.Context) extends ComponentContext
    case class Low(ctx: LowSensitivity.Context) extends ComponentContext
    def isPrimitive(nam: Option[String]): Boolean = nam match {
      case Some(n) if SchemePrimitives.names.contains(n) => true
      case _ => false
    }

    def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position): ComponentContext = {
      if (isPrimitive(nam)) {
        High(HighSensitivity.alloc(clo._1.idn.pos, call, args))
      } else {
        Low(LowSensitivity.alloc(clo._1.idn.pos, call, args))
      }
    }
  }


  class NoSensitivity[Value] extends Sensitivity[Value] {
    object NoContext extends Context
    def alloc(target: Position, callSite: Position, args: List[Value]): Context = NoContext
  }

  class CallSiteSensitivity[Value] extends Sensitivity[Value] {
    case class CallSiteContext(callSite: Position) extends Context
    def alloc(target: Position, callSite: Position, args: List[Value]): Context = CallSiteContext(callSite)
  }

  class FullArgumentSensitivity[Value] extends Sensitivity[Value] {
    case class FullArgumentContext(args: List[Value]) extends Context
    def alloc(target: Position, callSite: Position, args: List[Value]): Context = FullArgumentContext(args)
  }

  class FunctionSensitivity[Value] extends Sensitivity[Value] {
    case class FunctionContext(fn: Position) extends Context
    def alloc(target: Position, callSite: Position, args: List[Value]): Context = FunctionContext(target)
  }

  class ProductSensitivity[Value](val sensitivity1: Sensitivity[Value], val sensitivity2: Sensitivity[Value]) extends Sensitivity[Value] {
    case class ProductContext(p1: sensitivity1.Context, p2: sensitivity2.Context) extends Context
    def alloc(target: Position, callSite: Position, args: List[Value]): Context =
      ProductContext(
        sensitivity1.alloc(target, callSite, args),
        sensitivity2.alloc(target, callSite, args))
  }

  trait S_0_0 extends CompoundSensitivity {
    val HighSensitivity = new NoSensitivity[Value]
    val LowSensitivity = new NoSensitivity[Value]
  }

  trait S_CS_0 extends CompoundSensitivity {
    val HighSensitivity = new CallSiteSensitivity[Value]
    val LowSensitivity = new NoSensitivity[Value]
  }

  /* TODO: To be sure: do we need FCS or CS */
  trait S_FCS_0 extends CompoundSensitivity {
    val HighSensitivity = new ProductSensitivity[Value](new CallSiteSensitivity[Value] {}, new FunctionSensitivity[Value] {})
    val LowSensitivity = new NoSensitivity[Value]
  }

  trait S_CS_CS extends CompoundSensitivity {
    val HighSensitivity = new CallSiteSensitivity[Value]
    val LowSensitivity = new CallSiteSensitivity[Value]
  }

  trait S_FA_0 extends CompoundSensitivity {
    val HighSensitivity = new FullArgumentSensitivity[Value]
    val LowSensitivity = new NoSensitivity[Value]
  }

  trait S_FA_CS extends CompoundSensitivity {
    val HighSensitivity = new FullArgumentSensitivity[Value]
    val LowSensitivity = new CallSiteSensitivity[Value]
  }

  trait S_CSFA_0 extends CompoundSensitivity {
    val HighSensitivity = new ProductSensitivity[Value](new CallSiteSensitivity[Value], new FullArgumentSensitivity[Value])
    val LowSensitivity = new NoSensitivity[Value]
  }

  trait S_CSFA_CS extends CompoundSensitivity {
    val HighSensitivity = new ProductSensitivity[Value](new CallSiteSensitivity[Value], new FullArgumentSensitivity[Value])
    val LowSensitivity = new CallSiteSensitivity[Value]
  }


}

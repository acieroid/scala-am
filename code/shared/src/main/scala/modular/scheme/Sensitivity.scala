package scalaam.modular.scheme

import scalaam.core.Identity.Position

/* Simplest (and most imprecise): no context-sensitivity */
trait NoSensitivity extends SchemeModFSemantics {
  case class ComponentContext() {
    override def toString = ""
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Option[ComponentContext]): ComponentContext = ComponentContext()
  def getPtrCtx(cmp: Option[ComponentContext]): Any = ()
}

/* Full argument sensitivity for ModF */
trait FullArgumentSensitivity extends SchemeModFSemantics {
  case class ComponentContext(args: List[Value]) {
    override def toString: String = args.mkString(",")
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Option[ComponentContext]): ComponentContext = ComponentContext(args)
  def getPtrCtx(cmp: Option[ComponentContext]): Any = ()
}

trait CallSiteSensitivity extends SchemeModFSemantics {
  case class ComponentContext(fn: Position, call: Position) {
    override def toString: String = s"$call->$fn"
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Option[ComponentContext]): ComponentContext = ComponentContext(clo._1.idn.pos, call)
  def getPtrCtx(cmp: Option[ComponentContext]): Any = ()
}

trait FullArgumentCallSiteSensitivity extends SchemeModFSemantics {
  case class ComponentContext(fn: Position, call: Position, args: List[Value]) {
    override def toString: String = {
      val argsstr = args.mkString(",")
      s"$call->$fn $argsstr"
    }
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Option[ComponentContext]): ComponentContext =
    ComponentContext(clo._1.idn.pos, call, args)
  def getPtrCtx(cmp: Option[ComponentContext]): Any = ()
}

object CompoundSensitivities {
  import scalaam.language.scheme.primitives.SchemePrelude

  trait Sensitivity[Value] {
    trait Context
    def alloc(target: Position, callSite: Position, args: List[Value]): Context
  }

  object S extends Enumeration {
    type S = Value
    val S_0_0_Old, S_0_0, S_CS_0, S_CS_0_Old, S_CS_CS, S_FA_0_Old, S_FA_0, S_FA_CS, S_CSFA_0, S_CSFA_0_Old, S_CSFA_CS = Value
  }

  import S._

  val sensitivities: List[S] = List(S_0_0_Old, S_0_0, S_CS_0_Old, S_CS_0, S_CS_CS, S_FA_0_Old, S_FA_0, S_FA_CS, S_CSFA_0_Old, S_CSFA_0, S_CSFA_CS)

  trait CompoundSensitivityOld extends SchemeModFSemantics {
    val HighSensitivity: Sensitivity[Value]
    val LowSensitivity: Sensitivity[Value]
    trait ComponentContext
    case class High(ctx: HighSensitivity.Context) extends ComponentContext
    case class Low(ctx: LowSensitivity.Context) extends ComponentContext
    def isPrimitive(nam: Option[String]): Boolean = nam match {
      case Some(n) if SchemePrelude.primNames.contains(n) => true
      case _ => false
    }

    def getPtrCtx(cmp: Option[ComponentContext]): Any = ()

    def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Option[ComponentContext]): ComponentContext = {
      if (isPrimitive(nam)) {
        High(HighSensitivity.alloc(clo._1.idn.pos, call, args))
      } else {
        Low(LowSensitivity.alloc(clo._1.idn.pos, call, args))
      }
    }
  }

  trait CompoundSensitivity extends SchemeModFSemantics {
    val HighSensitivity: Sensitivity[Value]
    val LowSensitivity: Sensitivity[Value]

    trait ComponentContext
    case class High(ctx: HighSensitivity.Context, userCall: Position) extends ComponentContext
    case class Low(ctx: LowSensitivity.Context) extends ComponentContext
    def isPrimitive(nam: Option[String]): Boolean = nam match {
      case Some(n) if SchemePrelude.primPrecision.contains(n) => true
      case _ => false
    }

    def getPtrCtx(cmp: Option[ComponentContext]): Any = cmp match {
      case Some(High(_, userCall)) => userCall
      case _ => ()
    }

    def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Option[ComponentContext]): ComponentContext = {
      if (isPrimitive(nam)) {
        High(HighSensitivity.alloc(clo._1.idn.pos, call, args),
          caller match {
            case Some(High(_, userCall)) => userCall
            case _ => call
          }
        )
      } else {
        Low(LowSensitivity.alloc(clo._1.idn.pos, call, args))
      }
    }
  }




  class NoSensitivity[Value] extends Sensitivity[Value] {
    object NoContext extends Context {
      override def toString = "NoCtx"
    }
    def alloc(target: Position, callSite: Position, args: List[Value]): Context = NoContext
  }

  class CallSiteSensitivity[Value] extends Sensitivity[Value] {
    case class CallSiteContext(callSite: Position) extends Context {
      override def toString = s"CSCtx($callSite)"
    }
    def alloc(target: Position, callSite: Position, args: List[Value]): Context = CallSiteContext(callSite)
  }

  class FullArgumentSensitivity[Value] extends Sensitivity[Value] {
    case class FullArgumentContext(args: List[Value]) extends Context {
      override def toString = s"FACtx($args)"
    }
    def alloc(target: Position, callSite: Position, args: List[Value]): Context = FullArgumentContext(args)
  }

  class ProductSensitivity[Value](val sensitivity1: Sensitivity[Value], val sensitivity2: Sensitivity[Value]) extends Sensitivity[Value] {
    case class ProductContext(p1: sensitivity1.Context, p2: sensitivity2.Context) extends Context
    def alloc(target: Position, callSite: Position, args: List[Value]): Context =
      ProductContext(
        sensitivity1.alloc(target, callSite, args),
        sensitivity2.alloc(target, callSite, args))
  }

  trait S_0_0_Old extends CompoundSensitivityOld {
    val HighSensitivity = new NoSensitivity[Value]
    val LowSensitivity = new NoSensitivity[Value]
  }

  trait S_0_0 extends CompoundSensitivity {
    val HighSensitivity = new NoSensitivity[Value]
    val LowSensitivity = new NoSensitivity[Value]
  }

  trait S_CS_0_Old extends CompoundSensitivityOld {
    val HighSensitivity = new CallSiteSensitivity[Value]
    val LowSensitivity = new NoSensitivity[Value]
  }

  trait S_CS_0 extends CompoundSensitivity {
    val HighSensitivity = new CallSiteSensitivity[Value]
    val LowSensitivity = new NoSensitivity[Value]
  }

  trait S_CS_CS extends CompoundSensitivity {
    val HighSensitivity = new CallSiteSensitivity[Value]
    val LowSensitivity = new CallSiteSensitivity[Value]
  }

  trait S_FA_0_Old extends CompoundSensitivityOld {
    val HighSensitivity = new FullArgumentSensitivity[Value]
    val LowSensitivity = new NoSensitivity[Value]
  }

  trait S_FA_0 extends CompoundSensitivity {
    val HighSensitivity = new FullArgumentSensitivity[Value]
    val LowSensitivity = new NoSensitivity[Value]
  }

  trait S_FA_CS extends CompoundSensitivity {
    val HighSensitivity = new FullArgumentSensitivity[Value]
    val LowSensitivity = new CallSiteSensitivity[Value]
  }

  trait S_CSFA_0_Old extends CompoundSensitivityOld {
    val HighSensitivity = new ProductSensitivity[Value](new CallSiteSensitivity[Value], new FullArgumentSensitivity[Value])
    val LowSensitivity = new NoSensitivity[Value]
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



package scalaam.modular.scheme

import scalaam.core.Position._

/* Simplest (and most imprecise): no context-sensitivity */
trait NoSensitivity extends SchemeModFSemantics {
  case class ComponentContext() {
    override def toString = ""
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component): ComponentContext = ComponentContext()
}

/* Full argument sensitivity for ModF */
trait FullArgumentSensitivity extends SchemeModFSemantics {
  case class ComponentContext(args: List[Value]) {
    override def toString: String = args.mkString(",")
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component): ComponentContext = ComponentContext(args)
}

trait CallSiteSensitivity extends SchemeModFSemantics {
  case class ComponentContext(fn: Position, call: Position) {
    override def toString: String = s"$call->$fn"
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component): ComponentContext = ComponentContext(clo._1.idn.pos, call)
}

trait FullArgumentCallSiteSensitivity extends SchemeModFSemantics {
  case class ComponentContext(fn: Position, call: Position, args: List[Value]) {
    override def toString: String = {
      val argsstr = args.mkString(",")
      s"$call->$fn $argsstr"
    }
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component): ComponentContext =
    ComponentContext(clo._1.idn.pos, call, args)
}

object CompoundSensitivities {
  import scalaam.language.scheme.primitives.SchemePrelude

  trait Sensitivity[Value, Component] {
    trait Context
    def alloc(target: Position, args: List[Value], callSite: Position, callerCtx: Option[Context]): Context
  }

  class NoSensitivity[V, Component] extends Sensitivity[V, Component] {
    object NoContext extends Context {
      override def toString = "NoCtx"
    }
    def alloc(target: Position, args: List[V], callSite: Position, callerCtx: Option[Context]): Context = NoContext
  }

  class CallSiteSensitivity[V, Component] extends Sensitivity[V, Component] {
    case class CallSiteContext(callSite: Position) extends Context {
      override def toString = s"CSCtx($callSite)"
    }
    def alloc(target: Position, args: List[V], callSite: Position, callerCtx: Option[Context]): Context = CallSiteContext(callSite)
  }

  class FullArgumentSensitivity[V, Component] extends Sensitivity[V, Component] {
    case class FullArgumentContext(args: List[V]) extends Context {
      override def toString = s"FACtx($args)"
    }
    def alloc(target: Position, args: List[V], callSite: Position, callerCtx: Option[Context]): Context = FullArgumentContext(args)
  }

  class ProductSensitivity[V, Component](val sensitivity1: Sensitivity[V, Component], val sensitivity2: Sensitivity[V, Component]) extends Sensitivity[V, Component] {
    case class ProductContext(p1: sensitivity1.Context, p2: sensitivity2.Context) extends Context
    def alloc(target: Position, args: List[V], callSite: Position, callerCtx: Option[Context]): Context = {
      val (p1, p2) = callerCtx match {
        case Some(ProductContext(p1, p2)) => (Some(p1), Some(p2))
        case _ => (None, None)
      }
      ProductContext(
        sensitivity1.alloc(target, args, callSite, p1),
        sensitivity2.alloc(target, args, callSite, p2))
    }
  }

  class kContextSensitivity[V, Component](val k: Int, val sensitivity: Sensitivity[V, Component]) extends Sensitivity[V, Component] {
    case class kContext(l: List[sensitivity.Context]) extends Context
    def alloc(target: Position, args: List[V], callSite: Position, callerCtx: Option[Context]): Context =
      kContext((sensitivity.alloc(target, args, callSite, None /* inner sensitivity should not be context sensitive */) :: (callerCtx match {
        case Some(kContext(l2)) => l2
        case _ => List()
      })).take(k))
  }

  // Acyclic k-CFA inspired by "JSAI: A Static Analysis Platform for JavaScript" (FSE'14)
  class kAcyclicCallSiteSensitivity[V, Component](val k: Int) extends Sensitivity[V, Component] {
    case class kContext(l: List[Position]) extends Context
    def alloc(target: Position, args: List[V], callSite: Position, callerCtx: Option[Context]): Context =
      kContext(callerCtx match {
        case Some(kContext(l2)) => if (l2.contains(callSite)) {
          l2.dropWhile(_ != callSite)
        } else {
          (callSite :: l2).take(k)
        }
        case _ => List(callSite)
      })
  }


  class kCallSitesSensitivity[V, Component](k: Int) extends kContextSensitivity(k, new CallSiteSensitivity[V, Component])
  class kFullArgumentSensitivity[V, Component](k: Int) extends kContextSensitivity(k, new FullArgumentSensitivity[V, Component])
  class kCSFASensitivity[V, Component](k: Int) extends kContextSensitivity(k, new ProductSensitivity[V, Component](new CallSiteSensitivity[V, Component], new FullArgumentSensitivity[V, Component]))


  trait CompoundSensitivity extends SchemeModFSemantics {
    val HighSensitivity: Sensitivity[Value, Component]
    val LowSensitivity: Sensitivity[Value, Component]
    val primPrecision: Set[String] = SchemePrelude.primNames

    trait ComponentContext

    def isPrimitive(nam: Option[String]): Boolean = nam match {
      case Some(n) if primPrecision.contains(n) => true
      case _ => false
    }
  }


  trait SeparateLowHighSensitivity extends CompoundSensitivity {
    case class High(ctx: HighSensitivity.Context) extends ComponentContext
    case class Low(ctx: LowSensitivity.Context) extends ComponentContext

    def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component): ComponentContext = {
      if (isPrimitive(nam)) {
        High(HighSensitivity.alloc(clo._1.idn.pos, args, call, context(caller) match {
          case Some(High(ctx)) => Some(ctx)
          case _ => None
        }))
      } else {
        Low(LowSensitivity.alloc(clo._1.idn.pos, args, call, context(caller) match {
          case Some(Low(ctx)) => Some(ctx)
          case _ => None
        }))
      }
    }
  }

  object SeparateLowHighSensitivity {
    trait S_0_0 extends SeparateLowHighSensitivity {
      val HighSensitivity = new NoSensitivity[Value, Component]
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_CS_0 extends SeparateLowHighSensitivity {
      val HighSensitivity = new CallSiteSensitivity[Value, Component]
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_2CS_0 extends SeparateLowHighSensitivity {
      val HighSensitivity = new kCallSitesSensitivity[Value, Component](2)
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_10CS_0 extends SeparateLowHighSensitivity {
      val HighSensitivity = new kCallSitesSensitivity[Value, Component](10)
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_FA_0 extends SeparateLowHighSensitivity {
      val HighSensitivity = new FullArgumentSensitivity[Value, Component]
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_2FA_0 extends SeparateLowHighSensitivity {
      val HighSensitivity = new kFullArgumentSensitivity[Value, Component](2)
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_10FA_0 extends SeparateLowHighSensitivity {
      val HighSensitivity = new kFullArgumentSensitivity[Value, Component](10)
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_CSFA_0 extends SeparateLowHighSensitivity {
      val HighSensitivity = new ProductSensitivity[Value, Component](new CallSiteSensitivity[Value, Component], new FullArgumentSensitivity[Value, Component])
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_2AcyclicCS_0 extends SeparateLowHighSensitivity {
      val HighSensitivity = new kAcyclicCallSiteSensitivity[Value, Component](2)
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_10AcyclicCS_0 extends SeparateLowHighSensitivity {
      val HighSensitivity = new kAcyclicCallSiteSensitivity[Value, Component](10)
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
  }

  trait TrackLowToHighSensitivity extends CompoundSensitivity {
    case class High(ctx: HighSensitivity.Context, userCall: Position) extends ComponentContext
    case class Low(ctx: LowSensitivity.Context) extends ComponentContext

    def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component): ComponentContext = {
      if (isPrimitive(nam)) {
        High(HighSensitivity.alloc(clo._1.idn.pos, args, call, context(caller) match {
          case Some(High(ctx, _)) => Some(ctx)
          case _ => None
        }),
          caller match {
            case Some(High(_, userCall)) => userCall
            case _ => call
          }
        )
      } else {
        Low(LowSensitivity.alloc(clo._1.idn.pos, args, call, context(caller) match {
          case Some(Low(ctx)) => Some(ctx)
          case _ => None
        }))
      }
    }
  }

  object TrackLowToHighSensitivity {
    trait S_0_0 extends TrackLowToHighSensitivity {
      val HighSensitivity = new NoSensitivity[Value, Component]
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_CS_0 extends TrackLowToHighSensitivity {
      val HighSensitivity = new CallSiteSensitivity[Value, Component]
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_2CS_0 extends TrackLowToHighSensitivity {
      val HighSensitivity = new kCallSitesSensitivity[Value, Component](2)
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_10CS_0 extends TrackLowToHighSensitivity {
      val HighSensitivity = new kCallSitesSensitivity[Value, Component](10)
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_FA_0 extends TrackLowToHighSensitivity {
      val HighSensitivity = new FullArgumentSensitivity[Value, Component]
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_2FA_0 extends TrackLowToHighSensitivity {
      val HighSensitivity = new kFullArgumentSensitivity[Value, Component](2)
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_10FA_0 extends TrackLowToHighSensitivity {
      val HighSensitivity = new kFullArgumentSensitivity[Value, Component](10)
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_CSFA_0 extends TrackLowToHighSensitivity {
      val HighSensitivity = new ProductSensitivity[Value, Component](new CallSiteSensitivity[Value, Component], new FullArgumentSensitivity[Value, Component])
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_2AcyclicCS_0 extends TrackLowToHighSensitivity {
      val HighSensitivity = new kAcyclicCallSiteSensitivity[Value, Component](2)
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
    trait S_10AcyclicCS_0 extends TrackLowToHighSensitivity {
      val HighSensitivity = new kAcyclicCallSiteSensitivity[Value, Component](10)
      val LowSensitivity = new NoSensitivity[Value, Component]
    }
  }
}



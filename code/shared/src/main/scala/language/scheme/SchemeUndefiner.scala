package scalaam.language.scheme

import scalaam.core._

/**
  * Remove defines from a Scheme expression, replacing them by let bindings.
  * For example:
  *   (define foo 1)
  *   (define (f x) x)
  *   (f foo)
  * Will be converted to:
  *   (letrec ((foo 1)
  *            (f (lambda (x) x)))
  *     (f foo))
  * Which is semantically equivalent with respect to the end result
  */
object SchemeUndefiner {
  import scala.util.control.TailCalls._

  def undefine(exps: List[SchemeExp]): SchemeExp =
    undefine(exps, List(), None).result

  def undefine(exps: List[SchemeExp], defs: List[(Identifier, SchemeExp)], idn: Option[Identity]): TailRec[SchemeExp] =
    exps match {
      case Nil => done(SchemeBegin(Nil, Identity.none))
      case SchemeDefineFunction(name, args, body, pos) :: rest =>
        tailcall(
          tailcall(undefineBody(body)).flatMap(
            bodyv =>
              undefine(
                SchemeDefineVariable(name, SchemeLambda(args, bodyv, exps.head.idn), pos) :: rest,
                defs,
                idn
              )
          )
        )
      case SchemeDefineVarArgFunction(name, args, vararg, body, pos) :: rest =>
        tailcall(
          tailcall(undefineBody(body)).flatMap(
            bodyv =>
              undefine(
                SchemeDefineVariable(name, SchemeVarArgLambda(args, vararg, bodyv, exps.head.idn), pos) :: rest,
                defs,
                idn
              )
          )
        )
      case SchemeDefineVariable(name, value, pos) :: rest =>
        tailcall(undefine1(value)).flatMap(v => tailcall(undefine(rest, (name, v) :: defs, idn.orElse(Some(pos)))))
      case _ :: _ =>
        if (defs.isEmpty) {
          tailcall(undefineBody(exps)).flatMap({
            case Nil        => done(SchemeBegin(Nil, Identity.none))
            case exp :: Nil => done(exp)
            case exps       => done(SchemeBegin(exps, exps.head.idn))
          })
        } else {
          tailcall(undefineBody(exps))
            .flatMap(body => done(SchemeLetrec(defs.reverse, body, idn.get)))
        }
    }

  def trampolineM[A, B](f: A => TailRec[B], l: List[A]): TailRec[List[B]] = l match {
    case Nil => done(Nil)
    case x :: xs =>
      tailcall(f(x)).flatMap(y => tailcall(trampolineM(f, xs)).flatMap(ys => done(y :: ys)))
  }

  def undefine1(exp: SchemeExp): TailRec[SchemeExp] = undefine(List(exp), List(), None)

  def undefineBody(exps: List[SchemeExp]): TailRec[List[SchemeExp]] = exps match {
    case Nil                                   => done(Nil)
    case SchemeDefineFunction(_, _, _, _) :: _          => tailcall(undefine(exps, List(), None)).map(v => List(v))
    case SchemeDefineVarArgFunction(_, _, _, _, _) :: _ => tailcall(undefine(exps, List(), None)).map(v => List(v))
    case SchemeDefineVariable(_, _, _) :: _             => tailcall(undefine(exps, List(), None)).map(v => List(v))
    case exp :: rest => {
      val exp2 = exp match {
        case SchemeLambda(args, body, pos) =>
          tailcall(undefineBody(body)).map(b => SchemeLambda(args, b, pos))
        case SchemeVarArgLambda(args, vararg, body, pos) =>
          tailcall(undefineBody(body)).map(b => SchemeVarArgLambda(args, vararg, b, pos))
        case SchemeFuncall(f, args, pos) =>
          tailcall(undefine1(f)).flatMap(
            fun => trampolineM(undefine1, args).map(argsv => SchemeFuncall(fun, argsv, pos))
          )
        case SchemeIf(cond, cons, alt, pos) =>
          tailcall(undefine1(cond)).flatMap(
            condv =>
              tailcall(undefine1(cons)).flatMap(
                consv => tailcall(undefine1(alt)).map(altv => SchemeIf(condv, consv, altv, pos))
              )
          )
        case SchemeLet(bindings, body, pos) =>
          trampolineM(
            (x: (Identifier, SchemeExp)) =>
              x match {
                case (b, v) => tailcall(undefine1(v)).map(vv => (b, vv))
              },
            bindings
          ).flatMap(
            bindingsv => tailcall(undefineBody(body)).map(bodyv => SchemeLet(bindingsv, bodyv, pos))
          )
        case SchemeLetStar(bindings, body, pos) =>
          trampolineM(
            (x: (Identifier, SchemeExp)) =>
              x match {
                case (b, v) => tailcall(undefine1(v)).map(vv => (b, vv))
              },
            bindings
          ).flatMap(
            bindingsv =>
              tailcall(undefineBody(body)).map(bodyv => SchemeLetStar(bindingsv, bodyv, pos))
          )
        case SchemeLetrec(bindings, body, pos) =>
          trampolineM(
            (x: (Identifier, SchemeExp)) =>
              x match {
                case (b, v) => tailcall(undefine1(v)).map(vv => (b, vv))
              },
            bindings
          ).flatMap(
            bindingsv =>
              tailcall(undefineBody(body)).map(bodyv => SchemeLetrec(bindingsv, bodyv, pos))
          )
        case SchemeNamedLet(name, bindings, body, pos) =>
          trampolineM(
            (x: (Identifier, SchemeExp)) =>
              x match {
                case (b, v) => tailcall(undefine1(v)).map(vv => (b, vv))
              },
            bindings
          ).flatMap(
            bindingsv =>
              tailcall(undefineBody(body)).map(bodyv => SchemeNamedLet(name, bindingsv, bodyv, pos))
          )
        case SchemeSet(variable, value, pos) =>
          tailcall(undefine1(value)).map(v => SchemeSet(variable, v, pos))
        case SchemeBegin(exps, pos) =>
          tailcall(undefineBody(exps)).map(expsv => SchemeBegin(expsv, pos))
        case SchemeAnd(args, pos) =>
          trampolineM(undefine1, args).map(argsv => SchemeAnd(argsv, pos))
        case SchemeOr(args, pos)       => trampolineM(undefine1, args).map(argsv => SchemeOr(argsv, pos))
        case SchemePair(car,cdr,pos)   =>
          for {
            carUndef <- tailcall(undefine1(car))
            cdrUndef <- tailcall(undefine1(cdr))
          } yield SchemePair(carUndef, cdrUndef, pos)
        case SchemeSplicedPair(exps,cdr,pos) =>
          for {
            spliceUndef <- tailcall(undefine1(exps))
            cdrUndef <- tailcall(undefine1(cdr))
          } yield SchemeSplicedPair(spliceUndef, cdrUndef, pos)
        case SchemeVar(id)             => done(SchemeVar(id))
        case SchemeValue(value, pos)   => done(SchemeValue(value, pos))
      }
      exp2.flatMap(e2 => tailcall(undefineBody(rest)).flatMap(e3 => done(e2 :: e3)))
    }
  }
}

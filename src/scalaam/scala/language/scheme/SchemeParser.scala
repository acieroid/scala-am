package scalaam.language.scheme

import scalaam.core.{Position, Identifier}
import scalaam.language.sexp._

/* TODO: use trampolines to ensure that we can parse anything without blowing up the stack */
/* TODO: free vars function, and other helpers? */

/**
  * Object that provides a method to compile an s-expression into a Scheme expression
  */
object SchemeCompiler {

  /**
    * Reserved keywords
    */
  val reserved: List[String] =
    List("lambda", "if", "let", "let*", "letrec", "cond", "case", "set!", "begin", "define", "do")

  def compile(exp: SExp): SchemeExp = exp match {
    case SExpPair(SExpId(Identifier("quote", _)), SExpPair(quoted, SExpValue(ValueNil, _), _), _) =>
      compile(SExpQuoted(quoted, exp.pos))
    case SExpPair(SExpId(Identifier("quote", _)), _, _) =>
      throw new Exception(s"Invalid Scheme quote: $exp (${exp.pos})")
    case SExpPair(SExpId(Identifier("lambda", _)),
                  SExpPair(args, SExpPair(first, rest, _), _),
                  _) =>
      SchemeLambda(compileArgs(args), compile(first) :: compileBody(rest), exp.pos)
    case SExpPair(SExpId(Identifier("lambda", _)), _, _) =>
      throw new Exception(s"Invalid Scheme lambda: $exp (${exp.pos})")
    case SExpPair(SExpId(Identifier("if", _)),
                  SExpPair(cond, SExpPair(cons, SExpPair(alt, SExpValue(ValueNil, _), _), _), _),
                  _) =>
      SchemeIf(compile(cond), compile(cons), compile(alt), exp.pos)
    case SExpPair(SExpId(Identifier("if", _)),
                  SExpPair(cond, SExpPair(cons, SExpValue(ValueNil, _), _), _),
                  _) =>
      /* Empty else branch is replaced by #f (R5RS states it's unspecified) */
      SchemeIf(compile(cond), compile(cons), SchemeValue(ValueBoolean(false), exp.pos), exp.pos)
    case SExpPair(SExpId(Identifier("if", _)), _, _) =>
      throw new Exception(s"Invalid Scheme if: $exp (${exp.pos})")
    case SExpPair(SExpId(Identifier("let", _)),
                  SExpPair(SExpId(name), SExpPair(bindings, SExpPair(first, rest, _), _), _),
                  _) =>
      SchemeNamedLet(name, compileBindings(bindings), compile(first) :: compileBody(rest), exp.pos)
    case SExpPair(SExpId(Identifier("let", _)),
                  SExpPair(bindings, SExpPair(first, rest, _), _),
                  _) =>
      SchemeLet(compileBindings(bindings), compile(first) :: compileBody(rest), exp.pos)
    case SExpPair(SExpId(Identifier("let", _)), _, _) =>
      throw new Exception(s"Invalid Scheme let: $exp")
    case SExpPair(SExpId(Identifier("let*", _)),
                  SExpPair(bindings, SExpPair(first, rest, _), _),
                  _) =>
      SchemeLetStar(compileBindings(bindings), compile(first) :: compileBody(rest), exp.pos)
    case SExpPair(SExpId(Identifier("let*", _)), _, _) =>
      throw new Exception(s"Invalid Scheme let*: $exp")
    case SExpPair(SExpId(Identifier("letrec", _)),
                  SExpPair(bindings, SExpPair(first, rest, _), _),
                  _) =>
      SchemeLetrec(compileBindings(bindings), compile(first) :: compileBody(rest), exp.pos)
    case SExpPair(SExpId(Identifier("letrec", _)), _, _) =>
      throw new Exception(s"Invalid Scheme letrec: $exp")
    case SExpPair(SExpId(Identifier("set!", _)),
                  SExpPair(SExpId(v), SExpPair(value, SExpValue(ValueNil, _), _), _),
                  _) =>
      SchemeSet(v, compile(value), exp.pos)
    case SExpPair(SExpId(Identifier("set!", _)), _, _) =>
      throw new Exception(s"Invalid Scheme set!: $exp")
    case SExpPair(SExpId(Identifier("begin", _)), body, _) =>
      SchemeBegin(compileBody(body), exp.pos)
    case SExpPair(SExpId(Identifier("cond", _)), clauses, _) =>
      SchemeCond(compileCondClauses(clauses), exp.pos)
    case SExpPair(SExpId(Identifier("case", _)), SExpPair(exp, clauses, _), _) =>
      val (c, d) = compileCaseClauses(clauses)
      SchemeCase(compile(exp), c, d, exp.pos)
    case SExpPair(SExpId(Identifier("and", _)), args, _) =>
      SchemeAnd(compileBody(args), exp.pos)
    case SExpPair(SExpId(Identifier("or", _)), args, _) =>
      SchemeOr(compileBody(args), exp.pos)
    case SExpPair(SExpId(Identifier("define", _)),
                  SExpPair(SExpId(name), SExpPair(value, SExpValue(ValueNil, _), _), _),
                  _) =>
      SchemeDefineVariable(name, compile(value), exp.pos)
    case SExpPair(SExpId(Identifier("define", _)),
                  SExpPair(SExpPair(SExpId(name), args, _), SExpPair(first, rest, _), _),
                  _) =>
      SchemeDefineFunction(name, compileArgs(args), compile(first) :: compileBody(rest), exp.pos)
    case SExpPair(SExpId(Identifier("do", _)),
                  SExpPair(bindings, SExpPair(SExpPair(test, finals, _), commands, _), _),
                  _) =>
      SchemeDo(compileDoBindings(bindings),
               compile(test),
               compileBody(finals),
               compileBody(commands),
               exp.pos)

    case SExpPair(f, args, _) =>
      SchemeFuncall(compile(f), compileBody(args), exp.pos)
    case SExpId(v) =>
      if (reserved.contains(v.name)) {
        throw new Exception(s"Invalid Scheme identifier (reserved): $exp")
      } else {
        SchemeVar(v)
      }
    case SExpValue(value, _)   => SchemeValue(value, exp.pos)
    case SExpQuoted(quoted, _) => SchemeQuoted(quoted, exp.pos)
  }

  def compileArgs(args: SExp): List[Identifier] = args match {
    case SExpPair(SExpId(id), rest, _) => id :: compileArgs(rest)
    case SExpValue(ValueNil, _)        => Nil
    case _                             => throw new Exception(s"Invalid Scheme argument list: $args (${args.pos})")
  }

  def compileBody(body: SExp): List[SchemeExp] = body match {
    case SExpPair(exp, rest, _) => compile(exp) :: compileBody(rest)
    case SExpValue(ValueNil, _) => Nil
    case _                      => throw new Exception(s"Invalid Scheme body: $body (${body.pos})")
  }

  def compileBindings(bindings: SExp): List[(Identifier, SchemeExp)] = bindings match {
    case SExpPair(SExpPair(SExpId(v), SExpPair(value, SExpValue(ValueNil, _), _), _), rest, _) =>
      if (reserved.contains(v.name)) {
        throw new Exception(s"Invalid Scheme identifier (reserved): $v (${bindings.pos})")
      } else {
        (v, compile(value)) :: compileBindings(rest)
      }
    case SExpValue(ValueNil, _) => Nil
    case _                      => throw new Exception(s"Invalid Scheme bindings: $bindings (${bindings.pos})")
  }

  def compileDoBindings(bindings: SExp): List[(Identifier, SchemeExp, Option[SchemeExp])] =
    bindings match {
      case SExpPair(SExpPair(SExpId(v), SExpPair(value, SExpValue(ValueNil, _), _), _), rest, _) =>
        if (reserved.contains(v.name)) {
          throw new Exception(s"Invalid Scheme identifier (reserved): $v (${bindings.pos})")
        } else {
          (v, compile(value), None) :: compileDoBindings(rest)
        }
      case SExpPair(
          SExpPair(SExpId(v), SExpPair(value, SExpPair(step, SExpValue(ValueNil, _), _), _), _),
          rest,
          _) =>
        if (reserved.contains(v.name)) {
          throw new Exception(s"Invalid Scheme identifier (reserved): $v (${bindings.pos})")
        } else {
          (v, compile(value), Some(compile(step))) :: compileDoBindings(rest)
        }
      case SExpValue(ValueNil, _) => Nil
      case _                      => throw new Exception(s"Invalid Scheme do-bindings: $bindings (${bindings.pos})")
    }

  def compileCondClauses(clauses: SExp): List[(SchemeExp, List[SchemeExp])] = clauses match {
    case SExpPair(SExpPair(SExpId(Identifier("else", _)), SExpPair(first, rest, _), _),
                  SExpValue(ValueNil, _),
                  _) =>
      List((SchemeValue(ValueBoolean(true), clauses.pos), compile(first) :: compileBody(rest)))
    case SExpPair(SExpPair(cond, SExpPair(first, rest, _), _), restClauses, _) =>
      (compile(cond), compile(first) :: compileBody(rest)) :: compileCondClauses(restClauses)
    case SExpPair(SExpPair(cond, SExpValue(ValueNil, _), _), restClauses, _) =>
      (compile(cond), Nil) :: compileCondClauses(restClauses)
    case SExpValue(ValueNil, _) => Nil
    case _                      => throw new Exception(s"Invalid Scheme cond clauses: $clauses ${clauses.pos})")
  }

  def compileCaseClauses(
      clauses: SExp): (List[(List[SchemeValue], List[SchemeExp])], List[SchemeExp]) =
    clauses match {
      case SExpPair(SExpPair(SExpId(Identifier("else", _)), SExpPair(first, rest, _), _),
                    SExpValue(ValueNil, _),
                    _) =>
        (List(), compile(first) :: compileBody(rest))
      case SExpPair(SExpPair(objects, body, _), restClauses, _) =>
        val (compiled, default) = compileCaseClauses(restClauses)
        ((compileCaseObjects(objects), compileBody(body)) :: compiled, default)
      case SExpValue(ValueNil, _) => (Nil, Nil)
      case _                      => throw new Exception(s"Invalid Scheme case clauses: $clauses (${clauses.pos})")
    }

  def compileCaseObjects(objects: SExp): List[SchemeValue] = objects match {
    case SExpPair(SExpValue(v, _), rest, _) =>
      SchemeValue(v, objects.pos) :: compileCaseObjects(rest)
    case SExpPair(SExpId(id), rest, _) =>
      /* identifiers in case expressions are treated as symbols */
      SchemeValue(ValueSymbol(id.name), id.pos) :: compileCaseObjects(rest)
    case SExpValue(ValueNil, _) => Nil
    case _                      => throw new Exception(s"Invalid Scheme case objects: $objects (${objects.pos})")
  }
}

/**
  * Object that provides a method to rename variables in a Scheme program in
  * order to have only unique names. For example, (let ((x 1)) (let ((x 2)) x))
  * will be converted to (let ((_x0 1)) (let ((_x1 2)) _x1)). This is useful to
  * perform ANF conversion.
  */
object SchemeRenamer {

  /** Maps each variables to their alpha-renamed version (eg. x -> _x0) */
  type NameMap = Map[String, String]

  /** Map each variables to the number of times it is bound */
  type CountMap = Map[String, Int]

  def rename(exp: SchemeExp): SchemeExp =
    rename(exp, Map[String, String](), Map[String, Int]()) match {
      case (e, _) => e
    }

  def rename(exp: SchemeExp, names: NameMap, count: CountMap): (SchemeExp, CountMap) = exp match {
    case SchemeLambda(args, body, pos) =>
      countl(args, names, count) match {
        case (args1, names1, count1) =>
          renameList(body, names1, count1) match {
            case (body1, count2) => (SchemeLambda(args1, body1, pos), count2)
          }
      }
    case SchemeFuncall(f, args, pos) =>
      rename(f, names, count) match {
        case (f1, count1) =>
          renameList(args, names, count1) match {
            case (args1, count2) => (SchemeFuncall(f1, args1, pos), count2)
          }
      }
    case SchemeIf(cond, cons, alt, pos) =>
      rename(cond, names, count) match {
        case (cond1, count1) =>
          rename(cons, names, count1) match {
            case (cons1, count2) =>
              rename(alt, names, count2) match {
                case (alt1, count3) => (SchemeIf(cond1, cons1, alt1, pos), count3)
              }
          }
      }
    case SchemeLet(bindings, body, pos) =>
      countl(bindings.map(_._1), names, count) match {
        /* Use old names for expressions of bindings */
        case (variables, names1, count1) =>
          renameList(bindings.map(_._2), names, count1) match {
            case (exps, count2) =>
              renameList(body, names1, count2) match {
                case (body1, count3) => (SchemeLet(variables.zip(exps), body1, pos), count3)
              }
          }
      }
    case SchemeLetStar(bindings, body, pos) =>
      renameLetStarBindings(bindings, names, count) match {
        case (bindings1, names1, count1) =>
          renameList(body, names1, count1) match {
            case (body1, count2) => (SchemeLetStar(bindings1, body1, pos), count2)
          }
      }
    case SchemeLetrec(bindings, body, pos) =>
      countl(bindings.map(_._1), names, count) match {
        /* Use new names for expressions of bindings */
        case (variables, names1, count1) =>
          renameList(bindings.map(_._2), names1, count1) match {
            case (exps, count2) =>
              renameList(body, names1, count2) match {
                case (body1, count3) => (SchemeLetrec(variables.zip(exps), body1, pos), count3)
              }
          }
      }
    case SchemeNamedLet(name, bindings, body, pos) =>
      countl(bindings.map(_._1), names, count) match {
        case (variables, names1, count1) =>
          renameList(bindings.map(_._2), names1, count1) match {
            case (exps, count2) =>
              renameList(body, names1, count2) match {
                case (body1, count3) =>
                  (SchemeNamedLet(name /* TODO: rename it as well */,
                                  variables.zip(exps),
                                  body1,
                                  pos),
                   count3)
              }
          }
      }
    case SchemeSet(variable, value, pos) =>
      rename(value, names, count) match {
        case (value1, count1) =>
          (SchemeSet(names.get(variable.name) match {
            case Some(n) => Identifier(n, variable.pos)
            case None    => variable
          }, value1, pos), count1)
      }
    case SchemeBegin(body, pos) =>
      renameList(body, names, count) match {
        case (body1, count1) => (SchemeBegin(body1, pos), count1)
      }
    case SchemeCond(clauses, pos) =>
      clauses.foldLeft((List[(SchemeExp, List[SchemeExp])](), count))(
        (st: (List[(SchemeExp, List[SchemeExp])], CountMap), cl: (SchemeExp, List[SchemeExp])) =>
          (st, cl) match {
            case ((l, cs), (e, body)) =>
              rename(e, names, cs) match {
                case (e1, count1) =>
                  renameList(body, names, count1) match {
                    case (body1, count2) =>
                      ((e1, body1) :: l, count2)
                  }
              }
        }) match {
        case (l, count1) => (SchemeCond(l.reverse, pos), count1)
      }
    case SchemeCase(exp, clauses, default, pos) =>
      rename(exp, names, count) match {
        case (exp1, count1) =>
          clauses.foldLeft((List[(List[SchemeValue], List[SchemeExp])](), count1))(
            (st: (List[(List[SchemeValue], List[SchemeExp])], CountMap),
             cl: (List[SchemeValue], List[SchemeExp])) =>
              (st, cl) match {
                case ((l, cs), (objs, body)) =>
                  renameList(body, names, cs) match {
                    case (body1, count2) => ((objs, body1) :: l, count2)
                  }
            }) match {
            case (l, count1) =>
              renameList(default, names, count1) match {
                case (default1, count2) => (SchemeCase(exp1, l.reverse, default1, pos), count2)
              }
          }
      }
    case SchemeAnd(exps, pos) =>
      renameList(exps, names, count) match {
        case (exps1, count1) => (SchemeAnd(exps1, pos), count1)
      }
    case SchemeOr(exps, pos) =>
      renameList(exps, names, count) match {
        case (exps1, count1) => (SchemeOr(exps1, pos), count1)
      }
    case SchemeDefineVariable(name, value, pos) =>
      /* Keeps name untouched (maybe not correct?) */
      rename(value, names, count) match {
        case (value1, count1) => (SchemeDefineVariable(name, value1, pos), count1)
      }
    case SchemeDefineFunction(name, args, body, pos) =>
      countl(args, names, count) match {
        case (args1, names1, count1) =>
          renameList(body, names1, count1) match {
            case (body1, count2) =>
              (SchemeDefineFunction(name, args1, body1, pos), count2)
          }
      }
    case SchemeQuoted(quoted, pos) =>
      (SchemeQuoted(quoted, pos), count)
    case SchemeVar(id) =>
      names.get(id.name) match {
        case Some(n) => (SchemeVar(Identifier(n, id.pos)), count)
        case None    => (SchemeVar(Identifier(id.name, id.pos)), count) /* keep original name */
      }
    case SchemeValue(v, pos) =>
      (SchemeValue(v, pos), count)
    case _ => throw new Exception(s"Unhandled expression in renamer: $exp")
  }

  /** Renames a list of expressions executed sequentially (eg. within a begin) */
  def renameList(exps: List[SchemeExp],
                 names: NameMap,
                 count: CountMap): (List[SchemeExp], CountMap) = exps match {
    case exp :: rest =>
      val (exp1, count1)  = rename(exp, names, count)
      val (rest1, count2) = renameList(rest, names, count1)
      (exp1 :: rest1, count2)
    case Nil => (Nil, count)
  }

  def renameLetStarBindings(bindings: List[(Identifier, SchemeExp)],
                            names: NameMap,
                            count: CountMap): (List[(Identifier, SchemeExp)], NameMap, CountMap) =
    bindings match {
      case (v, e) :: rest =>
        count1(v, names, count) match {
          /* use old names, as with a let* the variable is not yet bound in its
           * definition */
          case (v1, names1, count1) =>
            rename(e, names, count1) match {
              case (e1, count2) =>
                renameLetStarBindings(rest, names1, count2) match {
                  case (rest1, names2, count3) =>
                    ((v1, e1) :: rest1, names2, count3)
                }
            }
        }
      case Nil => (Nil, names, count)
    }

  /** To be called when a new variable is introduced in the scope. Adds it to the
    * name map and count map */
  def count1(variable: Identifier,
             names: NameMap,
             count: CountMap): (Identifier, NameMap, CountMap) = {
    val c: Int = count.get(variable.name) match {
      case Some(x) => x + 1
      case None    => 0
    }
    val n = s"_$variable$c"
    (Identifier(n, variable.pos), names + (variable.name -> n), count + (variable.name -> c))
  }

  /** Same as count1 but for a list of variables */
  def countl(variables: List[Identifier],
             names: NameMap,
             count: CountMap): (List[Identifier], NameMap, CountMap) =
    variables.foldLeft((List[Identifier](), names, count))(
      (st: (List[Identifier], NameMap, CountMap), v: Identifier) =>
        st match {
          case (l, ns, cs) =>
            count1(v, ns, cs) match {
              case (v1, ns1, cs1) => ((v1 :: l), ns1, cs1)
            }
      }) match {
      case (l, ns, cs) => (l.reverse, ns, cs)
    }
}

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
    undefine(exps, List()).result

  def undefine(exps: List[SchemeExp], defs: List[(Identifier, SchemeExp)]): TailRec[SchemeExp] =
    exps match {
      case Nil => done(SchemeBegin(Nil, Position.none))
      case SchemeDefineFunction(name, args, body, pos) :: rest =>
        tailcall(
          tailcall(undefineBody(body)).flatMap(
            bodyv =>
              undefine(
                SchemeDefineVariable(name, SchemeLambda(args, bodyv, exps.head.pos), pos) :: rest,
                defs)))
      case SchemeDefineVariable(name, value, _) :: rest =>
        tailcall(undefine1(value)).flatMap(v => tailcall(undefine(rest, (name, v) :: defs)))
      case _ :: _ =>
        if (defs.isEmpty) {
          tailcall(undefineBody(exps)).flatMap({
            case Nil        => done(SchemeBegin(Nil, Position.none))
            case exp :: Nil => done(exp)
            case exps       => done(SchemeBegin(exps, exps.head.pos))
          })
        } else {
          tailcall(undefineBody(exps)).flatMap(body =>
            done(SchemeLetrec(defs.reverse, body, exps.head.pos)))
        }
    }

  def trampolineM[A, B](f: A => TailRec[B], l: List[A]): TailRec[List[B]] = l match {
    case Nil => done(Nil)
    case x :: xs =>
      tailcall(f(x)).flatMap(y => tailcall(trampolineM(f, xs)).flatMap(ys => done(y :: ys)))
  }

  def undefine1(exp: SchemeExp): TailRec[SchemeExp] = undefine(List(exp), List())

  def undefineBody(exps: List[SchemeExp]): TailRec[List[SchemeExp]] = exps match {
    case Nil                                   => done(Nil)
    case SchemeDefineFunction(_, _, _, _) :: _ => tailcall(undefine(exps, List())).map(v => List(v))
    case SchemeDefineVariable(_, _, _) :: _    => tailcall(undefine(exps, List())).map(v => List(v))
    case exp :: rest => {
      val exp2 = exp match {
        case SchemeLambda(args, body, pos) =>
          tailcall(undefineBody(body)).map(b => SchemeLambda(args, b, pos))
        case SchemeFuncall(f, args, pos) =>
          tailcall(undefine1(f)).flatMap(fun =>
            trampolineM(undefine1, args).map(argsv => SchemeFuncall(fun, argsv, pos)))
        case SchemeIf(cond, cons, alt, pos) =>
          tailcall(undefine1(cond)).flatMap(condv =>
            tailcall(undefine1(cons)).flatMap(consv =>
              tailcall(undefine1(alt)).map(altv => SchemeIf(condv, consv, altv, pos))))
        case SchemeLet(bindings, body, pos) =>
          trampolineM((x: (Identifier, SchemeExp)) =>
                        x match {
                          case (b, v) => tailcall(undefine1(v)).map(vv => (b, vv))
                      },
                      bindings).flatMap(bindingsv =>
            tailcall(undefineBody(body)).map(bodyv => SchemeLet(bindingsv, bodyv, pos)))
        case SchemeLetStar(bindings, body, pos) =>
          trampolineM((x: (Identifier, SchemeExp)) =>
                        x match {
                          case (b, v) => tailcall(undefine1(v)).map(vv => (b, vv))
                      },
                      bindings).flatMap(bindingsv =>
            tailcall(undefineBody(body)).map(bodyv => SchemeLetStar(bindingsv, bodyv, pos)))
        case SchemeLetrec(bindings, body, pos) =>
          trampolineM((x: (Identifier, SchemeExp)) =>
                        x match {
                          case (b, v) => tailcall(undefine1(v)).map(vv => (b, vv))
                      },
                      bindings).flatMap(bindingsv =>
            tailcall(undefineBody(body)).map(bodyv => SchemeLetrec(bindingsv, bodyv, pos)))
        case SchemeNamedLet(name, bindings, body, pos) =>
          trampolineM((x: (Identifier, SchemeExp)) =>
                        x match {
                          case (b, v) => tailcall(undefine1(v)).map(vv => (b, vv))
                      },
                      bindings).flatMap(bindingsv =>
            tailcall(undefineBody(body)).map(bodyv => SchemeNamedLet(name, bindingsv, bodyv, pos)))
        case SchemeSet(variable, value, pos) =>
          tailcall(undefine1(value)).map(v => SchemeSet(variable, v, pos))
        case SchemeBegin(exps, pos) =>
          tailcall(undefineBody(exps)).map(expsv => SchemeBegin(expsv, pos))
        case SchemeCond(clauses, pos) =>
          trampolineM(
            (e: (SchemeExp, List[SchemeExp])) =>
              e match {
                case (cond, body) =>
                  tailcall(undefine1(cond)).flatMap(condv =>
                    tailcall(undefineBody(body)).map(bodyv => (condv, bodyv)))
            },
            clauses
          ).map(clausesv => SchemeCond(clausesv, pos))
        case SchemeCase(key, clauses, default, pos) =>
          tailcall(undefine1(key)).flatMap(
            keyv =>
              trampolineM((c: (List[SchemeValue], List[SchemeExp])) =>
                            c match {
                              case (vs, body) =>
                                tailcall(undefineBody(body)).map(bodyv => (vs, bodyv))
                          },
                          clauses).flatMap(clausesv =>
                tailcall(undefineBody(default)).map(defaultv =>
                  SchemeCase(keyv, clausesv, defaultv, pos))))
        case SchemeAnd(args, pos) =>
          trampolineM(undefine1, args).map(argsv => SchemeAnd(argsv, pos))
        case SchemeOr(args, pos) => trampolineM(undefine1, args).map(argsv => SchemeOr(argsv, pos))
        case SchemeDo(vars, test, finals, commands, pos) =>
          trampolineM(
            (x: (Identifier, SchemeExp, Option[SchemeExp])) =>
              x match {
                case (id, init, step) =>
                  tailcall(undefine1(init)).flatMap(initv =>
                    step match {
                      case Some(s) =>
                        undefine1(s).map(stepv => {
                          val x: Option[SchemeExp] = Some(stepv)
                          (id, initv, x)
                        })
                      case None =>
                        val x: Option[SchemeExp] = None
                        done((id, initv, x))
                  })
            },
            vars
          ).flatMap(varsv =>
            tailcall(undefine1(test)).flatMap(testv =>
              tailcall(undefineBody(finals)).flatMap(finalsv =>
                tailcall(undefineBody(commands).map(commandsv =>
                  SchemeDo(varsv, testv, finalsv, commandsv, pos))))))
        case SchemeVar(id)             => done(SchemeVar(id))
        case SchemeQuoted(quoted, pos) => done(SchemeQuoted(quoted, pos))
        case SchemeValue(value, pos)   => done(SchemeValue(value, pos))
      }
      exp2.flatMap(e2 => tailcall(undefineBody(rest)).flatMap(e3 => done(e2 :: e3)))
    }
  }
}

object SchemeParser {

  /**
    * Compiles a s-expression into a scheme expression
    */
  def compile(exp: SExp): SchemeExp = SchemeCompiler.compile(exp)

  /**
    * Performs alpha-renaming to ensure that every variable has a unique name
    */
  def rename(exp: SchemeExp): SchemeExp = SchemeRenamer.rename(exp)

  /**
    * Replace defines in a program (a list of expressions) by a big letrec as a single expression
    */
  def undefine(exps: List[SchemeExp]): SchemeExp = SchemeUndefiner.undefine(exps)

  /**
    * Parse a string representing a Scheme program
    */
  def parse(s: String): SchemeExp = undefine(SExpParser.parse(s).map(compile _))
}

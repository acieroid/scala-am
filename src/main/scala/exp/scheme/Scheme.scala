/**
 * Abstract syntax of Scheme programs (probably far from complete)
 */
trait SchemeExp {
  val pos: Position
}
object SchemeExp {
  implicit val isExp: Expression[SchemeExp] = new Expression[SchemeExp] {
    def pos(e: SchemeExp) = e.pos
  }
}

/**
 * A lambda expression: (lambda (args...) body...)
 * Not supported: "rest"-arguments, of the form (lambda arg body), or (lambda (arg1 . args) body...)
 */
case class SchemeLambda(args: List[Identifier], body: List[SchemeExp], pos: Position) extends SchemeExp {
  require(body.nonEmpty)
  override def toString = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(lambda ($a) $b)"
  }
}

/**
 * A function call: (f args...)
 */
case class SchemeFuncall(f: SchemeExp, args: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    if (args.isEmpty) {
      s"($f)"
    } else {
      val a = args.mkString(" ")
      s"($f $a)"
    }
  }
}
/**
 * An if statement: (if cond cons alt)
 * If without alt clauses need to be encoded with an empty begin as alt clause
 */
case class SchemeIf(cond: SchemeExp, cons: SchemeExp, alt: SchemeExp, pos: Position) extends SchemeExp {
  override def toString = s"(if $cond $cons $alt)"
}
/**
 * Let-bindings: (let ((v1 e1) ...) body...)
 */
case class SchemeLet(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let ($bi) $bo)"
  }
}
/**
 * Let*-bindings: (let* ((v1 e1) ...) body...)
 */
case class SchemeLetStar(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let* ($bi) $bo)"
  }
}
/**
 * Letrec-bindings: (letrec ((v1 e1) ...) body...)
 */
case class SchemeLetrec(bindings: List[(Identifier, SchemeExp)], body: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(letrec ($bi) $bo)"
  }
}
/**
 * A set! expression: (set! variable value)
 */
case class SchemeSet(variable: Identifier, value: SchemeExp, pos: Position) extends SchemeExp {
  override def toString = s"(set! $variable $value)"
}
/**
 * A begin clause: (begin body...)
 */
case class SchemeBegin(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    val body = exps.mkString(" ")
    s"(begin $body)"
  }
}
/**
 * A cond expression: (cond (test1 body1...) ...)
 */
case class SchemeCond(clauses: List[(SchemeExp, List[SchemeExp])], pos: Position) extends SchemeExp {
  override def toString = {
    val c = clauses.map({ case (cond, cons) => {
      val b = cons.mkString(" ")
      s"($cond $b)"
    }}).mkString(" ")
    s"(cond $c)"
  }
}

/**
 * A case expression: (case key ((vals1...) body1...) ... (else default...))
 */
case class SchemeCase(key: SchemeExp, clauses: List[(List[SchemeValue], List[SchemeExp])], default: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    val c = clauses.map({ case (datums, cons) => {
      val d = datums.mkString(" ")
      val b = cons.mkString(" ")
      s"(($d) $b)"
    }}).mkString(" ")
    if (default.isEmpty) {
      s"(case $key $c)"
    } else {
      s"(case $key $c (else ${default.mkString(" ")}))"
    }
  }
}

/**
 * An and expression: (and exps...)
 */
case class SchemeAnd(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    val e = exps.mkString(" ")
    s"(and $e)"
  }
}
/**
 * An or expression: (or exps...)
 */
case class SchemeOr(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    val e = exps.mkString(" ")
    s"(or $e)"
  }
}
/**
 * A variable definition: (define name value)
 */
case class SchemeDefineVariable(name: Identifier, value: SchemeExp, pos: Position) extends SchemeExp {
  override def toString = s"(define $name $value)"
}
/**
 * A function definition: (define (name args...) body...)
 */
case class SchemeDefineFunction(name: Identifier, args: List[Identifier], body: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(define ($name $a) $b)"
  }
}
/**
 * An identifier: name
 */
case class SchemeVar(id: Identifier) extends SchemeExp {
  val pos = id.pos
  override def toString = id.name
}

/**
 * A quoted expression: '(foo (bar baz))
 *  The quoted expression is *not* converted to a Scheme expression, and remains
 * a simple s-expression, because that's exactly what it should be.
 */
case class SchemeQuoted(quoted: SExp, pos: Position) extends SchemeExp {
  override def toString = s"'$quoted"
}

/**
 * A literal value (number, symbol, string, ...)
 */
case class SchemeValue(value: Value, pos: Position) extends SchemeExp {
  override def toString = value.toString
}

/**
 * Compare-and-swap, concurrency synchronization primitive.
 */
case class SchemeCas(variable: Identifier, eold: SchemeExp, enew: SchemeExp, pos: Position) extends SchemeExp {
  override def toString = s"(cas $variable $eold $enew)"
}

/**
 * Compare-and-swap on a vector
 */
case class SchemeCasVector(variable: Identifier, index: SchemeExp, eold: SchemeExp, enew: SchemeExp, pos: Position) extends SchemeExp {
  override def toString = s"(cas-vector $variable $index $eold $enew)"
}

/**
 * Acquire a lock
 */
case class SchemeAcquire(exp: SchemeExp, pos: Position) extends SchemeExp {
  override def toString = s"(acquire $exp)"
}

/**
 * Release a lock
 */
case class SchemeRelease(exp: SchemeExp, pos: Position) extends SchemeExp {
  override def toString = s"(release $exp)"
}

/**
 * Spawn a new thread to compute an expression
 */
case class SchemeSpawn(exp: SchemeExp, pos: Position) extends SchemeExp {
  override def toString = s"(spawn $exp)"
}

/**
 * Wait for a thread (whose identifier is the value of exp) to terminate
 */
case class SchemeJoin(exp: SchemeExp, pos: Position) extends SchemeExp {
  override def toString = s"(join $exp)"
}

/**
 * Send a message to an actor
 */
case class SchemeSend(target: SchemeExp, message: Identifier, args: List[SchemeExp], pos: Position) extends SchemeExp {
  val a = args.mkString(" ")
  override def toString = if (args.isEmpty) s"(send $target $message)" else s"(send $target $message $a)"
}

/**
 * Create an actor from a behavior
 */
case class SchemeCreate(actor: SchemeExp, args: List[SchemeExp], pos: Position) extends SchemeExp {
  val a = (actor :: args).mkString(" ")
  override def toString = s"(create $a)"
}

/**
 * Change the behavior of the current actor
 */
case class SchemeBecome(actor: SchemeExp, args: List[SchemeExp], pos: Position) extends SchemeExp {
  val a = (actor :: args).mkString(" ")
  override def toString = s"(become $a)"
}

/**
 * Terminate the execution of an actor
 */
case class SchemeTerminate(pos: Position) extends SchemeExp {
  override def toString = "(terminate)"
}

/**
 * Define a behavior
 */
case class SchemeActor(name: String, xs: List[Identifier], defs: Map[String, (List[Identifier], List[SchemeExp])], pos: Position) extends SchemeExp {
  val xss = xs.mkString(" ")
  val defss = defs.toList.map({ case (msg, (args, body)) =>
    val argss = args.mkString(" ")
    val bodys = body.mkString(" ")
    s"($msg ($argss) $bodys)"
  }).mkString(" ")
  override def toString = s"(actor $name ($xss) ($defss))"
}

/**
 * Object that provides a method to compile an s-expression into a Scheme expression
 */
object SchemeCompiler {
  /**
    * Reserved keywords
    */
  val reserved: List[String] = List("lambda", "if", "let", "let*", "letrec", "cond", "case", "set!", "begin", "define", "cas", "acquire", "release", "cas-vector")

  def compile(exp: SExp): SchemeExp = exp match {
    case SExpPair(SExpId(Identifier("quote", _)), SExpPair(quoted, SExpValue(ValueNil, _), _), _) =>
      compile(SExpQuoted(quoted, exp.pos))
    case SExpPair(SExpId(Identifier("quote", _)), _, _) =>
      throw new Exception(s"Invalid Scheme quote: $exp (${exp.pos})")
    case SExpPair(SExpId(Identifier("lambda", _)),
      SExpPair(args, SExpPair(first, rest, _), _), _) =>
      SchemeLambda(compileArgs(args), compile(first) :: compileBody(rest), exp.pos)
    case SExpPair(SExpId(Identifier("lambda", _)), _, _) =>
      throw new Exception(s"Invalid Scheme lambda: $exp (${exp.pos})")
    case SExpPair(SExpId(Identifier("if", _)),
      SExpPair(cond, SExpPair(cons, SExpPair(alt, SExpValue(ValueNil, _), _), _), _), _) =>
      SchemeIf(compile(cond), compile(cons), compile(alt), exp.pos)
    case SExpPair(SExpId(Identifier("if", _)),
      SExpPair(cond, SExpPair(cons, SExpValue(ValueNil, _), _), _), _) =>
      /* Empty else branch is replaced by #f (R5RS states it's unspecified) */
      SchemeIf(compile(cond), compile(cons), SchemeValue(ValueBoolean(false), exp.pos), exp.pos)
    case SExpPair(SExpId(Identifier("if", _)), _, _) =>
        throw new Exception(s"Invalid Scheme if: $exp (${exp.pos})")
    case SExpPair(SExpId(Identifier("let", _)),
      SExpPair(bindings, SExpPair(first, rest, _), _), _) =>
      SchemeLet(compileBindings(bindings), compile(first) :: compileBody(rest), exp.pos)
    case SExpPair(SExpId(Identifier("let", _)), _, _) =>
      throw new Exception(s"Invalid Scheme let: $exp")
    case SExpPair(SExpId(Identifier("let*", _)),
      SExpPair(bindings, SExpPair(first, rest, _), _), _) =>
      SchemeLetStar(compileBindings(bindings), compile(first) :: compileBody(rest), exp.pos)
    case SExpPair(SExpId(Identifier("let*", _)), _, _) =>
      throw new Exception(s"Invalid Scheme let*: $exp")
    case SExpPair(SExpId(Identifier("letrec", _)),
      SExpPair(bindings, SExpPair(first, rest, _), _), _) =>
      SchemeLetrec(compileBindings(bindings), compile(first) :: compileBody(rest), exp.pos)
    case SExpPair(SExpId(Identifier("letrec", _)), _, _) =>
      throw new Exception(s"Invalid Scheme letrec: $exp")
    case SExpPair(SExpId(Identifier("set!", _)),
      SExpPair(SExpId(v), SExpPair(value, SExpValue(ValueNil, _), _), _), _) =>
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
      SExpPair(SExpId(name), SExpPair(value, SExpValue(ValueNil, _), _), _), _) =>
      SchemeDefineVariable(name, compile(value), exp.pos)
    case SExpPair(SExpId(Identifier("define", _)),
      SExpPair(SExpPair(SExpId(name), args, _),
        SExpPair(first, rest, _), _), _) =>
      SchemeDefineFunction(name, compileArgs(args), compile(first) :: compileBody(rest), exp.pos)

    case SExpPair(SExpId(Identifier("cas", _)),
      SExpPair(SExpId(variable),
        SExpPair(eold, SExpPair(enew, SExpValue(ValueNil, _), _), _), _), _) =>
      SchemeCas(variable, compile(eold), compile(enew), exp.pos)
    case SExpPair(SExpId(Identifier("cas", _)), _, _) =>
      throw new Exception(s"Invalid Scheme cas: $exp")
    case SExpPair(SExpId(Identifier("cas-vector", _)),
      SExpPair(SExpId(variable),
        SExpPair(index, SExpPair(eold, SExpPair(enew, SExpValue(ValueNil, _), _), _), _), _), _) =>
      SchemeCasVector(variable, compile(index), compile(eold), compile(enew), exp.pos)
    case SExpPair(SExpId(Identifier("cas-vector", _)), _, _) =>
      throw new Exception(s"Indavil Scheme cas-vector: $exp")
    case SExpPair(SExpId(Identifier("acquire", _)),
      SExpPair(exp, SExpValue(ValueNil, _), _), _) =>
      SchemeAcquire(compile(exp), exp.pos)
    case SExpPair(SExpId(Identifier("acquire", _)), _, _) =>
      throw new Exception(s"Invalid Scheme acquire: $exp")
    case SExpPair(SExpId(Identifier("release", _)),
      SExpPair(exp, SExpValue(ValueNil, _), _), _) =>
      SchemeRelease(compile(exp), exp.pos)
  case SExpPair(SExpId(Identifier("release", _)), _, _) =>
      throw new Exception(s"Invalid Scheme release: $exp")
    case SExpPair(SExpId(Identifier("spawn", _)),
      SExpPair(exp, SExpValue(ValueNil, _), _), _) =>
      SchemeSpawn(compile(exp), exp.pos)
    case SExpPair(SExpId(Identifier("spawn", _)), _, _) =>
      throw new Exception(s"Invalid Scheme spawn: $exp")
    case SExpPair(SExpId(Identifier("join", _)),
      SExpPair(exp, SExpValue(ValueNil, _), _), _) =>
      SchemeJoin(compile(exp), exp.pos)
    case SExpPair(SExpId(Identifier("join", _)), _, _) =>
      throw new Exception(s"Invalid Scheme join: $exp")

    case SExpPair(SExpId(Identifier("send", _)), SExpPair(target, SExpPair(SExpId(message), args, _), _), _) =>
      SchemeSend(compile(target), message, compileBody(args), exp.pos)
    case SExpPair(SExpId(Identifier("send", _)), _, _) =>
      throw new Exception(s"Invalid Scheme send: $exp (${exp.pos})")
    case SExpPair(SExpId(Identifier("become", _)), SExpPair(actor, args, _), _) =>
      SchemeBecome(compile(actor), compileBody(args), exp.pos)
    case SExpPair(SExpId(Identifier("become", _)), _, _) =>
      throw new Exception(s"Invalid Scheme become: $exp (${exp.pos})")
    case SExpPair(SExpId(Identifier("create", _)), SExpPair(actor, args, _), _) =>
      SchemeCreate(compile(actor), compileBody(args), exp.pos)
    case SExpPair(SExpId(Identifier("create", _)), _, _) =>
      throw new Exception(s"Invalid Scheme create: $exp (${exp.pos})")
    case SExpPair(SExpId(Identifier("terminate", _)), SExpValue(ValueNil, _), _) =>
      SchemeTerminate(exp.pos)
    case SExpPair(SExpId(Identifier("actor", _)), SExpPair(SExpValue(ValueString(name), _), SExpPair(args, defs, _), _), _) =>
      SchemeActor(name, compileArgs(args), compileActorDefs(defs).map({ case (n, v) => (n.name, v) }).toMap, exp.pos)
    case SExpPair(SExpId(Identifier("actor", _)), _, _) =>
      throw new Exception(s"Invalid Scheme actor: $exp (${exp.pos})")

    case SExpPair(f, args, _) =>
      SchemeFuncall(compile(f), compileBody(args), exp.pos)
    case SExpId(v) => if (reserved.contains(v.name)) {
      throw new Exception(s"Invalid Scheme identifier (reserved): $exp")
    } else {
      SchemeVar(v)
    }
    case SExpValue(value, _) => SchemeValue(value, exp.pos)
    case SExpQuoted(quoted, _) => SchemeQuoted(quoted, exp.pos)
  }

  def compileActorDefs(defs: SExp): List[(Identifier, (List[Identifier], List[SchemeExp]))] = defs match {
    case SExpPair(SExpPair(SExpId(m), SExpPair(args, body, _), _), rest, _) =>
      (m, (compileArgs(args), compileBody(body))) :: compileActorDefs(rest)
    case SExpValue(ValueNil, _) => Nil
    case _ => throw new Exception(s"Invalid Scheme actor definition: $defs (${defs.pos})")
  }

  def compileArgs(args: SExp): List[Identifier] = args match {
    case SExpPair(SExpId(id), rest, _) => id :: compileArgs(rest)
    case SExpValue(ValueNil, _) => Nil
    case _ => throw new Exception(s"Invalid Scheme argument list: $args (${args.pos})")
  }

  def compileBody(body: SExp): List[SchemeExp] = body match {
    case SExpPair(exp, rest, _) => compile(exp) :: compileBody(rest)
    case SExpValue(ValueNil, _) => Nil
    case _ => throw new Exception(s"Invalid Scheme body: $body (${body.pos})")
  }

  def compileBindings(bindings: SExp): List[(Identifier, SchemeExp)] = bindings match {
    case SExpPair(SExpPair(SExpId(v),
      SExpPair(value, SExpValue(ValueNil, _), _), _), rest, _) =>
      if (reserved.contains(v.name)) {
        throw new Exception(s"Invalid Scheme identifier (reserved): $v (${bindings.pos})")
      } else {
        (v, compile(value)) :: compileBindings(rest)
      }
    case SExpValue(ValueNil, _) => Nil
    case _ => throw new Exception(s"Invalid Scheme bindings: $bindings (${bindings.pos})")
  }

  def compileCondClauses(clauses: SExp): List[(SchemeExp, List[SchemeExp])] = clauses match {
    case SExpPair(SExpPair(SExpId(Identifier("else", _)), SExpPair(first, rest, _), _),
                  SExpValue(ValueNil, _), _) =>
      List((SchemeValue(ValueBoolean(true), clauses.pos), compile(first) :: compileBody(rest)))
    case SExpPair(SExpPair(cond, SExpPair(first, rest, _), _), restClauses, _) =>
      (compile(cond), compile(first) :: compileBody(rest)) :: compileCondClauses(restClauses)
    case SExpPair(SExpPair(cond, SExpValue(ValueNil, _), _), restClauses, _) =>
      (compile(cond), Nil) :: compileCondClauses(restClauses)
    case SExpValue(ValueNil, _) => Nil
    case _ => throw new Exception(s"Invalid Scheme cond clauses: $clauses ${clauses.pos})")
  }

  def compileCaseClauses(clauses: SExp): (List[(List[SchemeValue], List[SchemeExp])], List[SchemeExp]) = clauses match {
    case SExpPair(SExpPair(SExpId(Identifier("else", _)), SExpPair(first, rest, _), _),
                  SExpValue(ValueNil, _), _) =>
      (List(), compile(first) :: compileBody(rest))
    case SExpPair(SExpPair(objects, body, _), restClauses, _) =>
      val (compiled, default) = compileCaseClauses(restClauses)
      ((compileCaseObjects(objects), compileBody(body)) :: compiled, default)
    case SExpValue(ValueNil, _) => (Nil, Nil)
    case _ => throw new Exception(s"Invalid Scheme case clauses: $clauses (${clauses.pos})")
  }

  def compileCaseObjects(objects: SExp): List[SchemeValue] = objects match {
    case SExpPair(SExpValue(v, _), rest, _) =>
      SchemeValue(v, objects.pos) :: compileCaseObjects(rest)
    case SExpPair(SExpId(id), rest, _) =>
      /* identifiers in case expressions are treated as symbols */
      SchemeValue(ValueSymbol(id.name), id.pos) :: compileCaseObjects(rest)
    case SExpValue(ValueNil, _) => Nil
    case _ => throw new Exception(s"Invalid Scheme case objects: $objects (${objects.pos})")
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
        case (args1, names1, count1) => renameList(body, names1, count1) match {
          case (body1, count2) => (SchemeLambda(args1, body1, pos), count2)
        }
      }
    case SchemeFuncall(f, args, pos) =>
      rename(f, names, count) match {
        case (f1, count1) => renameList(args, names, count1) match {
          case (args1, count2) => (SchemeFuncall(f1, args1, pos), count2)
        }
      }
    case SchemeIf(cond, cons, alt, pos) =>
      rename(cond, names, count) match {
        case (cond1, count1) => rename(cons, names, count1) match {
          case (cons1, count2) => rename(alt, names, count2) match {
            case (alt1, count3) => (SchemeIf(cond1, cons1, alt1, pos), count3)
          }
        }
      }
    case SchemeLet(bindings, body, pos) =>
      countl(bindings.map(_._1), names, count) match {
        /* Use old names for expressions of bindings */
        case (variables, names1, count1) => renameList(bindings.map(_._2), names, count1) match {
          case (exps, count2) => renameList(body, names1, count2) match {
            case (body1, count3) => (SchemeLet(variables.zip(exps), body1, pos), count3)
          }
        }
      }
    case SchemeLetStar(bindings, body, pos) =>
      renameLetStarBindings(bindings, names, count) match {
        case (bindings1, names1, count1) => renameList(body, names1, count1) match {
          case (body1, count2) => (SchemeLetStar(bindings1, body1, pos), count2)
        }
      }
    case SchemeLetrec(bindings, body, pos) =>
      countl(bindings.map(_._1), names, count) match {
        /* Use new names for expressions of bindings */
        case (variables, names1, count1) => renameList(bindings.map(_._2), names1, count1) match {
          case (exps, count2) => renameList(body, names1, count2) match {
            case (body1, count3) => (SchemeLetrec(variables.zip(exps), body1, pos), count3)
          }
        }
      }
    case SchemeSet(variable, value, pos) =>
      rename(value, names, count) match {
        case (value1, count1) => (SchemeSet(names.get(variable.name) match {
          case Some(n) => Identifier(n, variable.pos)
          case None => variable
        }, value1, pos), count1)
      }
    case SchemeBegin(body, pos) =>
      renameList(body, names, count) match {
        case (body1, count1) => (SchemeBegin(body1, pos), count1)
      }
    case SchemeCond(clauses, pos) =>
      clauses.foldLeft((List[(SchemeExp, List[SchemeExp])](), count))(
        (st: (List[(SchemeExp, List[SchemeExp])], CountMap),
          cl: (SchemeExp, List[SchemeExp])) =>
        (st, cl) match {
          case ((l, cs), (e, body)) => rename(e, names, cs) match {
            case (e1, count1) => renameList(body, names, count1) match {
              case (body1, count2) =>
                ((e1, body1) :: l, count2)
              }
            }
          }) match {
        case (l, count1) => (SchemeCond(l.reverse, pos), count1)
      }
    case SchemeCase(exp, clauses, default, pos) =>
      rename(exp, names, count) match {
        case (exp1, count1) => clauses.foldLeft((List[(List[SchemeValue], List[SchemeExp])](), count))(
          (st: (List[(List[SchemeValue], List[SchemeExp])], CountMap),
            cl: (List[SchemeValue], List[SchemeExp])) =>
          (st, cl) match {
            case ((l, cs), (objs, body)) => renameList(body, names, cs) match {
              case (body1, count1) => ((objs, body1) :: l, count1)
            }
          }) match {
          case (l, count1) => renameList(default, names, count1) match {
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
        case (args1, names1, count1) => renameList(body, names1, count1) match {
          case (body1, count2) =>
            (SchemeDefineFunction(name, args1, body1, pos), count2)
        }
      }
    case SchemeQuoted(quoted, pos) =>
      (SchemeQuoted(quoted, pos), count)
    case SchemeVar(id) => names.get(id.name) match {
      case Some(n) => (SchemeVar(Identifier(n, id.pos)), count)
      case None => (SchemeVar(Identifier(id.name, id.pos)), count) /* keep original name */
    }
    case SchemeValue(v, pos) =>
      (SchemeValue(v, pos), count)
    case _ => throw new Exception(s"Unhandled expression in renamer: $exp")
  }

  /** Renames a list of expressions executed sequentially (eg. within a begin) */
  def renameList(exps: List[SchemeExp], names: NameMap, count: CountMap): (List[SchemeExp], CountMap) = exps match {
    case exp :: rest =>
      val (exp1, count1) = rename(exp, names, count)
      val (rest1, count2) = renameList(rest, names, count1)
      (exp1 :: rest1, count2)
    case Nil => (Nil, count)
  }

  def renameLetStarBindings(bindings: List[(Identifier, SchemeExp)], names: NameMap, count: CountMap): (List[(Identifier, SchemeExp)], NameMap, CountMap) = bindings match {
    case (v, e) :: rest =>
      count1(v, names, count) match {
        /* use old names, as with a let* the variable is not yet bound in its
         * definition */
        case (v1, names1, count1) => rename(e, names, count1) match {
          case (e1, count2) => renameLetStarBindings(rest, names1, count2) match {
            case (rest1, names2, count3) =>
              ((v1, e1) :: rest1, names2, count3)
          }
        }
      }
    case Nil => (Nil, names, count)
  }

  /** To be called when a new variable is introduced in the scope. Adds it to the
    * name map and count map */
  def count1(variable: Identifier, names: NameMap, count: CountMap): (Identifier, NameMap, CountMap) = {
    val c: Int  = count.get(variable.name) match {
      case Some(x) => x + 1
      case None => 0
    }
    val n = s"_$variable$c"
    (Identifier(n, variable.pos),
      names + (variable.name -> n), count + (variable.name -> c))
  }

  /** Same as count1 but for a list of variables */
  def countl(variables: List[Identifier], names: NameMap, count: CountMap): (List[Identifier], NameMap, CountMap) =
    variables.foldLeft((List[Identifier](), names, count))(
      (st: (List[Identifier], NameMap, CountMap), v: Identifier) => st match {
      case (l, ns, cs) => count1(v, ns, cs) match {
        case (v1, ns1, cs1) => ((v1 :: l), ns1, cs1)
      }}) match {
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
  def undefine(exps: List[SchemeExp]): SchemeExp =
    undefine(exps, List())

  def undefine(exps: List[SchemeExp], defs: List[(Identifier, SchemeExp)]): SchemeExp = exps match {
    case Nil => SchemeBegin(Nil, Position.none)
    case SchemeDefineFunction(name, args, body, pos) :: rest => undefine(SchemeDefineVariable(name, SchemeLambda(args, undefineBody(body), exps.head.pos), pos) :: rest, defs)
    case SchemeDefineVariable(name, value, _) :: rest => undefine(rest, (name, value) :: defs)
    case _ :: _ => if (defs.isEmpty) {
      undefineBody(exps) match {
        case Nil => SchemeBegin(Nil, Position.none)
        case exp :: Nil => exp
        case exps => SchemeBegin(exps, exps.head.pos)
      }
    } else {
      SchemeLetrec(defs.reverse, undefineBody(exps), exps.head.pos)
    }
  }

  def undefine1(exp: SchemeExp): SchemeExp = undefine(List(exp))

  def undefineBody(exps: List[SchemeExp]): List[SchemeExp] = exps match {
    case Nil => Nil
    case SchemeDefineFunction(_, _, _, _) :: _ => List(undefine(exps, List()))
    case SchemeDefineVariable(_, _, _) :: _ => List(undefine(exps, List()))
    case exp :: rest => {
      val exp2 = exp match {
        case SchemeLambda(args, body, pos) => SchemeLambda(args, undefineBody(body), pos)
        case SchemeFuncall(f, args, pos) => SchemeFuncall(undefine1(f), args.map(undefine1), pos)
        case SchemeIf(cond, cons, alt, pos) => SchemeIf(undefine1(cond), undefine1(cons), undefine1(alt), pos)
        case SchemeLet(bindings, body, pos) => SchemeLet(bindings.map({ case (b, v) => (b, undefine1(v)) }), undefineBody(body), pos)
        case SchemeLetStar(bindings, body, pos) => SchemeLetStar(bindings.map({ case (b, v) => (b, undefine1(v)) }), undefineBody(body), pos)
        case SchemeLetrec(bindings, body, pos) => SchemeLetrec(bindings.map({ case (b, v) => (b, undefine1(v)) }), undefineBody(body), pos)
        case SchemeSet(variable, value, pos) => SchemeSet(variable, undefine1(value), pos)
        case SchemeBegin(exps, pos) => SchemeBegin(undefineBody(exps), pos)
        case SchemeCond(clauses, pos) => SchemeCond(clauses.map({ case (cond, body) => (undefine1(cond), undefineBody(body)) }), pos)
        case SchemeCase(key, clauses, default, pos) => SchemeCase(undefine1(key), clauses.map({ case (vs, body) => (vs, undefineBody(body)) }), undefineBody(default), pos)
        case SchemeAnd(args, pos) => SchemeAnd(args.map(undefine1), pos)
        case SchemeOr(args, pos) => SchemeOr(args.map(undefine1), pos)
        case SchemeVar(id) => SchemeVar(id)
        case SchemeQuoted(quoted, pos) => SchemeQuoted(quoted, pos)
        case SchemeValue(value, pos) => SchemeValue(value, pos)
        case SchemeCas(variable, eold, enew, pos) => SchemeCas(variable, undefine1(eold), undefine1(enew), pos)
        case SchemeCasVector(variable, index, eold, enew, pos) => SchemeCasVector(variable, undefine1(index), undefine1(eold), undefine1(enew), pos)
        case SchemeAcquire(exp, pos) => SchemeAcquire(undefine1(exp), pos)
        case SchemeRelease(exp, pos) => SchemeRelease(undefine1(exp), pos)
        case SchemeSpawn(exp, pos) => SchemeSpawn(undefine1(exp), pos)
        case SchemeJoin(exp, pos) => SchemeJoin(undefine1(exp), pos)
        case SchemeSend(target, message, args, pos) => SchemeSend(undefine1(target), message, undefineBody(args), pos)
        case SchemeCreate(beh, args, pos) => SchemeCreate(undefine1(beh), undefineBody(args), pos)
        case SchemeBecome(beh, args, pos) => SchemeBecome(undefine1(beh), undefineBody(args), pos)
        case SchemeActor(name, xs, defs, pos) => SchemeActor(name, xs, defs.map({ case (name, (args, body)) => (name, (args, undefineBody(body))) }), pos)
        case SchemeTerminate(pos) => SchemeTerminate(pos)
      }
      exp2 :: undefineBody(rest)
    }
  }
}

object Scheme {
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

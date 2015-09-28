/**
  * Abstract syntax of Scheme programs (probably far from complete)
  */

trait SchemeExp extends scala.util.parsing.input.Positional
case class SchemeLambda(args: List[String], body: List[SchemeExp]) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeLambda] && pos == that.asInstanceOf[SchemeLambda].pos
  override def toString() = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(lambda ($a) $b)"
  }
}
case class SchemeFuncall(f: SchemeExp, args: List[SchemeExp]) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeFuncall] && pos == that.asInstanceOf[SchemeFuncall].pos
  override def toString() = {
    if (args.isEmpty) {
      s"($f)"
    } else {
      val a = args.mkString(" ")
      s"($f $a)"
    }
  }
}
case class SchemeIf(cond: SchemeExp, cons: SchemeExp, alt: SchemeExp) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeIf] && pos == that.asInstanceOf[SchemeIf].pos && super.equals(that)
  override def toString() = s"(if $cond $cons $alt)"
}
case class SchemeLet(bindings: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeLet] && pos == that.asInstanceOf[SchemeLet].pos && super.equals(that)
  override def toString() = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let ($bi) $bo)"
  }
}
case class SchemeLetStar(bindings: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeLetStar] && pos == that.asInstanceOf[SchemeLetStar].pos && super.equals(that)
  override def toString() = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let* ($bi) $bo)"
  }
}
case class SchemeLetrec(bindings: List[(String, SchemeExp)], body: List[SchemeExp]) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeLetrec] && pos == that.asInstanceOf[SchemeLetrec].pos && super.equals(that)
  override def toString() = {
    val bi = bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(letrec ($bi) $bo)"
  }
}
case class SchemeSet(variable: String, value: SchemeExp) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeSet] && pos == that.asInstanceOf[SchemeSet].pos && super.equals(that)
  override def toString() = s"(set! $variable $value)"
}
case class SchemeBegin(exps: List[SchemeExp]) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeBegin] && pos == that.asInstanceOf[SchemeBegin].pos && super.equals(that)
  override def toString() = {
    val body = exps.mkString(" ")
    s"(begin $body)"
  }
}
case class SchemeCond(clauses: List[(SchemeExp, List[SchemeExp])]) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeCond] && pos == that.asInstanceOf[SchemeCond].pos && super.equals(that)
  override def toString() = {
    val c = clauses.map({ case (cond, cons) => {
      val b = cons.mkString(" ")
      s"($cond $b)"
    }}).mkString(" ")
    s"(cond $c)"
  }
}

case class SchemeCase(key: SchemeExp, clauses: List[(List[SchemeValue], List[SchemeExp])], default: List[SchemeExp]) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeCase] && pos == that.asInstanceOf[SchemeCase].pos && super.equals(that)
  override def toString() = {
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

case class SchemeAnd(exps: List[SchemeExp]) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeAnd] && pos == that.asInstanceOf[SchemeAnd].pos && super.equals(that)
  override def toString() = {
    val e = exps.mkString(" ")
    s"(and $e)"
  }
}
case class SchemeOr(exps: List[SchemeExp]) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeOr] && pos == that.asInstanceOf[SchemeOr].pos && super.equals(that)
  override def toString() = {
    val e = exps.mkString(" ")
    s"(or $e)"
  }
}
case class SchemeDefineVariable(name: String, value: SchemeExp) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeDefineVariable] && pos == that.asInstanceOf[SchemeDefineVariable].pos && super.equals(that)
  override def toString() = s"(define $name $value)"
}
case class SchemeDefineFunction(name: String, args: List[String], body: List[SchemeExp]) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeDefineFunction] && pos == that.asInstanceOf[SchemeDefineFunction].pos && super.equals(that)
  override def toString() = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(define ($name $a) $b)"
  }
}
case class SchemeIdentifier(name: String) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeIdentifier] && pos == that.asInstanceOf[SchemeIdentifier].pos && super.equals(that)
  override def toString() = name
}
case class SchemeQuoted(quoted: SExp) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeQuoted] && pos == that.asInstanceOf[SchemeQuoted].pos && super.equals(that)
  override def toString() = s"'$quoted"
}
case class SchemeValue(value: Value) extends SchemeExp {
  override def equals(that: Any) = that.isInstanceOf[SchemeValue] && pos == that.asInstanceOf[SchemeValue].pos && super.equals(that)
  override def toString() = value.toString
}

object SchemeCompiler {
  /**
    * Reserved keywords
    */
  val reserved: List[String] = List("lambda", "if", "let", "let*", "letrec", "cond", "case", "set!", "begin", "define")

  def compile(exp: SExp): SchemeExp = {
    val exp2 = exp match {
      case SExpPair(SExpIdentifier("lambda"),
        SExpPair(args, SExpPair(first, rest))) =>
        SchemeLambda(compileArgs(args), compile(first) :: compileBody(rest))
      case SExpPair(SExpIdentifier("lambda"), _) =>
        throw new Exception(s"Invalid Scheme lambda: $exp")
      case SExpPair(SExpIdentifier("if"),
        SExpPair(cond, SExpPair(cons, SExpPair(alt, SExpValue(ValueNil()))))) =>
        SchemeIf(compile(cond), compile(cons), compile(alt))
      case SExpPair(SExpIdentifier("if"),
        SExpPair(cond, SExpPair(cons, SExpValue(ValueNil())))) =>
        /* Empty else branch is replaced by #f */
        SchemeIf(compile(cond), compile(cons), SchemeValue(ValueBoolean(false)))
      case SExpPair(SExpIdentifier("if"), _) =>
        throw new Exception(s"Invalid Scheme if: $exp")
      case SExpPair(SExpIdentifier("let"),
        SExpPair(bindings, SExpPair(first, rest))) =>
        SchemeLet(compileBindings(bindings), compile(first) :: compileBody(rest))
      case SExpPair(SExpIdentifier("let"), _) =>
        throw new Exception(s"Invalid Scheme let: $exp")
      case SExpPair(SExpIdentifier("let*"),
        SExpPair(bindings, SExpPair(first, rest))) =>
        SchemeLetStar(compileBindings(bindings), compile(first) :: compileBody(rest))
      case SExpPair(SExpIdentifier("let*"), _) =>
        throw new Exception(s"Invalid Scheme let*: $exp")
      case SExpPair(SExpIdentifier("letrec"),
        SExpPair(bindings, SExpPair(first, rest))) =>
        SchemeLetrec(compileBindings(bindings), compile(first) :: compileBody(rest))
      case SExpPair(SExpIdentifier("letrec"), _) =>
        throw new Exception(s"Invalid Scheme letrec: $exp")
      case SExpPair(SExpIdentifier("set!"),
        SExpPair(SExpIdentifier(variable), SExpPair(value, SExpValue(ValueNil())))) =>
      SchemeSet(variable, compile(value))
      case SExpPair(SExpIdentifier("set!"), _) =>
        throw new Exception(s"Invalid Scheme set!: $exp")
      case SExpPair(SExpIdentifier("begin"), body) =>
        SchemeBegin(compileBody(body))
      case SExpPair(SExpIdentifier("cond"), clauses) =>
        SchemeCond(compileCondClauses(clauses))
      case SExpPair(SExpIdentifier("case"), SExpPair(exp, clauses)) => {
        val (c, d) = compileCaseClauses(clauses)
        SchemeCase(compile(exp), c, d)
      }
      case SExpPair(SExpIdentifier("and"), args) =>
        SchemeAnd(compileBody(args))
      case SExpPair(SExpIdentifier("or"), args) =>
        SchemeOr(compileBody(args))
      case SExpPair(SExpIdentifier("define"),
        SExpPair(SExpIdentifier(name), SExpPair(value, SExpValue(ValueNil())))) =>
        SchemeDefineVariable(name, compile(value))
      case SExpPair(SExpIdentifier("define"),
        SExpPair(SExpPair(SExpIdentifier(name), args),
          SExpPair(first, rest))) =>
        SchemeDefineFunction(name, compileArgs(args), compile(first) :: compileBody(rest))
      case SExpPair(f, args) =>
        SchemeFuncall(compile(f), compileBody(args))
      case SExpIdentifier(name) => if (reserved.contains(name)) {
        throw new Exception(s"Invalid Scheme identifier (reserved): $exp")
      } else {
        SchemeIdentifier(name)
      }
      case SExpValue(value) => SchemeValue(value)
      case SExpQuoted(quoted) => SchemeQuoted(quoted)
    }
    exp2.setPos(exp.pos)
  }

  def compileArgs(args: SExp): List[String] = args match {
    case SExpPair(SExpIdentifier(id), rest) => id :: compileArgs(rest)
    case SExpValue(ValueNil()) => Nil
    case _ => throw new Exception(s"Invalid Scheme argument list: $args")
  }

  def compileBody(body: SExp): List[SchemeExp] = body match {
    case SExpPair(exp, rest) => compile(exp) :: compileBody(rest)
    case SExpValue(ValueNil()) => Nil
    case _ => throw new Exception(s"Invalid Scheme body: $body")
  }

  def compileBindings(bindings: SExp): List[(String, SchemeExp)] = bindings match {
    case SExpPair(SExpPair(SExpIdentifier(name),
      SExpPair(value, SExpValue(ValueNil()))), rest) =>
      (name, compile(value)) :: compileBindings(rest)
    case SExpValue(ValueNil()) => Nil
    case _ => throw new Exception(s"Invalid Scheme bindings: $bindings")
  }

  def compileCondClauses(clauses: SExp): List[(SchemeExp, List[SchemeExp])] = clauses match {
    case SExpPair(SExpPair(SExpIdentifier("else"), SExpPair(first, rest)),
                  SExpValue(ValueNil())) =>
      List((SchemeValue(ValueBoolean(true)).setPos(clauses.pos), compile(first) :: compileBody(rest)))
    case SExpPair(SExpPair(cond, SExpPair(first, rest)), restClauses) =>
      (compile(cond), compile(first) :: compileBody(rest)) :: compileCondClauses(restClauses)
    case SExpPair(SExpPair(cond, SExpValue(ValueNil())), restClauses) =>
      (compile(cond), Nil) :: compileCondClauses(restClauses)
    case SExpValue(ValueNil()) => Nil
    case _ => throw new Exception(s"Invalid Scheme cond clauses: $clauses")
  }

  def compileCaseClauses(clauses: SExp): (List[(List[SchemeValue], List[SchemeExp])], List[SchemeExp]) = clauses match {
    case SExpPair(SExpPair(SExpIdentifier("else"), SExpPair(first, rest)),
                  SExpValue(ValueNil())) =>
      (List(), compile(first) :: compileBody(rest))
    case SExpPair(SExpPair(objects, body), restClauses) =>
      val (compiled, default) = compileCaseClauses(restClauses)
      ((compileCaseObjects(objects), compileBody(body)) :: compiled, default)
    case SExpValue(ValueNil()) => (Nil, Nil)
    case _ => throw new Exception(s"Invalid Scheme case clauses: $clauses")
  }

  def compileCaseObjects(objects: SExp): List[SchemeValue] = objects match {
    case SExpPair(SExpValue(v), rest) =>
      SchemeValue(v).setPos(objects.pos) :: compileCaseObjects(rest)
    case SExpPair(SExpIdentifier(id), rest) =>
      /* identifiers in case expressions are treated as symbols */
      SchemeValue(ValueSymbol(id)).setPos(objects.pos) :: compileCaseObjects(rest)
    case SExpValue(ValueNil()) => Nil
    case _ => throw new Exception(s"Invalid Scheme case objects: $objects")
  }
}

object SchemeRenamer {
  /** Maps each variables to their alpha-renamed version (eg. x -> _x0) */
  type NameMap = Map[String, String]
  /** Map each variables to the number of times it is bound */
  type CountMap = Map[String, Integer]

  def rename(exp: SchemeExp): SchemeExp =
    rename(exp, Map[String, String](), Map[String, Integer]()) match {
      case (e, _) => e
    }

  def rename(exp: SchemeExp, names: NameMap, count: CountMap): (SchemeExp, CountMap) = {
    val (exp2, count2) = exp match {
      case SchemeLambda(args, body) =>
        countl(args, names, count) match {
          case (args1, names1, count1) => renameList(body, names1, count1) match {
            case (body1, count2) => (SchemeLambda(args1, body1), count2)
          }
        }
      case SchemeFuncall(f, args) =>
        rename(f, names, count) match {
          case (f1, count1) => renameList(args, names, count1) match {
            case (args1, count2) => (SchemeFuncall(f1, args1), count2)
          }
        }
      case SchemeIf(cond, cons, alt) =>
        rename(cond, names, count) match {
          case (cond1, count1) => rename(cons, names, count1) match {
            case (cons1, count2) => rename(alt, names, count2) match {
              case (alt1, count3) => (SchemeIf(cond1, cons1, alt1), count3)
            }
          }
      }
      case SchemeLet(bindings, body) =>
        countl(bindings.map(_._1), names, count) match {
          /* Use old names for expressions of bindings */
          case (variables, names1, count1) => renameList(bindings.map(_._2), names, count1) match {
            case (exps, count2) => renameList(body, names1, count2) match {
            case (body1, count3) => (SchemeLet(variables.zip(exps), body1), count3)
            }
          }
        }
      case SchemeLetStar(bindings, body) =>
        renameLetStarBindings(bindings, names, count) match {
          case (bindings1, names1, count1) => renameList(body, names1, count1) match {
            case (body1, count2) => (SchemeLetStar(bindings1, body1), count2)
          }
        }
      case SchemeLetrec(bindings, body) =>
        countl(bindings.map(_._1), names, count) match {
        /* Use new names for expressions of bindings */
          case (variables, names1, count1) => renameList(bindings.map(_._2), names1, count1) match {
            case (exps, count2) => renameList(body, names1, count2) match {
            case (body1, count3) => (SchemeLetrec(variables.zip(exps), body1), count3)
            }
          }
        }
      case SchemeSet(variable, value) =>
        rename(value, names, count) match {
          case (value1, count1) => (SchemeSet(names.get(variable) match {
            case Some(n) => n
          case None => variable
          }, value1), count1)
        }
      case SchemeBegin(body) =>
        renameList(body, names, count) match {
          case (body1, count1) => (SchemeBegin(body1), count1)
        }
      case SchemeCond(clauses) =>
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
          case (l, count1) => (SchemeCond(l.reverse), count1)
      }
      case SchemeCase(exp, clauses, default) =>
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
              case (default1, count2) => (SchemeCase(exp1, l.reverse, default1), count2)
            }
          }
        }
      case SchemeAnd(exps) =>
        renameList(exps, names, count) match {
          case (exps1, count1) => (SchemeAnd(exps1), count1)
        }
      case SchemeOr(exps) =>
      renameList(exps, names, count) match {
        case (exps1, count1) => (SchemeOr(exps1), count1)
      }
      case SchemeDefineVariable(name, value) =>
        /* Keeps name untouched (maybe not correct?) */
        rename(value, names, count) match {
          case (value1, count1) => (SchemeDefineVariable(name, value1), count1)
        }
      case SchemeDefineFunction(name, args, body) =>
      countl(args, names, count) match {
        case (args1, names1, count1) => renameList(body, names1, count1) match {
          case (body1, count2) =>
            (SchemeDefineFunction(name, args1, body1), count2)
        }
      }
      case SchemeQuoted(quoted) =>
        (SchemeQuoted(quoted), count)
      case SchemeIdentifier(name) => names.get(name) match {
      case Some(n) => (SchemeIdentifier(n), count)
        case None => (SchemeIdentifier(name), count) /* keep original name */
      }
      case SchemeValue(v) =>
        (SchemeValue(v), count)
      case _ => throw new Exception(s"Unhandled expression in renamer: $exp")
    }
    (exp2.setPos(exp.pos), count2)
  }


  /** Renames a list of expressions executed sequentially (eg. within a begin) */
  def renameList(exps: List[SchemeExp], names: NameMap, count: CountMap): (List[SchemeExp], CountMap) = exps match {
    case exp :: rest =>
      val (exp1, count1) = rename(exp, names, count)
      val (rest1, count2) = renameList(rest, names, count1)
      (exp1 :: rest1, count2)
    case Nil => (Nil, count)
  }

  def renameLetStarBindings(bindings: List[(String, SchemeExp)], names: NameMap, count: CountMap): (List[(String, SchemeExp)], NameMap, CountMap) = bindings match {
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
  def count1(variable: String, names: NameMap, count: CountMap): (String, NameMap, CountMap) = {
    val c: Int  = count.get(variable) match {
      case Some(x) => x + 1
      case None => 0
    }
    val n = s"_$variable$c"
    (n, names + (variable -> n), count + (variable -> c))
  }

  /** Same as count1 but for a list of variables */
  def countl(variables: List[String], names: NameMap, count: CountMap): (List[String], NameMap, CountMap) =
    variables.foldLeft((List[String](), names, count))(
      (st: (List[String], NameMap, CountMap), v: String) => st match {
      case (l, ns, cs) => count1(v, ns, cs) match {
        case (v1, ns1, cs1) => ((v1 :: l), ns1, cs1)
      }}) match {
      case (l, ns, cs) => (l.reverse, ns, cs)
    }
}

object SchemeUndefiner {
  def undefine(exps: List[SchemeExp]): SchemeExp =
    undefine(exps, List())

  def undefine(exps: List[SchemeExp], defs: List[(String, SchemeExp)]): SchemeExp = exps match {
    case Nil => SchemeBegin(Nil)
    case SchemeDefineFunction(name, args, body) :: rest => undefine(SchemeDefineVariable(name, SchemeLambda(args, body).setPos(exps.head.pos)) :: rest, defs)
    case SchemeDefineVariable(name, value) :: rest => undefine(rest, (name, value) :: defs)
    case _ :: _ => if (defs.isEmpty) {
      undefineBody(exps) match {
        case Nil => SchemeBegin(Nil)
        case exp :: Nil => exp
        case exps => SchemeBegin(exps).setPos(exps.head.pos)
      }
    } else {
      SchemeLetrec(defs.reverse, undefineBody(exps)).setPos(exps.head.pos)
    }
  }

  def undefine1(exp: SchemeExp): SchemeExp = undefine(List(exp))

  def undefineBody(exps: List[SchemeExp]): List[SchemeExp] = exps match {
    case Nil => Nil
    case SchemeDefineFunction(_, _, _) :: _ => List(undefine(exps, List()))
    case SchemeDefineVariable(_, _) :: _ => List(undefine(exps, List()))
    case exp :: rest => {
      val exp2 = exp match {
        case SchemeLambda(args, body) => SchemeLambda(args, undefineBody(body))
        case SchemeFuncall(f, args) => SchemeFuncall(undefine1(f), args.map(undefine1))
        case SchemeIf(cond, cons, alt) => SchemeIf(undefine1(cond), undefine1(cons), undefine1(alt))
        case SchemeLet(bindings, body) => SchemeLet(bindings.map({ case (b, v) => (b, undefine1(v)) }), undefineBody(body))
        case SchemeLetStar(bindings, body) => SchemeLetStar(bindings.map({ case (b, v) => (b, undefine1(v)) }), undefineBody(body))
        case SchemeLetrec(bindings, body) => SchemeLetrec(bindings.map({ case (b, v) => (b, undefine1(v)) }), undefineBody(body))
        case SchemeSet(variable, value) => SchemeSet(variable, undefine1(value))
        case SchemeBegin(exps) => SchemeBegin(undefineBody(exps))
        case SchemeCond(clauses) => SchemeCond(clauses.map({ case (cond, body) => (undefine1(cond), undefineBody(body)) }))
        case SchemeCase(key, clauses, default) => SchemeCase(undefine1(key), clauses.map({ case (vs, body) => (vs, undefineBody(body)) }), undefineBody(default)).setPos(exps.head.pos)
        case SchemeAnd(args) => SchemeAnd(args.map(undefine1)).setPos(exps.head.pos)
        case SchemeOr(args) => SchemeOr(args.map(undefine1)).setPos(exps.head.pos)
        case SchemeIdentifier(name) => SchemeIdentifier(name).setPos(exps.head.pos)
        case SchemeQuoted(quoted) => SchemeQuoted(quoted).setPos(exps.head.pos)
        case SchemeValue(value) => SchemeValue(value).setPos(exps.head.pos)
      }
      exp2.setPos(exp.pos) :: undefineBody(rest)
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
  def parseString(s: String): SchemeExp = {
    undefine(SExpParser.parse(s).map(compile _))
  }

  /**
   * Parse a Scheme file into a single Scheme expression (replacing defines by let bindings)
   */
  def parse(file: String): SchemeExp = {
    val f = scala.io.Source.fromFile(file)
    val content = f.getLines.mkString("\n")
    f.close()
    parseString(content)
  }
}

/**
 * Abstract syntax of ParSimple programs. A ParSimple program has the following form:
 * (threads (vars) (threads) res)
 * where vars are bindings of variables shared among the different threads, and
 * threads is the binding of each threads to its code, consisting of
 * assignments: (= var val), and res is a variable evaluated after each thread
 * has finished, and is the final result of the program.
 *
 * Example:
 * (threads ((x 0) (y 0))
 *   ((= x 1) (= x 2))
 *   ((= y 1) (= x 3))
 *   x)
 * Possible results: 2 or 3
 *
 * This language is useful to test simple programs that have a fixed number of
 * threads, and is another example of how to support a new language in this
 * framework.
 */

trait ParSimpleExp extends scala.util.parsing.input.Positional

case class ParSimpleProgram(vars: List[(String, Value)], threads: List[(String, ParSimpleThreadCode)], variable: ParSimpleVariable) extends ParSimpleExp {
  override def equals(that: Any) = that.isInstanceOf[ParSimpleProgram] && pos == that.asInstanceOf[ParSimpleProgram].pos && super.equals(that)
  override def toString = {
    val varsstr = vars.map({ case (variable, value) => s"($variable $value)"}).mkString(" ")
    val threadsstr = threads.map({ case (name, code) => s"($name $code)" }).mkString(" ")
    s"(threads ($varsstr) ($threadsstr) $variable)"
  }
}

case class ParSimpleThreadCode(code: List[ParSimpleAssignment]) extends ParSimpleExp {
  override def equals(that: Any) = that.isInstanceOf[ParSimpleThreadCode] && pos == that.asInstanceOf[ParSimpleThreadCode].pos && super.equals(that)
  override def toString = "(" + code.mkString(" ") + ")"
}
case class ParSimpleAssignment(variable: String, value: Value) extends ParSimpleExp {
  override def equals(that: Any) = that.isInstanceOf[ParSimpleAssignment] && pos == that.asInstanceOf[ParSimpleAssignment].pos && super.equals(that)
  override def toString = s"(= $variable $value)"
}

case class ParSimpleVariable(variable: String) extends ParSimpleExp {
  override def equals(that: Any) = that.isInstanceOf[ParSimpleVariable] && pos == that.asInstanceOf[ParSimpleVariable].pos && super.equals(that)
  override def toString = variable
}

object ParSimpleCompiler {
  def compile(exp: SExp): ParSimpleProgram = {
    val exp2 = exp match {
      case SExpPair(SExpIdentifier("threads"), SExpPair(vars, SExpPair(threads, SExpPair(SExpIdentifier(variable), SExpValue(ValueNil))))) =>
        ParSimpleProgram(compileVars(vars), compileThreads(threads), ParSimpleVariable(variable))
      case _ => throw new Exception(s"Invalid ParSimple program: $exp (${exp.pos})")
    }
    exp2.setPos(exp.pos)
  }
  def compileVars(exp: SExp): List[(String, Value)] = exp match {
    case SExpPair(variable, rest) => compileVar(variable) :: compileVars(rest)
    case SExpValue(ValueNil) => List()
    case _ => throw new Exception(s"Invalid ParSimple bindings: $exp (${exp.pos})")
  }
  def compileVar(exp: SExp): (String, Value) = exp match {
    case SExpPair(SExpIdentifier(variable), SExpPair(SExpValue(value), SExpValue(ValueNil))) => (variable, value)
    case _ => throw new Exception(s"Invalid ParSimple variable binding: $exp (${exp.pos})")
  }
  def compileThreads(exp: SExp): List[(String, ParSimpleThreadCode)] = exp match {
    case SExpPair(thread, rest) => compileThread(thread) :: compileThreads(rest)
    case SExpValue(ValueNil) => List()
    case _ => throw new Exception(s"Invalid ParSimple threads: $exp (${exp.pos})")
  }
  def compileThread(exp: SExp): (String, ParSimpleThreadCode) = {
    val (name, exp2) = exp match {
      case SExpPair(SExpIdentifier(name), SExpPair(code, SExpValue(ValueNil))) => (name, ParSimpleThreadCode(compileThreadCode(code)))
      case _ => throw new Exception(s"Invalid ParSimple thread binding: $exp (${exp.pos})")
    }
    (name, exp2.setPos(exp.pos))
  }
  def compileThreadCode(exp: SExp): List[ParSimpleAssignment] = exp match {
    case SExpPair(SExpPair(SExpIdentifier("="), SExpPair(SExpIdentifier(variable), SExpPair(SExpValue(value), SExpValue(ValueNil)))), rest) => {
      val assignment = ParSimpleAssignment(variable, value)
      assignment.setPos(exp.pos) :: compileThreadCode(rest)
    }
    case SExpValue(ValueNil) => List()
    case _ => throw new Exception(s"Invalid ParSimple assignment: $exp (${exp.pos})")
  }
}

object ParSimple {
  def compile(exp: SExp): ParSimpleExp = ParSimpleCompiler.compile(exp)
  def parse(s: String): ParSimpleExp = SExpParser.parse(s) match {
    case e :: Nil => compile(e)
    case p => throw new Exception(s"Invalid ParSimple program: it should contain a single s-expression (but it contains ${p.size})")
  }
}

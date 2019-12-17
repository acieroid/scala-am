package scalaam.modular.scheme

import scalaam.core._
import scalaam.language.scheme._
import scalaam.util.MonoidImplicits._
import scalaam.modular.ModAnalysis._

trait SmallStepSchemeModFSemantics extends SchemeModFSemanticBase {
  // defining the intra-analysis
  override def intraAnalysis(ptr: ComponentPointer) = new IntraAnalysis(ptr)
  class IntraAnalysis(ptr: ComponentPointer) extends super.IntraAnalysis(ptr) with SchemeModFSemanticsIntra {
    // the intermediate states in the intra-analysis
    sealed trait State
    case class EvalState(exp: SchemeExp,
                         cnt: Kont) extends State
    case class KontState(vlu: Value,
                         cnt: Kont) extends State
    case class CallState(fexp: SchemeExp,
                         fval: Value,
                         args: List[(SchemeExp,Value)],
                         cnt: Kont) extends State
    // the frames used to build the continuation
    type Kont = List[Frame]
    sealed trait Frame
    case class SeqFrame(exps: List[SchemeExp])            extends Frame
    case class DefVarFrame(id: Identifier)                extends Frame
    case class SetFrame(lex: LexicalRef)                  extends Frame
    case class IfFrame(csq: SchemeExp,
                       alt: SchemeExp)                    extends Frame
    case class LetFrame(id: Identifier,
                        bds: List[(Identifier, SchemeExp)],
                        bdy: List[SchemeExp])             extends Frame
    case class ArgsFrame(fexp: SchemeExp,
                         fval: Value,
                         curExp: SchemeExp,
                         toEval: List[SchemeExp],
                         args: List[(SchemeExp,Value)])   extends Frame
    case class FunFrame(fexp: SchemeExp,
                        args: List[SchemeExp])            extends Frame
    case class AndFrame(exps: List[SchemeExp])            extends Frame
    case class OrFrame(exps: List[SchemeExp])             extends Frame
    case class PairCarFrm(pairExp: SchemePair)            extends Frame
    case class PairCdrFrm(carValue: Value,
                          pairExp: SchemePair)            extends Frame
    case class SplicedCarFrm(pairExp: SchemeSplicedPair)  extends Frame
    case class SplicedCdrFrm(carValue: Value,
                             pairExp: SchemeSplicedPair)  extends Frame

    // the main analyze method
    def analyze(): Unit = {
      // determine the initial state
      val initialExp = component match {
        case MainComponent        => program
        case call: CallComponent  => SchemeBody(getLambda(call).body)
      }
      val initialState = EvalState(initialExp,Nil)
      // standard worklist algorithm
      var work    = Set[State](initialState)
      var visited = Set[State]()
      var result  = lattice.bottom
      while(work.nonEmpty) {
        val state = work.head
        work = work.tail
        state match {
          case KontState(vlu,Nil) =>
            result = lattice.join(result,vlu)
          case _ if !visited.contains(state) =>
            val successors = step(state)
            work ++= successors
            visited += state
          case _ => ()
        }
      }
      writeResult(result)
    }
    // stepping a state
    private def step(state: State): Set[State] = state match {
      case EvalState(exp, cnt) =>
        eval(exp, cnt)
      case KontState(vlu, cnt) =>
        val frm = cnt.head
        continue(frm, vlu, cnt.tail)
      case CallState(fexp, fval, args, cnt) =>
        val result = applyFun(fexp, fval, args)
        Set(KontState(result, cnt))
    }
    // eval
    private def eval(exp: SchemeExp, cnt: Kont): Set[State] = exp match {
      case SchemeValue(value, _) =>
        val result = evalLiteralValue(value)
        Set(KontState(result, cnt))
      case lambda: SchemeLambdaExp =>
        val result = makeClosure(lambda, None)
        Set(KontState(result, cnt))
      case SchemeVarLex(_, lex) =>
        val result = lookupVariable(lex)
        Set(KontState(result, cnt))
      case SchemeBegin(exps, _) =>
        evalSequence(exps, cnt)
      case SchemeDefineVariable(id, vexp, _) =>
        val frm = DefVarFrame(id)
        Set(EvalState(vexp, frm :: cnt))
      case SchemeDefineFunction(id, prs, bdy, pos) =>
        val lambda = SchemeLambda(prs, bdy, pos)
        val result = makeClosure(lambda,Some(id.name))
        defineVariable(id, result)
        Set(KontState(result, cnt))
      case SchemeDefineVarArgFunction(id, prs, vararg, bdy, pos) =>
        val lambda = SchemeVarArgLambda(prs, vararg, bdy, pos)
        val result = makeClosure(lambda,Some(id.name))
        defineVariable(id, result)
        Set(KontState(result, cnt))
      case SchemeSetLex(_, lex, vexp, _) =>
        val frm = SetFrame(lex)
        Set(EvalState(vexp, frm :: cnt))
      case SchemeIf(prd, csq, alt, _) =>
        val frm = IfFrame(csq, alt)
        Set(EvalState(prd, frm :: cnt))
      case SchemeLet(bindings, body, _) =>
        evalLet(bindings,body,cnt)
      case SchemeLetStar(bindings, body, _) =>
        evalLet(bindings,body,cnt)
      case SchemeLetrec(bindings, body, _) =>
        evalLet(bindings,body,cnt)
      case SchemeNamedLet(id,bindings,body,pos) =>
        val (prs,ags) = bindings.unzip
        val lambda = SchemeLambda(prs,body,pos)
        val closure = makeClosure(lambda,Some(id.name))
        defineVariable(id, closure)
        evalArgs(lambda,closure,ags,Nil,cnt)
      case SchemeFuncall(fexp,args,_) =>
        val frm = FunFrame(fexp,args)
        Set(EvalState(fexp, frm :: cnt))
      case SchemeAnd(Nil,_) =>
        Set(KontState(lattice.bool(true), cnt))
      case SchemeAnd(first :: rest, _) =>
        evalAnd(first,rest,cnt)
      case SchemeOr(exps,_) =>
        evalOr(exps,cnt)
      case pair: SchemePair =>
        val frm = PairCarFrm(pair)
        Set(EvalState(pair.car, frm :: cnt))
      case spliced: SchemeSplicedPair =>
        val frm = SplicedCarFrm(spliced)
        Set(EvalState(spliced.splice, frm :: cnt))
      case _ =>
        throw new Exception(s"Unsupported Scheme expression: $exp")
    }
    private def evalSequence(exps: List[SchemeExp], cnt: Kont): Set[State] =
      if (exps.tail.isEmpty) {
        Set(EvalState(exps.head, cnt))
      } else {
        val frm = SeqFrame(exps.tail)
        Set(EvalState(exps.head, frm :: cnt))
      }
    private def evalLet(bindings: List[(Identifier,SchemeExp)], body: List[SchemeExp], cnt: Kont): Set[State] =
      if (bindings.isEmpty) {
        evalSequence(body,cnt)
      } else {
        val (id,vexp) = bindings.head
        val frm = LetFrame(id,bindings.tail,body)
        Set(EvalState(vexp, frm :: cnt))
      }
    private def evalArgs(fexp: SchemeExp, fval: Value, toEval: List[SchemeExp], ags: List[(SchemeExp,Value)], cnt: Kont): Set[State] =
      if (toEval.isEmpty) {
        Set(CallState(fexp,fval,ags.reverse,cnt))
      } else {
        val curExp = toEval.head
        val frm = ArgsFrame(fexp, fval, curExp, toEval.tail, ags)
        Set(EvalState(curExp, frm :: cnt))
      }
    private def evalAnd(first: SchemeExp, rest: List[SchemeExp], cnt: Kont): Set[State] =
      if (rest.isEmpty) {
        Set(EvalState(first,cnt))
      } else {
        val frm = AndFrame(rest)
        Set(EvalState(first, frm :: cnt))
      }
    private def evalOr(exps: List[SchemeExp], cnt: Kont): Set[State] = exps match {
      case Nil =>
        Set(KontState(lattice.bool(false),cnt))
      case nxt :: rst =>
        val frm = OrFrame(rst)
        Set(EvalState(nxt, frm :: cnt))
      }
    // continue
    private def continue(frm: Frame, vlu: Value, cnt: Kont): Set[State] = frm match {
      case SeqFrame(exps) =>
        evalSequence(exps, cnt)
      case DefVarFrame(id) =>
        defineVariable(id,vlu)
        Set(KontState(vlu,cnt))
      case SetFrame(lex) =>
        setVariable(lex,vlu)
        Set(KontState(vlu,cnt))
      case IfFrame(csq, alt) =>
        conditional(vlu,
                    Set(EvalState(csq,cnt)),
                    Set(EvalState(alt,cnt)))
      case LetFrame(id,bindings,body) =>
        defineVariable(id, vlu)
        evalLet(bindings, body, cnt)
      case FunFrame(fexp, args) =>
        evalArgs(fexp,vlu,args,Nil,cnt)
      case ArgsFrame(fexp,fval,curExp,toEval,args) =>
        val newArgs = (curExp, vlu) :: args
        evalArgs(fexp,fval,toEval,newArgs,cnt)
      case AndFrame(exps) =>
        conditional(vlu,
                    evalAnd(exps.head,exps.tail,cnt),
                    Set(KontState(lattice.bool(false),cnt)))
      case OrFrame(exps) =>
        conditional(vlu,
                    Set(KontState(vlu,cnt)),
                    evalOr(exps,cnt))
      case PairCarFrm(pair) =>
        val frm = PairCdrFrm(vlu,pair)
        Set(EvalState(pair.cdr, frm :: cnt))
      case PairCdrFrm(carVlu,pairExp) =>
        val result = allocateCons(pairExp)(carVlu,vlu)
        Set(KontState(result,cnt))
      case SplicedCarFrm(spliced) =>
        val frm = SplicedCdrFrm(vlu,spliced)
        Set(EvalState(spliced.cdr, frm :: cnt))
      case SplicedCdrFrm(spliceValue,pairExp) =>
        val result = append(pairExp)((pairExp.splice, spliceValue), (pairExp.cdr,vlu))
        Set(KontState(result,cnt))
    }
  }
}

trait BaseSmallStepSchemeModFSemantics extends SmallStepSchemeModFSemantics with SchemeModFSemantics


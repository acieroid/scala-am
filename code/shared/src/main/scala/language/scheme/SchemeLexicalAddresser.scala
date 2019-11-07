package scalaam.language.scheme

import scalaam.core.Identifier

// lexical addresses:
// - local vars are variables in the scope of the current function
// - non-local vars need to be looked up in the lexical scope
// - global vars are in the global scope (= at the top of every lexical scope)
trait LexicalAddr
case class LocalVar(offset: Int)                 extends LexicalAddr
case class NonLocalVar(scope: Int, offset: Int)  extends LexicalAddr
case class GlobalVar(offset: Int)                extends LexicalAddr

object SchemeLexicalAddresser {

  type Defs = List[String]

  /** Extract the identifier names of all definitions in a given body
    * Follows R5RS specification on where such definitions can appear */
  def defs(exp: SchemeExp): Defs = defs(List(exp))
  def defs(bdy: List[SchemeExp]): Defs = defs(bdy, Nil)
  def defs(bdy: List[SchemeExp], acc: Defs): Defs = bdy match {
    case SchemeDefineVariable(id,_,_) :: rest =>
      defs(rest, id.name :: acc)
    case SchemeDefineFunction(id,_,_,_) :: rest =>
      defs(rest, id.name :: acc)
    case SchemeBegin(exps,_) :: rest =>
      defs(rest, defs(exps, acc))
    case _ :: rest =>
      defs(rest, acc)
    case _ => acc
  }

  // a scope is a chain of binding environments (always ending with the global binding environment)
  // a binding environment is a map from variables to their offset in that binding environment
  type Scope = List[Map[String,Int]]

  def resolve(name: String, scope: Scope): LexicalAddr =
    resolveAddr(name, scope, 1) match {
      case (1, ofs)                           => LocalVar(ofs)
      case (scp, ofs) if scp == scope.length  => GlobalVar(ofs)
      case (scp, ofs)                         => NonLocalVar(scp,ofs)
    }

  def resolveAddr(name: String, scope: Scope, scp: Int): (Int,Int) =
    if (scope.isEmpty) {
      throw new Exception(s"Undefined variable reference: $name")
    } else scope.head.get(name) match {
      case None       => resolveAddr(name, scope.tail, scp + 1)
      case Some(ofs)  => (scp, ofs)
    }

  def localOffset(name: String, scope: Scope): Int = scope.head(name)
  def localOffset(id: Identifier, scope: Scope): Int = localOffset(id.name, scope)

  def translateProgram(prg: SchemeExp, initialBindings: List[String]): SchemeExp =
    newFrame(Nil) { freshScp =>
      val definitions = defs(prg)
      val allBindings = initialBindings ++ definitions
      val initialScp = extendScope(freshScp, allBindings)
      val defOffsets = definitions.map(d => localOffset(d,initialScp))
      val translated = translate(prg, initialScp)
      translated
    }

  // TODO: use a state monad or something else that is less ugly ...
  // counts the total number of bindings ever allocated in the current frame
  var count = 0
  def translate(exp: SchemeExp, scope: Scope): SchemeExp = exp match {
    case vexp: SchemeValue => vexp
    case quoted: SchemeQuoted => quoted
    case SchemeLambda(prs,bdy,pos) =>
      val (bdyLex,lamSiz,lamOfs) = translateLambda(prs, bdy, scope)
      SchemeLambdaLex(prs, bdyLex, lamSiz, lamOfs, pos)
    case SchemeVar(id) =>
      SchemeVarLex(id, resolve(id.name, scope))
    case SchemeBegin(eps,pos) =>
      SchemeBegin(translate(eps, scope), pos)
    case SchemeDefineVariable(id,vexp,pos) =>
      val vexpLex = translate(vexp, scope)
      val varOffset = localOffset(id, scope)
      SchemeDefineVariableLex(id,varOffset,vexpLex,pos)
    case SchemeDefineFunction(id,prs,bdy,pos) =>
      val (bdyLex, lamSiz, lamOfs) = translateLambda(prs, bdy, scope)
      val funOffset = localOffset(id, scope)
      SchemeDefineFunctionLex(id,funOffset,prs,bdyLex,lamSiz,lamOfs,pos)
    case SchemeSet(id, vexp, pos) =>
      val vexpLex = translate(vexp, scope)
      val varAddr = resolve(id.name, scope)
      SchemeSetLex(id, varAddr, vexpLex, pos)
    case SchemeIf(prd,csq,alt,pos) =>
      SchemeIf(translate(prd,scope),translate(csq,scope),translate(alt,scope),pos)
    case SchemeFuncall(fun,args,pos) =>
      SchemeFuncall(translate(fun,scope),translate(args,scope),pos)
    case SchemeAnd(exps,pos) =>
      SchemeAnd(translate(exps,scope),pos)
    case SchemeOr(exps,pos) =>
      SchemeOr(translate(exps,scope),pos)
    case SchemeLet(bindings,body,pos) =>
      val (vrs,eps) = bindings.unzip
      val epsLex = eps.map { translate(_,scope) }
      val extScp = extendScope(scope, vrs.map(_.name))
      val bdsLex = vrs.map(localOffset(_,extScp)).zip(epsLex)
      val bdyDfs = defs(body)
      val bdyScp = extendScope(extScp, bdyDfs)
      val bdyOfs = bdyDfs.map(localOffset(_,bdyScp))
      val bdyLex = translate(body,bdyScp)
      // TODO: bindings still contains untranslated expressions!
      SchemeLetLex(bindings,bdsLex,bdyLex,bdyOfs,pos)
    case SchemeLetStar(bindings,body,pos) =>
      val (bdsLex, extScp) = bindings.foldLeft((List[(Int,SchemeExp)](),scope)) {
        case ((curBds,curScp),(nxtVar,nxtExp)) =>
          val expLex = translate(nxtExp,curScp)
          val nxtScp = extendScope(curScp,nxtVar.name)
          val vrbOfs = localOffset(nxtVar,nxtScp)
          ((vrbOfs,expLex) :: curBds, nxtScp)
      }
      val bdyDfs = defs(body)
      val bdyScp = extendScope(extScp, defs(body))
      val bdyOfs = bdyDfs.map(localOffset(_,bdyScp))
      val bdyLex = translate(body, bdyScp)
      // TODO: bindings still contains untranslated expressions!
      SchemeLetStarLex(bindings,bdsLex.reverse,bdyLex,bdyOfs,pos)
    case SchemeLetrec(bindings,body,pos) =>
      val (vrs,eps) = bindings.unzip
      val extScp = extendScope(scope, vrs.map(_.name))
      val epsLex = eps.map { translate(_,extScp) }
      val bdsLex = vrs.map(localOffset(_,extScp)).zip(epsLex)
      val bdyDfs = defs(body)
      val bdyScp = extendScope(extScp, bdyDfs)
      val bdyOfs = bdyDfs.map(localOffset(_,bdyScp))
      val bdyLex = translate(body,bdyScp)
      // TODO: bindings still contains untranslated expressions!
      SchemeLetrecLex(bindings,bdsLex,bdyLex,bdyOfs,pos)
    case SchemeNamedLet(name,bindings,body,pos) =>
      val (prs,eps) = bindings.unzip
      val epsLex = eps.map { translate(_,scope) }
      val extScp = extendScope(scope, name.name)
      val offset = localOffset(name, extScp)
      val (bdyLex,lamSiz,lamOfs) = translateLambda(prs,body,scope)
      // TODO: bindings still contains untranslated expressions!
      SchemeNamedLetLex(name,offset,bindings,epsLex,bdyLex,lamSiz,lamOfs,pos)
    case _ => throw new Exception(s"Unsupported Scheme expression: $exp")
  }

  def translate(bdy: List[SchemeExp], scope: Scope): List[SchemeExp] =
    bdy.map { exp => translate(exp,scope) }

  def translateLambda(prs: List[Identifier], bdy: List[SchemeExp], scope: Scope): (List[SchemeExp], Int, List[Int]) =
    newFrame(scope) { freshScp =>
      val bdyDfs = defs(bdy)
      val allDfs = prs.map(_.name) ++ bdyDfs
      val extScp = extendScope(freshScp,allDfs)
      val bdyLex = translate(bdy,extScp)
      val bdyOfs = bdyDfs.map(d => localOffset(d,extScp))
      (bdyLex, count, bdyOfs)
    }

  def extendScope(scope: Scope, vrb: String): Scope = {
    val localFrame :: restScope = scope
    val updatedLocalFrame = localFrame + (vrb -> count)
    count += 1
    updatedLocalFrame :: restScope
  }

  def extendScope(scope: Scope, vrs: List[String]): Scope =
    vrs.foldLeft(scope)(extendScope)

  /** When a new frame is added to the scope, it is initially empty *
    * Therefore, we need to reset the 'next offset'/'count' to zero */
  def newFrame[A](scope: Scope)(thunk: Scope => A): A = {
    val keep = count
    count = 0
    val result = thunk(Map[String,Int]() :: scope)
    count = keep
    result
  }

}

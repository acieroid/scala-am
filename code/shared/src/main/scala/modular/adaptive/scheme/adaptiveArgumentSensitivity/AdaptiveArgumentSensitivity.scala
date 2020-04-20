package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.core._
import scalaam.core.Position._
import scalaam.util._
import scalaam.modular.adaptive.scheme._

trait AdaptiveArgumentSensitivity extends AdaptiveSchemeModFSemantics {
  case class ComponentContext(args: List[Value]) {
    override def toString = args.mkString(",")
  }
  def updateCtx(update: Component => Component)(ctx: ComponentContext) =
    ComponentContext(ctx.args.map(updateValue(update)))
  // to determine how arguments need to be adapted
  private var adaptedArgs = Map[(lattice.Closure,Identifier),Set[Value]]()
  protected def widenArg(clo: lattice.Closure, par: Identifier, arg: Value) = {
    val currentAbs = adaptedArgs.getOrElse((clo,par),Set.empty)
    val updatedAbs = currentAbs.filterNot(lattice.subsumes(arg,_)) + arg
    this.adaptedArgs += ((clo,par) -> updatedAbs)
  }
  private def adaptArg(clo: lattice.Closure, par: Identifier, arg: Value) =
    adaptedArgs.getOrElse((clo,par),Set.empty)
               .find(lattice.subsumes(_, arg))
               .getOrElse(arg)
  private def adaptArgs(clo: lattice.Closure, args: List[Value]) =
    clo._1.args.zip(args).map { case (par,arg) => adaptArg(clo,par,arg) }
  // The context for a given closure only consists of argument values for non-excluded parameters for that closure
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component) = ComponentContext(adaptArgs(clo,args))
  // To adapt an existing component, we drop the argument values for parameters that have to be excluded
  def adaptComponent(cmp: ComponentData): ComponentData = cmp match {
    case Main               => Main
    case Call(clo,nam,ctx)  => Call(clo, nam, ComponentContext(adaptArgs(clo,ctx.args)))
  }
  // HELPER FUNCTIONS FOR `joinComponents`
  // TODO: Extract and put this somewhere else
  private def extractComponentRefs(value: Value): Set[Component] = value match {
    case valueLattice.Elements(vs)  => vs.flatMap(extractComponentRefs)
    case valueLattice.Element(v)    => extractComponentRefs(v)
  }
  private def extractComponentRefs(v: valueLattice.Value): Set[Component] = v match {
    case valueLattice.Pointer(addr)     => Set(extractComponentRefs(addr))
    case valueLattice.Clo(_,cmp,_)      => Set(cmp)
    case valueLattice.Cons(car,cdr)     => Set(extractComponentRefs(car),extractComponentRefs(cdr))
    case valueLattice.Vec(_,els,ini)    => extractComponentRefs(ini) ++ els.flatMap(p => extractComponentRefs(p._2))
    case _                              => Set.empty
  }
  private def extractComponentRefs(addr: Addr): Component = addr match {
    case ComponentAddr(cmp, _)  => cmp
    case ReturnAddr(cmp)        => cmp
  }
  private def getClosure(cmp: Component): Option[lattice.Closure] = view(cmp) match {
    case Main       => None
    case call: Call => Some(call.clo)
  }
  def similarV(v1: valueLattice.Value, v2: valueLattice.Value): Boolean = (v1,v2) match {
    case (valueLattice.Nil, valueLattice.Nil) => true
    case (valueLattice.Int(_), valueLattice.Int(_)) => true
    case (valueLattice.Real(_), valueLattice.Real(_)) => true
    case (valueLattice.Bool(_), valueLattice.Bool(_)) => true
    case (valueLattice.Char(_), valueLattice.Char(_)) => true
    case (valueLattice.Str(_), valueLattice.Str(_)) => true
    case (valueLattice.Symbol(_), valueLattice.Symbol(_)) => true
    case (valueLattice.Vec(_,_,_), valueLattice.Vec(_,_,_)) => true
    case (valueLattice.Clo(_,p1,_),valueLattice.Clo(_,p2,_)) => 
      getClosure(p1) == getClosure(p2)
    case (valueLattice.Cons(a1,d1),valueLattice.Cons(a2,d2)) =>
      getClosure(extractComponentRefs(a1)) == getClosure(extractComponentRefs(a2)) &&
      getClosure(extractComponentRefs(d1)) == getClosure(extractComponentRefs(d2))
    case (valueLattice.Pointer(a1),valueLattice.Pointer(a2)) =>
      getClosure(extractComponentRefs(a1)) == getClosure(extractComponentRefs(a2))
    case (_,_) => false
  }
  def similarValue(value1: Value, value2: Value): Boolean = (value1,value2) match {
    case (valueLattice.Element(v1), valueLattice.Element(v2)) => similarV(v1,v2)
    case (_,_) => false
  }
  def insertValue(values: List[Set[Value]], value: Value): List[Set[Value]] = values match {
    case Nil => List(Set(value))
    case vs :: rest if similarValue(vs.head,value) => (vs + value) :: rest 
    case vs :: rest => vs :: insertValue(rest, value)
  }
  def partitionValues(vs: Set[Value]): List[Set[Value]] = 
    vs.foldLeft(List.empty[Set[Value]])(insertValue)

  var toJoin = Set[Set[Component]]()
  var joinedArgs = List[Value]()
  def joinComponents(cmps: Set[Component]) = {
    this.toJoin = Set(cmps)
    while (toJoin.nonEmpty) {
      val next = this.toJoin.head
      this.toJoin = this.toJoin.tail
      // look at the next closure + contexts
      val calls = next.map(view(_).asInstanceOf[Call])
      val (clo, args) = (calls.head.clo, calls.map(_.ctx.args))
      //println("Joining the following components:")
      //calls.foreach(call => println(s"- $call"))
      // join all the argument values per parameter
      val valuesPerPos = args.toList.transpose.map(_.toSet)
      this.joinedArgs = clo._1.args.zip(valuesPerPos).map { case (par,args) =>
        val joinedArg = lattice.join(args)
        widenArg(clo, par, joinedArg)
        joinedArg
      }
      //println("into:")
      //println(s"=> ${calls.head.nam.getOrElse("anonymous")}(${joinedArgs.mkString(",")})")
      updateAnalysis()
      // from the joined args, determine the next components that need to be joined
      val componentRefs = this.joinedArgs.flatMap(extractComponentRefs)
      this.toJoin ++= componentRefs.groupBy(getClosure(_)).values.map(_.toSet)
      this.toJoin = this.toJoin.filterNot(_.size == 1).toSet   
    }
    //println()
  }

  def joinArg(clo: lattice.Closure, args: Set[List[Value]]) = {
    val valuesPerPos = args.toList.transpose.map(_.toSet)
    val valuesPerArg = clo._1.args.zip(valuesPerPos).toMap
    val (topArgument, argVals) = valuesPerArg.maxBy(_._2.size)
    val joinedArgVal = lattice.join(argVals)
    widenArg(clo, topArgument, joinedArgVal)
  }
  // we need to update the adaptedArgs mapping when the analysis is adapted
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    this.adaptedArgs = updateMap(updateClosurePar(update),updateSet(updateValue(update)))(adaptedArgs)(valueSetMonoid)
    this.toJoin = updateSet(updateSet(update))(toJoin)
    this.joinedArgs = joinedArgs.map(updateValue(update))
  }
  private def updateClosurePar(update: Component => Component)(key: (lattice.Closure, Identifier)) =
    (updateClosure(update)(key._1), key._2)
  private val valueSetMonoid = new Monoid[Set[Value]] {
    def zero = Set.empty
    def append(s1: Set[Value], s2: => Set[Value]) =
      if(s1.isEmpty) {
        s2
      } else if (s2.isEmpty) {
        s1
      } else {
        val union = s1 ++ s2
        union.filter(abs => !union.exists(other => other != abs && lattice.subsumes(other,abs)))
      }
  }
}

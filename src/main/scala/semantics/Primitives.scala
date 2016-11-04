/**
 * Each primitive has to implement this trait.
 */
trait Primitive[Addr, Abs] {
  /** The name of the primitive */
  val name: String
  /** Calls the primitive.
   * @param fexp: the expression with which the primitive has been called
   * @param args: the arguments with which the primitive has been called, both their expression and their value
   * @param store: the store
   * @return either an error, or the value returned by the primitive along with the updated store
   */
  def call[Exp : Expression, Time : Timestamp](fexp : Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time): MayFail[(Abs, Store[Addr, Abs], Set[Effect[Addr]])]
}

abstract class Primitives[Addr : Address, Abs : JoinLattice] {
  def all: List[Primitive[Addr, Abs]]
  def toVal(prim: Primitive[Addr, Abs]): Abs

  /** Modify a primitive to trace it: output will be printed when the primitive is
    * called. This is for debugging purposes. */
  def traced(prim: Primitive[Addr, Abs]): Primitive[Addr, Abs] = new Primitive[Addr, Abs] {
    val name = prim.name
    def call[Exp : Expression, Time : Timestamp](fexp: Exp, args: List[(Exp, Abs)], store: Store[Addr, Abs], t: Time) = {
      val argsstr = args.map({ case (_, v) => v.toString }).mkString(" ")
      val res = prim.call(fexp, args, store, t)
      print("($name $argsstr) -> ")
      res match {
        case MayFailError(errs) => "ERROR: " + errs.mkString(" | ERROR: ")
        case MayFailSuccess((v, store, effs)) => v.toString
        case MayFailBoth((v, store, effs), errs) => v.toString + " | ERROR: " + errs.mkString(" | ERROR: ")
      }
      res
    }
  }

  lazy val bindings = ("bottom", Address[Addr].botAddress, JoinLattice[Abs].bottom) :: all.map({ prim => (prim.name, Address[Addr].primitive(prim.name), toVal(prim)) })
}

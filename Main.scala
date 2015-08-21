import AbstractValue._

object Config {
  object Machine extends Enumeration {
    type Machine = Value
    val AAC, AAM, Free = Value
  }
  implicit val machineRead: scopt.Read[Machine.Value] = scopt.Read.reads(Machine withName _)

  object Lattice extends Enumeration {
    type Lattice = Value
    val Concrete, Type, TypeSet = Value
  }
  implicit val latticeRead: scopt.Read[Lattice.Value] = scopt.Read.reads(Lattice withName _)

  case class Config(machine: Machine.Value = Machine.Free, lattice: Lattice.Value = Lattice.TypeSet, concrete: Boolean = false, file: String = "", dotfile: Option[String] = None, anf: Boolean = false, diff: Option[(Int, Int)] = None)

  val parser = new scopt.OptionParser[Config]("scala-am") {
    head("scala-ac", "0.0")
    opt[Machine.Value]('m', "machine") action { (x, c) => c.copy(machine = x) } text("Abstract machine to use (AAM, AAC, Free)")
    opt[Lattice.Value]('l', "lattice") action { (x, c) => c.copy(lattice = x) } text("Lattice to use (Concrete, Type, TypeSet)")
    opt[Unit]('c', "concrete") action { (_, c) => c.copy(concrete = true) } text("Run in concrete mode")
    opt[String]('d', "dotfile") action { (x, c) => c.copy(dotfile = Some(x)) } text("Dot file to output graph to")
    opt[Unit]("anf") action { (_, c) => c.copy(anf = true) } text("Desugar program into ANF")
    opt[(Int, Int)]("diff") action { (x, c) => c.copy(diff = Some(x)) } text("States to diff") /* TODO: take this into account */
    arg[String]("<file>") required() maxOccurs(1) action { (x, c) => c.copy(file = x) } text("File to read program from")
  }
}

object Main {

  def runAAM[Abs, Addr](exp: SchemeExp, output: Option[String])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    println(s"Running AAM with lattice ${absi.name} and address ${addri.name}")
    val machine = new AAM[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])
    val result = machine.eval(exp, output)
    println(s"${result.size} possible results: $result")
  }

  def runAAC[Abs, Addr](exp: SchemeExp, output: Option[String])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    println(s"Running AAC with lattice ${absi.name} and address ${addri.name}")
    val machine = new AAC[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])
    val result = machine.eval(exp, output)
    println(s"${result.size} possible results: $result")
  }

  def runFree[Abs, Addr](exp: SchemeExp, output: Option[String])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    println(s"Running Free with lattice ${absi.name} and address ${addri.name}")
    val machine = new Free[Abs, Addr, SchemeExp](new SchemeSemantics[Abs, Addr])
    val result = machine.eval(exp, output)
    println(s"${result.size} possible results: $result")
  }

  def runAAMANF[Abs, Addr](exp: ANFExp, output: Option[String])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    println(s"Running AAM (ANF) with lattice ${absi.name} and address ${addri.name}")
    val machine = new AAM[Abs, Addr, ANFExp](new ANFSemantics[Abs, Addr])
    val result = machine.eval(exp, output)
    println(s"${result.size} possible results: $result")
  }

  def runAACANF[Abs, Addr](exp: ANFExp, output: Option[String])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    println(s"Running AAC (ANF) with lattice ${absi.name} and address ${addri.name}")
    val machine = new AAC[Abs, Addr, ANFExp](new ANFSemantics[Abs, Addr])
    val result = machine.eval(exp, output)
    println(s"${result.size} possible results: $result")
  }

  def runFreeANF[Abs, Addr](exp: ANFExp, output: Option[String])(implicit abs: AbstractValue[Abs], absi: AbstractInjection[Abs], addr: Address[Addr], addri: AddressInjection[Addr]): Unit = {
    println(s"Running Free (ANF) with lattice ${absi.name} and address ${addri.name}")
    val machine = new Free[Abs, Addr, ANFExp](new ANFSemantics[Abs, Addr])
    val result = machine.eval(exp, output)
    println(s"${result.size} possible results: $result")
  }

  def main(args: Array[String]) {
    Config.parser.parse(args, Config.Config()) match {
      case Some(config) => {
        /* ugly as fuck, but I don't find a simpler way to pass type parameters that are computed at runtime */
        if (config.anf) {
          val f = (config.machine, config.lattice, config.concrete) match {
            case (Config.Machine.AAM, Config.Lattice.Concrete, true) => runAAMANF[AbstractConcrete, ConcreteAddress] _
            case (Config.Machine.AAM, Config.Lattice.Concrete, false) => runAAMANF[AbstractConcrete, ClassicalAddress] _
            case (Config.Machine.AAM, Config.Lattice.TypeSet, true) => runAAMANF[AbstractTypeSet, ConcreteAddress] _
            case (Config.Machine.AAM, Config.Lattice.TypeSet, false) => runAAMANF[AbstractTypeSet, ClassicalAddress] _
            case (Config.Machine.AAM, Config.Lattice.Type, true) => runAAMANF[AbstractType, ConcreteAddress] _
            case (Config.Machine.AAM, Config.Lattice.Type, false) => runAAMANF[AbstractType, ClassicalAddress] _
            case (Config.Machine.AAC, Config.Lattice.Concrete, true) => runAACANF[AbstractConcrete, ConcreteAddress] _
            case (Config.Machine.AAC, Config.Lattice.Concrete, false) => runAACANF[AbstractConcrete, ClassicalAddress] _
            case (Config.Machine.AAC, Config.Lattice.TypeSet, true) => runAACANF[AbstractTypeSet, ConcreteAddress] _
            case (Config.Machine.AAC, Config.Lattice.TypeSet, false) => runAACANF[AbstractTypeSet, ClassicalAddress] _
            case (Config.Machine.AAC, Config.Lattice.Type, true) => runAACANF[AbstractType, ConcreteAddress] _
            case (Config.Machine.AAC, Config.Lattice.Type, false) => runAACANF[AbstractType, ClassicalAddress] _
            case (Config.Machine.Free, Config.Lattice.Concrete, true) => runFreeANF[AbstractConcrete, ConcreteAddress] _
            case (Config.Machine.Free, Config.Lattice.Concrete, false) => runFreeANF[AbstractConcrete, ClassicalAddress] _
            case (Config.Machine.Free, Config.Lattice.TypeSet, true) => runFreeANF[AbstractTypeSet, ConcreteAddress] _
            case (Config.Machine.Free, Config.Lattice.TypeSet, false) => runFreeANF[AbstractTypeSet, ClassicalAddress] _
            case (Config.Machine.Free, Config.Lattice.Type, true) => runFreeANF[AbstractType, ConcreteAddress] _
            case (Config.Machine.Free, Config.Lattice.Type, false) => runFreeANF[AbstractType, ClassicalAddress] _
            case _ => throw new Exception(s"Impossible configuration: $config")
          }
          val program = ANF.parse(config.file)
          println(program)
          f(program, config.dotfile)
        } else {
          val f = (config.machine, config.lattice, config.concrete) match {
            case (Config.Machine.AAM, Config.Lattice.Concrete, true) => runAAM[AbstractConcrete, ConcreteAddress] _
            case (Config.Machine.AAM, Config.Lattice.Concrete, false) => runAAM[AbstractConcrete, ClassicalAddress] _
            case (Config.Machine.AAM, Config.Lattice.TypeSet, true) => runAAM[AbstractTypeSet, ConcreteAddress] _
            case (Config.Machine.AAM, Config.Lattice.TypeSet, false) => runAAM[AbstractTypeSet, ClassicalAddress] _
            case (Config.Machine.AAM, Config.Lattice.Type, true) => runAAM[AbstractType, ConcreteAddress] _
            case (Config.Machine.AAM, Config.Lattice.Type, false) => runAAM[AbstractType, ClassicalAddress] _
            case (Config.Machine.AAC, Config.Lattice.Concrete, true) => runAAC[AbstractConcrete, ConcreteAddress] _
            case (Config.Machine.AAC, Config.Lattice.Concrete, false) => runAAC[AbstractConcrete, ClassicalAddress] _
            case (Config.Machine.AAC, Config.Lattice.TypeSet, true) => runAAC[AbstractTypeSet, ConcreteAddress] _
            case (Config.Machine.AAC, Config.Lattice.TypeSet, false) => runAAC[AbstractTypeSet, ClassicalAddress] _
            case (Config.Machine.AAC, Config.Lattice.Type, true) => runAAC[AbstractType, ConcreteAddress] _
            case (Config.Machine.AAC, Config.Lattice.Type, false) => runAAC[AbstractType, ClassicalAddress] _
            case (Config.Machine.Free, Config.Lattice.Concrete, true) => runFree[AbstractConcrete, ConcreteAddress] _
            case (Config.Machine.Free, Config.Lattice.Concrete, false) => runFree[AbstractConcrete, ClassicalAddress] _
            case (Config.Machine.Free, Config.Lattice.TypeSet, true) => runFree[AbstractTypeSet, ConcreteAddress] _
            case (Config.Machine.Free, Config.Lattice.TypeSet, false) => runFree[AbstractTypeSet, ClassicalAddress] _
            case (Config.Machine.Free, Config.Lattice.Type, true) => runFree[AbstractType, ConcreteAddress] _
            case (Config.Machine.Free, Config.Lattice.Type, false) => runFree[AbstractType, ClassicalAddress] _
            case _ => throw new Exception(s"Impossible configuration: $config")
          }
          val program = Scheme.parse(config.file)
          println(program)
          f(program, config.dotfile)
        }
      }
      case None => ()
    }
  }
}

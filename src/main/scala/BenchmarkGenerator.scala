object BenchmarkGeneratorConfig {
  case class Configuration(name: String = "indexer", threads: Int = 2, output: Option[String] = None)
  val parser = new scopt.OptionParser[Configuration]("scala-am") {
    head("scala-am", "0.0")
    opt[Int]('t', "threads") action { (x, c) => c.copy(threads = x) } text("Number of threads (2 by default)")
    opt[String]('n', "name") action { (x, c) => c.copy(name = x) } text("Name of the benchmark (indexer by default)")
    opt[String]('f', "file") action { (x, c) => c.copy(output = Some(x)) } text("Output file (stdout by default)")
  }
}

object BenchmarkGenerator {
  trait Generator {
    def generate(threads: Int): String
  }
  abstract class SimpleGenerator extends Generator {
    val definitions: String
    val threadName: String
    def generate(threads: Int) = {
      definitions +
        (1 to threads).map(i => s"(t$i (spawn ($threadName $i)))").mkString("\n") + ")\n" +
        (1 to threads).map(i => s"(join t$i)").mkString("\n") + ")"
    }
  }
  object Indexer extends SimpleGenerator {
    val definitions = """(let* ((size 128)
       (max 4)
       (table (make-vector size 0))
       (thread (lambda (tid)
                 (letrec ((hash (lambda (w) (modulo (* w 7) size)))
                          (process (lambda (m)
                                     (if (< m max)
                                         (letrec ((w (+ (* 11 (+ m 1)) tid))
                                                  (update (lambda (h)
                                                            (if (cas-vector table h 0 w)
                                                                #t
                                                                (update (modulo (+ h 1) size))))))
                                           (update (hash w))
                                           (process (+ m 1)))
                                         #t))))
                   (process 0))))
"""
    val threadName = "thread"
  }
  object Fs extends SimpleGenerator {
    val definitions = """(let* ((numblocks 26)
       (numinode 32)
       (locki (vector (new-lock) (new-lock) (new-lock) (new-lock) (new-lock)
                      (new-lock) (new-lock) (new-lock) (new-lock) (new-lock)
                      (new-lock) (new-lock) (new-lock) (new-lock) (new-lock)
                      (new-lock) (new-lock) (new-lock) (new-lock) (new-lock)
                      (new-lock) (new-lock) (new-lock) (new-lock) (new-lock)
                      (new-lock) (new-lock) (new-lock) (new-lock) (new-lock)
                      (new-lock) (new-lock)))
       (inode (make-vector numinode 0))
       (lockb (vector (new-lock) (new-lock) (new-lock) (new-lock) (new-lock)
                      (new-lock) (new-lock) (new-lock) (new-lock) (new-lock)
                      (new-lock) (new-lock) (new-lock) (new-lock) (new-lock)
                      (new-lock) (new-lock) (new-lock) (new-lock) (new-lock)
                      (new-lock) (new-lock) (new-lock) (new-lock) (new-lock)
                      (new-lock)))
       (busy (make-vector numblocks #f))
       (thread (lambda (tid)
                 (letrec ((i (modulo tid numinode))
                          (process (lambda (b)
                                     (acquire (vector-ref lockb b))
                                     (if (not (vector-ref busy b))
                                         (begin
                                           (vector-set! busy b #t)
                                           (vector-set! inode i (+ b 1))
                                           (release (vector-ref lockb b)))
                                         (begin
                                           (release (vector-ref lockb b))
                                           (process (modulo (+ b 1) numblocks)))))))
                   (acquire (vector-ref locki i))
                   (if (= (vector-ref inode i) 0)
                       (process (modulo (* i 2) numblocks)))
                   (release (vector-ref locki i)))))
"""
    val threadName = "thread"
  }
  val benchmarks: Map[String, Generator] = Map("fs" -> Fs, "indexer" -> Indexer)

  def main(args: Array[String]) {
    BenchmarkGeneratorConfig.parser.parse(args, BenchmarkGeneratorConfig.Configuration()) match {
      case Some(config) =>
        benchmarks.get(config.name) match {
          case Some(gen) =>
            val out = gen.generate(config.threads)
            config.output match {
              case Some(name) => {
                val f = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(name)))
                f.write(out)
                f.close()
              }
              case None => println(out)
            }
          case None => println(s"Benchmark '${config.name}' doesn't exist")
        }
      case None => println("Invalid configuration")
    }
  }
}

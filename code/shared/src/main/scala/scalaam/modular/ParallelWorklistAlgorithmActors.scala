package scalaam.modular

import scalaam.core._
import akka.actor.typed.{Behavior => Behaviour, ActorRef, ActorSystem, DispatcherSelector}
import akka.actor.typed.scaladsl.{Behaviors => Behaviours, Routers, PoolRouter, AskPattern}
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scalaam.util.benchmarks.Timeout
import com.typesafe.config.ConfigFactory

object ActorConfig {
  val config = ConfigFactory.parseString("""
akka {
  actor {
      default-dispatcher {
          type = PinnedDispatcher
          executor = "thread-pool-executor"
      }
  }   
}
  """)
}

trait ParallelWorklistAlgorithmActors[Expr <: Expression] extends ModAnalysis[Expr] { inter =>
  
  def workers: Int

  // We use a Master-Worker pattern, where the Master coordinates the distribution of intra-analyses, and the management of the global analysis state
  object Master { 
    sealed trait MasterMessage
    case class Start(timeout: Timeout.T, respondTo: ActorRef[Boolean]) extends MasterMessage
    case class Result(intra: ParallelIntra) extends MasterMessage
    case class TimedOut(cmp: Component) extends MasterMessage
    def ready(workers: ActorRef[Worker.WorkerMessage], cmps: Set[Component]): Behaviour[MasterMessage] =
      Behaviours.receive((context,msg) => msg match {
        case Start(timeout, replyTo) =>
          for (cmp <- cmps) {
            workers ! Worker.DoWork(intraAnalysis(cmp), timeout, context.self)
          }
          running(workers, cmps, timeout, replyTo)
      })
    def running(workers: ActorRef[Worker.WorkerMessage], queued: Set[Component], timeout: Timeout.T, replyTo: ActorRef[Boolean]): Behaviour[MasterMessage] = 
      if (queued.isEmpty) {
        replyTo ! true
        Behaviours.stopped
      } else {
        Behaviours.receive((context, msg) => msg match {
          case Result(intra) => 
            intra.commit()
            // add items to the worklist
            var updatedQueued = queued
            toDistribute.foreach { cmp =>
              if(!updatedQueued(cmp)) {
                updatedQueued += cmp
                workers ! Worker.DoWork(intraAnalysis(cmp), timeout, context.self)
              }
            }
            toDistribute = Nil
            // check if the current component needs to be analyzed again
            if(intra.isDone) {
              running(workers, updatedQueued - intra.component, timeout, replyTo)
            } else {
              workers ! Worker.DoWork(intraAnalysis(intra.component), timeout, context.self)
              running(workers, updatedQueued, timeout, replyTo)
            }
          case TimedOut(cmp) =>
            pause(workers, queued - cmp, Set(cmp), replyTo)
        })
      }
    def pause(workers: ActorRef[Worker.WorkerMessage], waitingFor: Set[Component], cmps: Set[Component], replyTo: ActorRef[Boolean]): Behaviour[MasterMessage] = 
      if(waitingFor.isEmpty) {
        replyTo ! false
        ready(workers, cmps)
      } else {
        Behaviours.receiveMessage {
          case TimedOut(cmp) =>
            pause(workers, waitingFor - cmp, cmps + cmp, replyTo)
          case Result(intra) => 
            intra.commit()
            val updatedCmps = cmps ++ toDistribute
            val updatedWaitingFor = waitingFor - intra.component
            toDistribute = Nil
            if(intra.isDone) {
              pause(workers, updatedWaitingFor, updatedCmps, replyTo)
            } else {
              pause(workers, updatedWaitingFor, updatedCmps + intra.component, replyTo)
            }
        }
      }
    def apply(): Behaviour[MasterMessage] = Behaviours.setup { context =>
      val workerPool = Routers.pool(poolSize = inter.workers)(Worker())
                          .withRoundRobinRouting()
                          .withRouteeProps(routeeProps = DispatcherSelector.sameAsParent())
      val workers = context.spawn(workerPool, "worker-pool", DispatcherSelector.sameAsParent())
      ready(workers, Set(initialComponent))
    }
  }

  // We use a Master-Worker pattern, where Workers perform the intra-analyses
  object Worker {
    sealed trait WorkerMessage
    case class DoWork(intra: ParallelIntra, timeout: Timeout.T, master: ActorRef[Master.MasterMessage]) extends WorkerMessage
    def apply(): Behaviour[WorkerMessage] = Behaviours.setup { context =>
      Behaviours.receiveMessage {
        case DoWork(intra, timeout, master) =>
          intra.analyze(timeout)
          if(timeout.reached) {
            master ! Master.TimedOut(intra.component)
          } else {
            master ! Master.Result(intra)
          }
          Behaviours.same
      }
    }
  }

  lazy val actorSystem = ActorSystem(Master(), "analysis-actor-system", ActorConfig.config)

  private var done = false
  def finished(): Boolean = done

  // use this to temporarily hold items that need to be added to the worklist
  private var toDistribute: List[Component] = Nil
  @inline final def addToWorkList(cmp: Component): Unit = 
    toDistribute = cmp :: toDistribute

  import AskPattern._
  def analyze(timeout: Timeout.T): Unit = {
    val infDuration = scala.concurrent.duration.Duration(10, TimeUnit.DAYS) // TODO: Akka doesn't seem to support infinite timeouts?
    implicit val infTimeout = akka.util.Timeout(infDuration)
    implicit val scheduler = actorSystem
    val future = actorSystem.ask(replyTo => Master.Start(timeout, replyTo))
    if(Await.result(future, infDuration)) {
      done = true
      actorSystem.terminate()
      Await.result(actorSystem.whenTerminated, infDuration)
    }
  }

  def analyzeAndShutDown(timeout: Timeout.T): Unit =
    try {
      analyze(timeout) 
    } finally {
      actorSystem.terminate()
    }

  // keep track for every dependency of its "version number"
  var depVersion = Map[Dependency, Int]().withDefaultValue(0)

  def intraAnalysis(component: Component): ParallelIntra
  trait ParallelIntra extends IntraAnalysis { intra =>
    val depVersion = inter.depVersion
    override def commit(dep: Dependency): Boolean =
      if (super.commit(dep)) {
        inter.depVersion += dep -> (inter.depVersion(dep) + 1)
        true
      } else {
        false
      }
    def isDone: Boolean = R.forall(dep => inter.depVersion(dep) == intra.depVersion(dep))
  }
}

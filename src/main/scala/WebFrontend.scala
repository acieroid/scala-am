import akka.actor.ActorSystem

import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import java.nio.file.{Files, Paths}
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.Duration
import spray.json.DefaultJsonProtocol

case class RunRequest(concrete: Boolean, program: String)
case class Answer(graph: String, time: Double, timedOut: Boolean)

object WebFrontend extends DefaultJsonProtocol {
  implicit val system = ActorSystem()
  implicit val executor = system.dispatcher
  implicit val materializer = ActorMaterializer()

  implicit val runRequestFormat = jsonFormat2(RunRequest.apply)
  implicit val answerFormat = jsonFormat3(Answer.apply)
  val timeout: Option[Long] = Some(Duration(60, "seconds").toNanos)
  val c : ContentType = ContentType(MediaType.customWithFixedCharset("text", "html", HttpCharsets.`UTF-8`)) //MediaTypes.`text/html`)

  val routes = {
    pathSingleSlash {
      get { complete { HttpResponse(OK, entity = HttpEntity(c, Files.readAllBytes(Paths.get("static/index.html")))) } } ~
      (post & entity(as[RunRequest])) { req =>
        complete {
          if (req.concrete) {
            val machine = new AAM[SchemeExp, ScalaAM.concreteLattice.L, ClassicalAddress.A, ConcreteTimestamp.T]
            val res = ScalaAM.run[SchemeExp, ScalaAM.concreteLattice.L, ClassicalAddress.A, ConcreteTimestamp.T](machine,
              new SchemeSemantics[ScalaAM.concreteLattice.L, ClassicalAddress.A, ConcreteTimestamp.T](
                new SchemePrimitives[ClassicalAddress.A, ScalaAM.concreteLattice.L]))(
              req.program, true, timeout)
            val graph = res.asInstanceOf[machine.AAMOutput].graph match {
              case Some(g) => Util.withStringWriter { GraphJSONOutput.out(g, ()) }
              case None => ""
            }
            Answer(graph, res.time, res.timedOut)
          } else {
            val machine = new AAM[SchemeExp, ScalaAM.typeLattice.L, ClassicalAddress.A, ZeroCFA.T]
            val res = ScalaAM.run[SchemeExp, ScalaAM.typeLattice.L, ClassicalAddress.A, ZeroCFA.T](machine,
              new SchemeSemantics[ScalaAM.typeLattice.L, ClassicalAddress.A, ZeroCFA.T](
                new SchemePrimitives[ClassicalAddress.A, ScalaAM.typeLattice.L]))(
              req.program, true, timeout)
            val graph = res.asInstanceOf[machine.AAMOutput].graph match {
              case Some(g) => Util.withStringWriter { GraphJSONOutput.out(g, ()) }
              case None => ""
            }
            Answer(graph, res.time, res.timedOut)
          }
        }
      }
    }
  }
  def main(args: Array[String]): Unit = {
    import scala.concurrent.Await
    import scala.io.StdIn
    Http().bindAndHandle(routes, "0.0.0.0", 9000)
    StdIn.readLine("Hit ENTER to exit\n")
    Await.ready(system.terminate(), Duration.Inf)
  }
}

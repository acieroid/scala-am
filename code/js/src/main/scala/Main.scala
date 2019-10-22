package scalaam.web

import modular.scheme.SchemeSmallStepModFAnalysis
import scalaam.modular.scheme._
import scalaam.language.scheme._

// Scala.js-related imports
import scala.scalajs.js
import org.scalajs.dom
import dom.{html,document}

// Scala.js helpers

object FileInputElement {
  def apply(handler: String => Unit): html.Input = {
    val input = document.createElement("input").asInstanceOf[html.Input]
    input.setAttribute("type","file")
    input.addEventListener("change", (evtUpload: dom.Event) => {
      val file = input.files.item(0)
      val reader = new dom.FileReader()
      reader.onload = (evtLoad: dom.Event) => handler(reader.result.asInstanceOf[String])
      reader.readAsText(file)
    }, false)
    return input
  }
}

object Main {

  val input = FileInputElement(loadFile)

  def main(args: Array[String]): Unit = setupUI()

  def setupUI() = {
    val body = document.body
    body.appendChild(input)
  }

  def loadFile(text: String): Unit = {
    val program = SchemeParser.parse(text)
    val analysis = new SchemeSmallStepModFAnalysis(program) with FullArgumentSensitivity with ConstantPropagationDomain {
      // stub implementation to keep the compiler happy
      def alpha(cmp: IntraComponent): IntraComponent = cmp
    }
    val visualisation = new WebVisualisation(analysis)
    // parameters for the visualisation
    val body = document.body
    val width = js.Dynamic.global.document.documentElement.clientWidth.asInstanceOf[Int]
    val height = js.Dynamic.global.document.documentElement.clientHeight.asInstanceOf[Int]
    visualisation.init(body, width, height)
  }
}

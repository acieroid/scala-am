package scalaam.web


import scalaam.modular.adaptive._
import scalaam.modular.adaptive.scheme._
import scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity._
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
    val analysis = new AdaptiveModAnalysis(program) with AdaptiveSchemeModFSemantics
                                                    with AdaptiveArgumentSensitivityPolicy1
                                                    with ConstantPropagationDomain
                                                    with WebAdaptiveAnalysis[SchemeExp] {
      val limit = 2
      override def step() = {
        val component = work.head
        val name = deref(component)
        val prevResult = store.get(ReturnAddr(component)).getOrElse(lattice.bottom)
        super.step()
        val newResult = store.get(ReturnAddr(component)).getOrElse(lattice.bottom)
        println(s"$name => $newResult (previously: $prevResult)")
      }
    }
    val visualisation = new WebVisualisationAdaptive(analysis)
    // parameters for the visualisation
    val body = document.body
    val width = js.Dynamic.global.document.documentElement.clientWidth.asInstanceOf[Int]
    val height = js.Dynamic.global.document.documentElement.clientHeight.asInstanceOf[Int]
    visualisation.init(body, width, height)
  }
}

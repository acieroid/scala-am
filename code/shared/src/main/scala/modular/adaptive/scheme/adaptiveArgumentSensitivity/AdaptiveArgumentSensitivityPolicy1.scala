package scalaam.modular.adaptive.scheme.adaptiveArgumentSensitivity

import scalaam.language.scheme._

trait AdaptiveArgumentSensitivityPolicy1 extends AdaptiveArgumentSensitivity {
  // parameterized by a simple limit
  // every closure can only have at most "limit" components
  val limit: Int
  override def onNewComponent(cmp: Component, call: Call)= {
    super.onNewComponent(cmp, call)
    // if there are too many components => do something about it!
    val cmps = cmpsPerFn(call.body)
    if (limit < cmps.size) {
      joinComponents(cmps)
    }
  }
}
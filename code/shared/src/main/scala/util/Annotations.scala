package scalaam.util

object Annotations {
  class unsound(reason: String = "")      extends scala.annotation.StaticAnnotation
  class maybeUnsound(reason: String = "") extends unsound

  class toCheck(reason: String = "")      extends scala.annotation.StaticAnnotation

  class mutable                           extends scala.annotation.StaticAnnotation
}

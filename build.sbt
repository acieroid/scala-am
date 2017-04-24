scalaVersion := "2.12.2"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.11"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
libraryDependencies ++= {
  val akkaV       = (if (sys.props("java.specification.version") == "1.8") "2.5.0" else "2.3.16")
  val akkaHttpV   = "10.0.5"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-stream" % akkaV,
    "com.typesafe.akka" %% "akka-testkit" % akkaV,
    "com.typesafe.akka" %% "akka-http" % akkaHttpV,
    "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpV,
    "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpV)
}
libraryDependencies += "jline" % "jline" % "2.14.3"
libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.1"
libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.5.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5" % "test"

maxErrors := 5
mainClass in (Compile, run) := Some("Main")

addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")
scalacOptions += "-P:linter:disable:UnusedParameter"

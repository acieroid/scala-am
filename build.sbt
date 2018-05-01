name := "scalaam"
version := "0.1.2018"

scalaVersion := "2.12.5"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.1.0"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.22"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.0"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.11"
libraryDependencies += "jline" % "jline" % "2.14.6"
libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.3"
libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.5.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

maxErrors := 5
scalaSource in Compile := baseDirectory.value / "src" / "scalaam"


resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

// mainClass in (Compile, run) := Some("scalaam.Main")

addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")
scalacOptions += "-P:linter:disable:UnusedParameter"

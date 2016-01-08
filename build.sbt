scalaVersion := "2.11.7"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.3"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.14"

maxErrors := 5
mainClass in (Compile, run) := Some("Main")

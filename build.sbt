scalaVersion := "2.11.8"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.6"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.0" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.15"
libraryDependencies += "com.typesafe.akka" %% "akka-agent" % "2.3.15"
libraryDependencies += "jline" % "jline" % "2.14.2"

maxErrors := 5
mainClass in (Compile, run) := Some("Main")

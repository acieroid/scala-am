scalaVersion := "2.11.8"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.3"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test" // latest: 1.13.0, but scalatest 2.2.6 depends on scalacheck 1.12.X
libraryDependencies += "com.github.scopt" %% "scopt" % "3.4.0"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.15"
libraryDependencies += "com.typesafe.akka" %% "akka-agent" % "2.3.15"

maxErrors := 5
mainClass in (Compile, run) := Some("Main")

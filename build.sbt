scalaVersion := "2.11.8"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.7"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % (if (sys.props("java.specification.version") == "1.8") "2.4.12" else "2.3.15")
libraryDependencies += "jline" % "jline" % "2.14.2"
libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.0"
libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.5.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.1" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

maxErrors := 5
mainClass in (Compile, run) := Some("Main")

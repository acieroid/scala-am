scalaVersion := "2.11.6"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.3"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
maxErrors := 5

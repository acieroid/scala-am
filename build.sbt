name := "scalaam"
version := "0.1.2018"

scalaVersion := "2.12.6"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.22"

maxErrors := 5
scalaSource in Compile := baseDirectory.value / "src" / "scalaam"

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")


addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")
scalacOptions += "-P:linter:disable:UnusedParameter"

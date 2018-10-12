name := "scalaam"
version := "0.1.2018"

scalaVersion := "2.12.7"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"

maxErrors := 5
scalaSource in Compile := baseDirectory.value / "src" / "scalaam"

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")


addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")
scalacOptions += "-P:linter:disable:UnusedParameter"

name := "team-a"

version := "0.1"

scalaVersion := "3.0.0-M3"


libraryDependencies ++= Seq(
  ("org.scala-lang.modules" %% "scala-parser-combinators" %  "1.1.2").withDottyCompat(scalaVersion.value),
  ("org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5").withDottyCompat(scalaVersion.value)
)

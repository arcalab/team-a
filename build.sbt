name := "teamA"

version := "0.1"

scalaVersion := "3.0.0-M1"


libraryDependencies ++= Seq(
  ("org.scala-lang.modules" %% "scala-parser-combinators" %  "1.1.2").withDottyCompat(scalaVersion.value),
  ("org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.6").withDottyCompat(scalaVersion.value),
  ("org.typelevel" %% "cats-core" % "2.1.1").withDottyCompat(scalaVersion.value)
)

organization := "me.jeffshaw.harmony"

name := "harmony_cats0.8.1_scalaz7.2"

version := "1.0"

scalaVersion := "2.12.1"

crossScalaVersions := Seq("2.11.8", "2.10.6")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.8.1",
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  "org.scalacheck" %% "scalacheck" % "1.12.6" % Test
)

//better type syntax from https://github.com/non/kind-projector
addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary)

libraryDependencies ++= (scalaBinaryVersion.value match {
  case "2.10" =>
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full) :: Nil
  case _ =>
    Nil
})

scalacOptions := Seq("-Xlog-implicits")

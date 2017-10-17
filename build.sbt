lazy val root =
  project.in(file(".")).
  settings(publishArtifact := false).
  aggregate(coreJS, coreJVM, mtlJS, mtlJVM)

lazy val core =
  crossProject.in(file("core")).
    settings(
      organization := Common.organization,
      name := "harmony_cats1.0.0-MF_scalaz7.2",
      version := Common.version,
      mimaPreviousArtifacts := Set(),
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-free" % "1.0.0-MF",
        "org.scalaz" %%% "scalaz-core" % "7.2.15",
        //  "org.scalaz" %% "scalaz-tests" % "7.2.8" % Test classifier "tests",
        "org.scalatest" %%% "scalatest" % "3.0.1" % Test,
        "org.scalacheck" %%% "scalacheck" % "1.13.4" % Test
      ),
      libraryDependencies ++= (scalaBinaryVersion.value match {
        case "2.10" =>
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full) :: Nil
        case _ =>
          Nil
      }),
      //better type syntax from https://github.com/non/kind-projector
      addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary),
      pomExtra := Common.pomExtra
)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val mtl =
  crossProject.in(file("mtl")).
  settings(
    organization := Common.organization,
    name := "harmony_cats-mtl-core0.0.2_scalaz7.2",
    version := Common.version,
    mimaPreviousArtifacts := Set(),
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-mtl-core" % "0.0.2",
      //  "org.scalaz" %% "scalaz-tests" % "7.2.8" % Test classifier "tests",
      "org.scalatest" %%% "scalatest" % "3.0.1" % Test,
      "org.scalacheck" %%% "scalacheck" % "1.13.4" % Test
    ),
    libraryDependencies ++= (scalaBinaryVersion.value match {
      case "2.10" =>
        compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full) :: Nil
      case _ =>
        Nil
    }),
    //better type syntax from https://github.com/non/kind-projector
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary),
    pomExtra := Common.pomExtra
  ).
  dependsOn(core)

lazy val mtlJVM = mtl.jvm
lazy val mtlJS = mtl.js

scalaVersion in ThisBuild := "2.12.3"
crossScalaVersions in ThisBuild := Seq("2.11.11", "2.10.6")

lazy val root =
  project.in(file(".")).
  settings(publishArtifact := false).
  aggregate(harmonyJS, harmonyJVM)

lazy val harmony =
  crossProject.in(file(".")).
    settings(
      organization := "me.jeffshaw.harmony",
      name := "harmony_cats0.9.0_scalaz7.2",
      version := "1.1-SNAPSHOT",
      scalaVersion := "2.12.1",
      crossScalaVersions := Seq("2.11.8", "2.10.6"),
      mimaPreviousArtifacts := Set("me.jeffshaw.harmony" %%% "harmony_cats0-8-1_scalaz7-2" % "1.0"),
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats" % "0.9.0",
        "org.scalaz" %%% "scalaz-core" % "7.2.8",
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
      pomExtra :=
        <url>https://github.com/shawjef3/Harmony</url>
          <licenses>
            <license>
              <name>Apache License, Version 2.0</name>
              <url>https://www.apache.org/licenses/LICENSE-2.0.txt</url>
              <distribution>repo</distribution>
              <comments>A business-friendly OSS license</comments>
            </license>
          </licenses>
          <developers>
            <developer>
              <name>Jeff Shaw</name>
              <id>shawjef3</id>
              <url>https://github.com/shawjef3/</url>
            </developer>
          </developers>
          <scm>
            <url>git@github.com:rocketfuel/Harmony.git</url>
            <connection>scm:git:git@github.com:rocketfuel/Harmony.git</connection>
          </scm>
)

lazy val harmonyJVM = harmony.jvm
lazy val harmonyJS = harmony.js

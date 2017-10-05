import sbt.Keys.pomExtra

object Common {
  val organization = "me.jeffshaw.harmony"

  val version = "1.2"

  val pomExtra =
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
}
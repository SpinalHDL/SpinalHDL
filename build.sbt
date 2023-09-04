import sbt.Keys._
import sbt._
import sbt.Tests._

val scalatestVersion = "3.2.14"
val defaultSettings = Defaults.coreDefaultSettings ++ xerial.sbt.Sonatype.sonatypeSettings ++ Seq(
  organization := "com.github.spinalhdl",
  version      := SpinalVersion.all,
  crossScalaVersions := SpinalVersion.compilers,
  scalaVersion := SpinalVersion.compilers(0),
  scalacOptions ++= Seq("-unchecked","-target:jvm-1.8"/*, "-feature" ,"-deprecation"*/),
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  fork := true,

  scalafmtFilter.withRank(KeyRanks.Invisible) := "diff-ref=dev",
  scalafmtPrintDiff := true,

  //Enable parallel tests
  Test / testForkedParallel := true,
  Test / testGrouping := (Test / testGrouping).value.flatMap { group =>
//    for(i <- 0 until 4) yield {
//      Group("g" + i,  group.tests.zipWithIndex.filter(_._2 % 4 == i).map(_._1), SubProcess(ForkOptions()))
//    }
    Seq(Group("g",  group.tests, SubProcess(ForkOptions())))
  },
//  concurrentRestrictions := Seq(Tags.limit(Tags.ForkedTestGroup, 4)),

  libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value,
  libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test",
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.10", //Ensure that scalatic version 3.2.5 is avoided

  dependencyOverrides += "net.java.dev.jna" % "jna" % "5.12.1",
  dependencyOverrides += "net.java.dev.jna" % "jna-platform" % "5.12.1",
  dependencyOverrides += "org.slf4j" % "slf4j-api" % "2.0.5",
  dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "1.3.0",

  //set SBT_OPTS="-Xmx2G"
  //sbt +clean +reload +publishSigned
  //https://oss.sonatype.org
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := (_ => false),
  pomExtra := {
    <url>github.com/SpinalHDL/SpinalHDL</url>
      <licenses>
        <license>
          <name>LGPL3</name>
          <url>https://www.gnu.org/licenses/lgpl.html</url>
        </license>
      </licenses>
      <scm>
        <connection>scm:git:github.com/SpinalHDL/SpinalHDL</connection>
        <developerConnection>scm:git:git@github.com:SpinalHDL/SpinalHDL</developerConnection>
        <url>github.com/SpinalHDL/SpinalHDL</url>
      </scm>
      <developers>
        <developer>
          <id>Dolu1990</id>
          <name>Dolu1990</name>
          <url>none</url>
        </developer>
      </developers>
  },

  publishTo := {
    val v = version.value
    val nexus = "https://oss.sonatype.org/"
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
 )

lazy val all = (project in file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .settings(
    defaultSettings,
    name := "SpinalHDL-all",
    version := SpinalVersion.all,
    publishArtifact := false,
    publishLocal := {},
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(lib, core)
  )
  .aggregate(sim, idslpayload, idslplugin, core, lib, tester)


import sys.process._
def gitHash(dir: File) = (try {
  s"git -C ${dir.toString} rev-parse HEAD".!!
} catch{
  case e : java.io.IOException => "???"
}).linesIterator.next()



lazy val idslpayload = (project in file("idslpayload"))
  .settings(
    defaultSettings,
    name := "SpinalHDL-idsl-payload",
    version := SpinalVersion.sim,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val idslplugin = (project in file("idslplugin"))
  .dependsOn(idslpayload)
  .settings(
    defaultSettings,
    name := "SpinalHDL-idsl-plugin",
    exportJars := true,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value
    )
  )

lazy val sim = (project in file("sim"))
  .settings(
    defaultSettings,
    name := "SpinalHDL-sim",
    libraryDependencies += "commons-io" % "commons-io" % "2.11.0",
    libraryDependencies += "net.openhft" % "affinity" % "3.23.2",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.5",
    libraryDependencies += "com.github.oshi" % "oshi-core" % "6.4.0",
    version := SpinalVersion.sim
  )

val defaultSettingsWithPlugin = defaultSettings ++ Seq(
  scalacOptions += (idslplugin / Compile / packageBin / artifactPath).map { file =>
    s"-Xplugin:${file.getAbsolutePath}"
  }.value
)

lazy val core = (project in file("core"))
  .dependsOn(idslplugin)
  .settings(
    defaultSettingsWithPlugin,
    name := "SpinalHDL-core",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0",
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.3.0",

    resolvers += Resolver.sonatypeRepo("public"),
    version := SpinalVersion.core,
    Compile / sourceGenerators += Def.task {
      val dir = (Compile / sourceManaged).value
      dir.mkdirs()
      val file = dir / "Info.scala"
      IO.write(file, """package spinal.core
                       |object Info {
                       |  val version = "%s"
                       |  val name = "%s"
                       |  val gitHash = "%s"
                       |}
                       |""".stripMargin.format(SpinalVersion.core, name, gitHash(dir)))
      Seq(file)
    }.taskValue
  )
  .dependsOn(sim)

lazy val lib = (project in file("lib"))
  .settings(
    defaultSettingsWithPlugin,
    name := "SpinalHDL-lib",
    libraryDependencies += "commons-io" % "commons-io" % "2.11.0",
    version := SpinalVersion.lib,
  )
  .dependsOn (sim, core)



lazy val tester = (project in file("tester"))
  .settings(
    defaultSettingsWithPlugin,
    name := "SpinalHDL-tester",
    version := SpinalVersion.tester,
    Test / baseDirectory := file("./"),
    libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion,
  )
  .dependsOn(sim, core, lib)

// Assembly

assembly / assemblyJarName := "spinalhdl.jar"

assembly / test := {}

Test / testOptions += Tests.Argument("-l", "spinal.tester.formal")
addCommandAlias("testFormal", "testOnly * -- -n spinal.tester.formal")
addCommandAlias("testWithoutFormal", "testOnly * -- -l spinal.tester.formal")

assembly / assemblyOutputPath := file("./release/spinalhdl.jar")

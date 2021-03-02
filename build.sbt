import sbt.Keys._
import sbt._
import sbt.Tests._


val defaultSettings = Defaults.coreDefaultSettings ++ xerial.sbt.Sonatype.sonatypeSettings ++ Seq(
  organization := "com.github.spinalhdl",
  version      := SpinalVersion.all,
  scalaVersion := SpinalVersion.compiler,
  scalacOptions ++= Seq("-unchecked","-target:jvm-1.7"/*, "-feature" ,"-deprecation"*/),
  javacOptions ++= Seq("-source", "1.7", "-target", "1.7"),
  baseDirectory in test := file("/out/"),
  fork := true,

  //Enable parallel tests
  Test / testForkedParallel := true,
  testGrouping in Test := (testGrouping in Test).value.flatMap { group =>
//    for(i <- 0 until 4) yield {
//      Group("g" + i,  group.tests.zipWithIndex.filter(_._2 % 4 == i).map(_._1), SubProcess(ForkOptions()))
//    }
    Seq(Group("g",  group.tests, SubProcess(ForkOptions())))
  },
//  concurrentRestrictions := Seq(Tags.limit(Tags.ForkedTestGroup, 4)),

  libraryDependencies += "org.scala-lang" % "scala-library" % SpinalVersion.compiler,

  dependencyOverrides += "net.java.dev.jna" % "jna" % "4.2.2",
  dependencyOverrides += "net.java.dev.jna" % "jna-platform" % "4.2.2",
  dependencyOverrides += "org.slf4j" % "slf4j-api" % "1.7.25",
  dependencyOverrides += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5",

  //set SBT_OPTS="-Xmx2G"
  //sbt clean reload publishSigned
  //https://oss.sonatype.org
  publishMavenStyle := true,
  publishArtifact in Test := false,
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
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(lib, core)
  )
  .aggregate(sim, idslpayload, idslplugin, core, lib, debugger, tester)


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
    version := SpinalVersion.sim
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
    libraryDependencies += "commons-io" % "commons-io" % "2.4",
    libraryDependencies += "net.openhft" % "affinity" % "3.1.11",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.25",
    libraryDependencies += "com.github.dblock" % "oshi-core" % "3.4.0",
    version := SpinalVersion.sim
  )

val defaultSettingsWithPlugin = defaultSettings ++ Seq(
  scalacOptions += (artifactPath in(idslplugin, Compile, packageBin)).map { file =>
    s"-Xplugin:${file.getAbsolutePath}"
  }.value
)

lazy val core = (project in file("core"))
  .dependsOn(idslplugin)
  .settings(
    defaultSettingsWithPlugin,
    name := "SpinalHDL-core",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "com.github.scopt" %% "scopt" % "3.4.0",

    resolvers += Resolver.sonatypeRepo("public"),
    version := SpinalVersion.core,
    sourceGenerators in Compile += Def.task {
      val dir = (sourceManaged in Compile).value
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
    libraryDependencies += "commons-io" % "commons-io" % "2.4",
    version := SpinalVersion.lib
  )
  .dependsOn (sim, core)


lazy val debugger = (project in file("debugger"))
  .settings(
    defaultSettingsWithPlugin,
    name := "SpinalHDL Debugger",
    version := SpinalVersion.debugger,
    resolvers += "sparetimelabs" at "https://www.sparetimelabs.com/maven2/",
    libraryDependencies += "com.github.purejavacomm" % "purejavacomm" % "1.0.2.RELEASE",
    libraryDependencies += "net.liftweb" %% "lift-json" % "3.1.0-M2",
    publishArtifact := false,
    publishLocal := {}
  )
.dependsOn(sim, core, lib/*, ip*/)

lazy val demo = (project in file("demo"))
  .settings(
    defaultSettingsWithPlugin,
    name := "SpinalHDL-demo",
    version := SpinalVersion.demo,
    publishArtifact := false,
    publishLocal := {}
  )
  .dependsOn(sim, core, lib, debugger)


lazy val tester = (project in file("tester"))
  .settings(
    defaultSettingsWithPlugin,
    name := "SpinalHDL-tester",
    version := SpinalVersion.tester,
    baseDirectory in (Test) := file("./"),

    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1",
    publishArtifact := false,
    publishLocal := {}
  )
  .dependsOn(sim, core, lib, debugger,demo)

// Assembly

assemblyJarName in assembly := "spinalhdl.jar"

test in assembly := {}

assemblyOutputPath in assembly := file("./release/spinalhdl.jar")

//To publish the scala doc :
//rm -rf ghpages
//sbt clean compile unidoc
//git clone https://github.com/SpinalHDL/SpinalHDL.git -b gh-pages --depth=1 ghpages
//rm -rf ghpages/*
//cp -r target/scala-2.11/unidoc/* ghpages
//cd ghpages
//git add *
//git commit -m "publish doc"
//git push
//cd ..
//rm -rf ghpages

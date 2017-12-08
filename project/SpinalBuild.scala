import sbt.Keys._
import sbt._
import xerial.sbt.Sonatype.SonatypeKeys._


object SpinalBuild extends Build {
  lazy val all = Project(
    id = "SpinalHDL-all",
    base = file("."),
    settings = defaultSettings ++ Seq(
      name := "SpinalHDL all",
      version := SpinalVersion.all,
      publishTo := None
    ),
    aggregate = Seq(sim, core, lib, debugger, tester)
  )

  import sys.process._
  def gitHash = (try {
    "git rev-parse HEAD".!!
  } catch{
    case e : java.io.IOException => "???"
  }).linesIterator.next()


  lazy val sim = Project(
    id = "SpinalHDL-sim",
    base = file("sim"),
    settings = defaultSettings ++ Seq(
      name := "SpinalHDL Sim",
      version := SpinalVersion.sim,
      libraryDependencies += "com.github.jnr" % "jnr-ffi" % "2.1.7"
    )
  )


  lazy val core = Project(
    id = "SpinalHDL-core",
    base = file("core"),
    settings = defaultSettings  ++  Seq(
      name := "SpinalHDL Core",
      libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      libraryDependencies += "com.github.scopt" %% "scopt" % "3.4.0",

      addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.6" % "1.0.2"),
      libraryDependencies += "org.scala-lang.plugins" %% "scala-continuations-library" % "1.0.2",
      scalacOptions += "-P:continuations:enable",

      resolvers += Resolver.sonatypeRepo("public"),
      version := SpinalVersion.core,
      sourceGenerators in Compile <+= (sourceManaged in Compile, version, name) map { (d, v, n) =>
        val file = d / "Info.scala"
        IO.write(file, """package spinal.core
                         |object Info {
                         |  val version = "%s"
                         |  val name = "%s"
                         |  val gitHash = "%s"
                         |}
                         |""".stripMargin.format(v, n,gitHash))
        Seq(file)
      }
    )
  ) dependsOn (sim)


  lazy val lib = Project(
    id = "SpinalHDL-lib",
    base = file("lib"),
    settings = defaultSettings ++ Seq(
      name := "SpinalHDL Lib",
      libraryDependencies += "commons-io" % "commons-io" % "2.4",
      version := SpinalVersion.lib
    )
  ) dependsOn (core)

  lazy val debugger = Project(
    id = "SpinalHDL-debugger",
    base = file("debugger"),
    settings = defaultSettings ++ Seq(
      name := "SpinalHDL Debugger",
      version := SpinalVersion.debugger,
      resolvers += "sparetimelabs" at "http://www.sparetimelabs.com/maven2/",
      //libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.40-R8",
      libraryDependencies += "com.github.purejavacomm" % "purejavacomm" % "1.0.2.RELEASE",
      libraryDependencies += "net.liftweb" %% "lift-json" % "3.1.0-M2",
      publishTo := None
    )
  ) dependsOn(core, lib/*, ip*/)

  lazy val demo = Project(
    id = "SpinalHDL-demo",
    base = file("demo"),
    settings = defaultSettings ++ Seq(
      name := "SpinalHDL Demo",
      version := SpinalVersion.demo,
      publishTo := None
    )
  ) dependsOn(core, lib/*, ip*/ ,debugger)


  lazy val tester = Project(
    id = "SpinalHDL-tester",
    base = file("tester"),
    settings = defaultSettings ++ Seq(
      name := "SpinalHDL tester",
      version := SpinalVersion.tester,
      libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1",
      libraryDependencies += "net.openhft" % "compiler" % "2.3.0",
      //libraryDependencies += "com.storm-enroute" %% "scalameter" % "latest.release",
      publishTo := None
    )
  ) dependsOn(core, lib, debugger,demo)

  //sbt clean reload publishSigned
  //https://oss.sonatype.org
  lazy val defaultSettings = Defaults.defaultSettings ++ xerial.sbt.Sonatype.sonatypeSettings ++ Seq(
    organization := "com.github.spinalhdl",
    scalaVersion := SpinalVersion.compiler,
    scalacOptions ++= Seq("-unchecked","-target:jvm-1.7"/*, "-feature" ,"-deprecation"*/),
    javacOptions ++= Seq("-source", "1.7", "-target", "1.7"),
    baseDirectory in test := file("/out/"),
    profileName := "Dolu1990",
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
    }
  )
}
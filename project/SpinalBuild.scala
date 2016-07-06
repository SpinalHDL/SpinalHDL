import sbt.Keys._
import sbt._
import xerial.sbt.Sonatype.SonatypeKeys
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
    aggregate = Seq(core, lib, debugger, tester)
  )

  lazy val core = Project(
    id = "SpinalHDL-core",
    base = file("core"),
    settings = defaultSettings ++ Seq(
      name := "SpinalHDL Core",
      libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      libraryDependencies += "com.github.scopt" %% "scopt" % "3.4.0",
      resolvers += Resolver.sonatypeRepo("public"),
      version := SpinalVersion.core
    )
  )

  lazy val lib = Project(
    id = "SpinalHDL-lib",
    base = file("lib"),
    settings = defaultSettings ++ Seq(
      name := "SpinalHDL Lib",
      version := SpinalVersion.lib
    )
  ) dependsOn (core)

  lazy val ip = Project(
    id = "SpinalHDL-ip",
    base = file("ip"),
    settings = defaultSettings ++ Seq(
      name := "SpinalHDL ip",
      version := SpinalVersion.ip
    )
  ) dependsOn (core,lib)

  lazy val debugger = Project(
    id = "SpinalHDL-debugger",
    base = file("debugger"),
    settings = defaultSettings ++ Seq(
      name := "SpinalHDL Debugger",
      version := SpinalVersion.debugger,
      resolvers += "sparetimelabs" at "http://www.sparetimelabs.com/maven2/",
      //libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.40-R8",
      libraryDependencies += "com.sparetimelabs" % "purejavacomm" % "0.0.22",
      libraryDependencies += "net.liftweb" %% "lift-json" % "latest.release",
      publishTo := None
    )
  ) dependsOn(core, lib, ip)

  lazy val demo = Project(
    id = "SpinalHDL-demo",
    base = file("demo"),
    settings = defaultSettings ++ Seq(
      name := "SpinalHDL Demo",
      version := SpinalVersion.demo,
      publishTo := None
    )
  ) dependsOn(core, lib, ip ,debugger)


  lazy val tester = Project(
    id = "SpinalHDL-tester",
    base = file("tester"),
    settings = defaultSettings ++ Seq(
      name := "SpinalHDL tester",
      version := SpinalVersion.tester,
      libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1",
      //libraryDependencies += "com.storm-enroute" %% "scalameter" % "latest.release",
      publishTo := None
    )
  ) dependsOn(core, lib, ip, debugger,demo)

  //sbt clean reload publishSigned
  //https://oss.sonatype.org
  lazy val defaultSettings = Defaults.defaultSettings ++ xerial.sbt.Sonatype.sonatypeSettings ++ Seq(
    organization := "com.github.spinalhdl",
    scalaVersion := SpinalVersion.compiler,
    scalacOptions ++= Seq("-unchecked"/*, "-feature" ,"-deprecation"*/),
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
import sbt._
import Keys._

object GraphBuild extends Build {
  lazy val all = Project(
    id = "SpinalHDL-all",
    base = file("."),
    settings = defaultSettings ++ Seq(
      name      := "SpinalHDL all",
      version   := Version.all,
      publishTo := None
	  ),
    aggregate = Seq(core,lib,test)
  )

  lazy val core = Project(
    id = "SpinalHDL-core",
    base = file("Core"),
    settings = defaultSettings ++ Seq(
      name      := "SpinalHDL Core",
      version   := Version.core
    )
  )

  lazy val lib = Project(
    id = "SpinalHDL-lib",
    base = file("Lib"),
    settings = defaultSettings ++ Seq(
      name      := "SpinalHDL Lib",
      version   := Version.lib
    )
  ) dependsOn (core)

  lazy val test = Project(
    id = "SpinalHDL-test",
    base = file("Test"),
    settings = defaultSettings ++ Seq(
      name      := "SpinalHDL Test",
      version   := Version.test,
	    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1"
    )
  ) dependsOn (core,lib)
  
  lazy val defaultSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.spinalhdl",
    scalaVersion := Version.compiler,
    scalacOptions ++= Seq("-unchecked","-feature")     
    //"-deprecation"
  )
}
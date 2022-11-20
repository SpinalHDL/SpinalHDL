// build.sc
import mill._, scalalib._, publish._

object SpinalVersion {
  val compilers = List("2.11.12", "2.12.13", "2.13.6")
  val compilerIsRC = false

  val isDev = true
  val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.7.2"
  val all = if (isDev) "dev" else s"$major$snapshot"
  val sim = all
  val core = all
  val lib = all
  val ip = all
  val debugger = all
  val demo = all
  val tester = all
}

trait CommonModule extends ScalaModule { outer =>
  def scalaVersion = SpinalVersion.compilers(0)

  val IvyDeps = Agg(
    ivy"org.scala-lang:scala-library:${scalaVersion}",
    ivy"net.java.dev.jna:jna:5.5.0",
    ivy"net.java.dev.jna:jna-platform:5.5.0",
    ivy"org.slf4j:slf4j-api:1.7.25",
    ivy"org.scala-lang.modules::scala-xml:1.2.0"
  )

  def testArgs = T { Seq.empty[String] }

  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest::3.2.5")
  }
}

object idslpayload extends SbtModule with CommonModule {
  def mainClass = Some("spinal.idslpayload")
  def ivyDeps = super.ivyDeps() ++ Agg(ivy"org.scala-lang:scala-reflect:${scalaVersion}")
}

object idslplugin extends SbtModule with CommonModule {
  def mainClass = Some("spinal.idslplugin")
  def moduleDeps = Seq(idslpayload)
  def ivyDeps = super.ivyDeps() ++ Agg(ivy"org.scala-lang:scala-compiler:${scalaVersion}")
}

object sim extends SbtModule with CommonModule {
  def mainClass = Some("spinal.sim")
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"commons-io:commons-io:2.4",
    ivy"net.openhft:affinity:3.21ea1.1",
    ivy"org.slf4j:slf4j-simple:1.7.25",
    ivy"com.github.oshi:oshi-core:5.2.0"
  )
  def publishVersion = SpinalVersion.sim
}

object lib extends SbtModule with CommonModule {
  def mainClass = Some("spinal.lib")
  def moduleDeps = Seq(core, sim)
  def ivyDeps = super.ivyDeps() ++ Agg(ivy"commons-io:commons-io:2.4", ivy"org.scalatest::scalatest:3.2.5")
  def publishVersion = SpinalVersion.lib
}

import sys.process._
def gitHash(dir: os.Path) = (try {
  s"git -C ${dir.toString} rev-parse HEAD".!!
} catch {
  case e: java.io.IOException => "???"
}).linesIterator.next()

object core extends SbtModule with CommonModule {
  def mainClass = Some("spinal.core")
  def moduleDeps = Seq(idslplugin, sim)

  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.scala-lang:scala-reflect:${scalaVersion}",
    ivy"com.github.scopt::scopt:3.7.1",
    ivy"com.lihaoyi::sourcecode:0.2.7"
  )

  override def generatedSources = T {
    val dest = T.dest / "Info.scala"
    val code =
      s"""package spinal.core
         |object Info {
         |  val version = "%s"
         |  val name = "%s"
         |  val gitHash = "%s"
         |}
         |""".stripMargin.format(SpinalVersion.core, mainClass, gitHash(T.dest))
    os.write(dest, code, createFolders = true)
    Seq(PathRef(T.dest))
  }

  def publishVersion = SpinalVersion.all

  def pomSettings = PomSettings(
    description = "SpinalHDL",
    organization = "com.github.spinalhdl",
    url = "https://github.com/SpinalHDL/SpinalHDL",
    licenses = Seq(License.`LGPL-3.0-or-later`),
    versionControl = VersionControl.github("SpinalHDL", "SpinalHDL"),
    developers = Seq(
      Developer("Dolu1990", "SpinalHDL", "https://github.com/SpinalHDL")
    )
  )
}

object tester extends SbtModule with CommonModule {
  def mainClass = Some("spinal.tester")
  def moduleDeps = Seq(core, sim, lib)
  def ivyDeps = super.ivyDeps() ++ Agg(ivy"org.scalatest::scalatest:3.2.5")
}

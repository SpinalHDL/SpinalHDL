// build.sc
import mill._, scalalib._, publish._
import $file.project.Version

trait SpinalModule extends SbtModule with CrossSbtModule { outer =>
  def scalatestVersion = "3.2.14"
  def scalacOptions = super.scalacOptions() ++ Seq("-unchecked", "-target:jvm-1.8")
  def javacOptions = super.javacOptions() ++ Seq("-source", "1.8", "-target", "1.8")

  val IvyDeps = Agg(
    ivy"org.scala-lang:scala-library:${scalaVersion}",
    ivy"org.scalactic:scalactic::3.2.10",
    ivy"net.java.dev.jna:jna:5.12.1",
    ivy"net.java.dev.jna:jna-platform:5.12.1",
    ivy"org.slf4j:slf4j-api:2.0.5",
    ivy"org.scala-lang.modules::scala-xml:1.3.0"
  )

  object test extends CrossSbtModuleTests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest::${scalatestVersion}")
  }
  def testOnly(args: String*) = T.command { test.testOnly(args: _*) }

  // Default definitions for moduleDeps.  For projects that consume us as a
  // foreign module (with a git submodule), override these to avoid building
  // these modules multiple times
  def coreMod = Some(core(crossScalaVersion))
  def libMod = Some(lib(crossScalaVersion))
  def simMod = Some(sim(crossScalaVersion))
  def idslpayloadMod = Some(idslpayload(crossScalaVersion))
  def idslpluginMod = Some(idslplugin(crossScalaVersion))
}

trait SpinalPublishModule extends PublishModule {
  def publishVersion = Version.SpinalVersion.all
  def artifactName = "spinalhdl-" + super.artifactName()

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

object idslpayload extends Cross[IdslPayload](Version.SpinalVersion.compilers)
trait IdslPayload extends SpinalModule with SpinalPublishModule {
  def mainClass = Some("spinal.idslpayload")
  override def artifactName = "spinalhdl-idsl-payload"
  def ivyDeps = super.ivyDeps() ++ Agg(ivy"org.scala-lang:scala-reflect:${scalaVersion}")
}

object idslplugin extends Cross[IdslPlugin](Version.SpinalVersion.compilers)
trait IdslPlugin extends SpinalModule with SpinalPublishModule {
  def mainClass = Some("spinal.idslplugin")
  override def artifactName = "spinalhdl-idsl-plugin"
  def moduleDeps = Seq(idslpayloadMod.get)
  def ivyDeps = super.ivyDeps() ++ Agg(ivy"org.scala-lang:scala-compiler:${scalaVersion}")
  def pluginOptions = T { Seq(s"-Xplugin:${assembly().path}") }
}

object sim extends Cross[Sim](Version.SpinalVersion.compilers){
  def defaultCrossSegments = Seq(Version.SpinalVersion.compilers.head)
}
trait Sim extends SpinalModule with SpinalPublishModule {
  def mainClass = Some("spinal.sim")
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"commons-io:commons-io:2.11.0",
    ivy"net.openhft:affinity:3.23.2",
    ivy"org.slf4j:slf4j-simple:2.0.5",
    ivy"com.github.oshi:oshi-core:6.4.0"
  )
  def publishVersion = Version.SpinalVersion.sim
}

object lib extends Cross[Lib](Version.SpinalVersion.compilers){
  def defaultCrossSegments = Seq(Version.SpinalVersion.compilers.head)
}
trait Lib extends SpinalModule with SpinalPublishModule {
  def mainClass = Some("spinal.lib")
  def moduleDeps = Seq(coreMod.get, simMod.get)
  def scalacOptions = super.scalacOptions() ++ idslpluginMod.get.pluginOptions()
  def ivyDeps = super.ivyDeps() ++ Agg(ivy"commons-io:commons-io:2.11.0", ivy"org.scalatest::scalatest:${scalatestVersion}")
  def publishVersion = Version.SpinalVersion.lib
}

import sys.process._
def gitHash(dir: os.Path) = (try {
  s"git -C ${dir.toString} rev-parse HEAD".!!
} catch {
  case e: java.io.IOException => "???"
}).linesIterator.next()

object core extends Cross[Core](Version.SpinalVersion.compilers){
  def defaultCrossSegments = Seq(Version.SpinalVersion.compilers.head)
}
trait Core extends SpinalModule with SpinalPublishModule {
  def mainClass = Some("spinal.core")
  def moduleDeps = Seq(idslpluginMod.get, simMod.get)

  def scalacOptions = super.scalacOptions() ++ idslpluginMod.get.pluginOptions()
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.scala-lang:scala-reflect:${scalaVersion}",
    ivy"com.github.scopt::scopt:4.1.0",
    ivy"com.lihaoyi::sourcecode:0.3.0"
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
            |""".stripMargin.format(Version.SpinalVersion.core, mainClass, gitHash(T.dest))
    os.write(dest, code, createFolders = true)
    Seq(PathRef(T.dest))
  }
}

object tester extends Cross[Tester](Version.SpinalVersion.compilers){
  def defaultCrossSegments = Seq(Version.SpinalVersion.compilers.head)
}
trait Tester extends SpinalModule with SpinalPublishModule {
  override def millSourcePath = os.pwd / "tester"
  def mainClass = Some("spinal.tester")
  def moduleDeps = Seq(coreMod.get, simMod.get, libMod.get)
  def scalacOptions = super.scalacOptions() ++ idslpluginMod.get.pluginOptions()
  def ivyDeps = super.ivyDeps() ++ Agg(ivy"org.scalatest::scalatest:${scalatestVersion}")
  def publishVersion = Version.SpinalVersion.tester
}

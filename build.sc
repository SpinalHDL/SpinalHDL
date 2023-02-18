// build.sc
import mill._, scalalib._, publish._
import $file.project.Version

trait SpinalModule extends SbtModule { outer =>
  def scalaVersion = Version.SpinalVersion.compilers(0)
  def scalacOptions = super.scalacOptions() ++ Seq("-unchecked", "-target:jvm-1.8")
  def javacOptions = super.javacOptions() ++ Seq("-source", "1.8", "-target", "1.8")

  val IvyDeps = Agg(
    ivy"org.scala-lang:scala-library:${scalaVersion}",
    ivy"net.java.dev.jna:jna:5.5.0",
    ivy"net.java.dev.jna:jna-platform:5.5.0",
    ivy"org.slf4j:slf4j-api:1.7.25",
    ivy"org.scala-lang.modules::scala-xml:1.2.0"
  )
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

object idslpayload extends SpinalModule with SpinalPublishModule {
  def mainClass = Some("spinal.idslpayload")
  override def artifactName = "spinalhdl-idsl-payload"
  def ivyDeps = super.ivyDeps() ++ Agg(ivy"org.scala-lang:scala-reflect:${scalaVersion}")
}

object idslplugin extends SpinalModule with SpinalPublishModule {
  def mainClass = Some("spinal.idslplugin")
  override def artifactName = "spinalhdl-idsl-plugin"
  def moduleDeps = Seq(idslpayload)
  def ivyDeps = super.ivyDeps() ++ Agg(ivy"org.scala-lang:scala-compiler:${scalaVersion}")
  def pluginOptions = T { Seq(s"-Xplugin:${assembly().path}") }
}

object sim extends SpinalModule with SpinalPublishModule {
  def mainClass = Some("spinal.sim")
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"commons-io:commons-io:2.4",
    ivy"net.openhft:affinity:3.21ea1.1",
    ivy"org.slf4j:slf4j-simple:1.7.25",
    ivy"com.github.oshi:oshi-core:5.2.0"
  )
  def publishVersion = Version.SpinalVersion.sim
}

object lib extends SpinalModule with SpinalPublishModule {
  def mainClass = Some("spinal.lib")
  def moduleDeps = Seq(core, sim)
  def scalacOptions = super.scalacOptions() ++ idslplugin.pluginOptions()
  def ivyDeps = super.ivyDeps() ++ Agg(ivy"commons-io:commons-io:2.4", ivy"org.scalatest::scalatest:3.2.5")
  def publishVersion = Version.SpinalVersion.lib
}

import sys.process._
def gitHash(dir: os.Path) = (try {
  s"git -C ${dir.toString} rev-parse HEAD".!!
} catch {
  case e: java.io.IOException => "???"
}).linesIterator.next()

object core extends SpinalModule with SpinalPublishModule {
  def mainClass = Some("spinal.core")
  def moduleDeps = Seq(idslplugin, sim)

  def scalacOptions = super.scalacOptions() ++ idslplugin.pluginOptions()
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
            |""".stripMargin.format(Version.SpinalVersion.core, mainClass, gitHash(T.dest))
    os.write(dest, code, createFolders = true)
    Seq(PathRef(T.dest))
  }
}

object tester extends SpinalModule with SpinalPublishModule {
  def mainClass = Some("spinal.tester")
  def moduleDeps = Seq(core, sim, lib)
  def scalacOptions = super.scalacOptions()
  def ivyDeps = super.ivyDeps() ++ Agg(ivy"org.scalatest::scalatest:3.2.5")

  object test extends Tests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest::3.2.5")
  }
}

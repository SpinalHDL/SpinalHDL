About SpinalHDL
============



Getting started
===============
SBT =>

    scalaVersion := "2.11.2"

    libraryDependencies ++= Seq(
      "com.github.spinalhdl" % "spinalhdl-core_2.11" % "latest.release",
      "com.github.spinalhdl" % "spinalhdl-lib_2.11" % "latest.release"
    )

JAR =>

    https://oss.sonatype.org/content/groups/public/com/github/spinalhdl/spinalhdl-core_2.11/
    https://oss.sonatype.org/content/groups/public/com/github/spinalhdl/spinalhdl-lib_2.11/

Example =>

    import spinal._

    class MyTopLevel extends Component {
      val io = new Bundle {
        val a = in Bool()
        val b = in Bool()
        val c = out Bool()
      }
      io.c := io.a && io.b
    }
    object MyTopLevel {
      def main(args: Array[String]) {
        SpinalVhdl(new MyTopLevel)
      }
    }


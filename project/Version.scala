import com.typesafe.config._
import sourcecode.File
import sbt.io.Path
import scala.collection.JavaConverters._

object SpinalVersion {  
  val currentFile = new java.io.File(sourcecode.File())
  val configFilePath = currentFile.getParent + Path.sep + "version.conf"
  val conf = ConfigFactory.parseFile(new java.io.File(configFilePath)).resolve()

  val compilers = conf.getStringList("compilers").asScala.toList
  val compilerIsRC = conf.getBoolean("compilerIsRC")

  val isDev = conf.getBoolean("isDev")
  val isSnapshot = conf.getBoolean("isSnapshot")
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = conf.getString("major")
  val all         = if(isDev) "dev" else s"$major$snapshot"
  val sim         = all
  val core        = all
  val lib         = all
  val ip          = all
  val debugger    = all
  val demo        = all
  val tester      = all
}

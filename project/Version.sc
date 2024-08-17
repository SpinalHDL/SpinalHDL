import com.typesafe.config._
import java.io.File
import scala.collection.JavaConverters._

object SpinalVersion {
  val conf = ConfigFactory.parseFile(new File("project/version.conf")).resolve()

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

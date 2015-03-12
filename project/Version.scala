object SpinalVersion {
  val compiler = "2.11.2"
  val compilerIsRC = false

  val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "0.1.2"
  val all         = s"$major$snapshot"
  val core        = s"$major$snapshot"
  val lib         = s"$major$snapshot"
  val tester        = s"$major$snapshot"


}
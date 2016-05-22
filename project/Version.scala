object SpinalVersion {
  val compiler = "2.11.6"
  val compilerIsRC = false

  val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "0.5.3"
  val all         = s"$major$snapshot"
  val core        = s"$major$snapshot"
  val lib         = s"$major$snapshot"
  val debugger    = s"$major$snapshot"
  val demo        = s"$major$snapshot"
  val tester      = s"$major$snapshot"


}
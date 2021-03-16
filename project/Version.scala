object SpinalVersion {
  val compiler = "2.11.12"
  val compilerIsRC = false

  val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.4.4"
  val all         = s"$major$snapshot"
  val sim         = s"$major$snapshot"
  val core        = s"$major$snapshot"
  val lib         = s"$major$snapshot"
  val ip          = s"$major$snapshot"
  val debugger    = s"$major$snapshot"
  val demo        = s"$major$snapshot"
  val tester      = s"$major$snapshot"
}

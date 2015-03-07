object Version {
  val compiler = "2.11.2"
  val compilerIsRC = false

  private val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.0"
  val all         = s"$major$snapshot"
  val core        = s"$major$snapshot"
  val lib         = s"$major$snapshot"
  val test        = s"$major$snapshot"
}
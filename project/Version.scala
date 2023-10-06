object SpinalVersion {
  val compilers = List("2.11.12", "2.12.18", "2.13.12")
  val compilerIsRC = false

  val isDev = true
  val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.7.3"
  val all         = if(isDev) "dev" else s"$major$snapshot"
  val sim         = all
  val core        = all
  val lib         = all
  val ip          = all
  val debugger    = all
  val demo        = all
  val tester      = all
}

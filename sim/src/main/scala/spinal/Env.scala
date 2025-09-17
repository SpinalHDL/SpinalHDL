package spinal

object SpinalEnv {
  // Path to the GNU make binary
  val makeCmd = getMakeBinaryPath()

  /**
   * This function retrieves the appropriate filesystem path to the "make" binary.
   *
   * It uses the following rules/priorities:
   *   1. If the environment variable "SPINAL_MAKE_CMD" is defined, always use that value
   *   2. If the OS/platform has a specified entry in this function, use that
   *   3. Default to "make"
   *
   * @note SpinalHDL actually requires GNU Make (gmake on non-GNU platforms).
   *
   * @return Filesystem path to the make binary.
   */
  def getMakeBinaryPath(): String = {
    // If the environment variable is defined, always use that
    val envVal = sys.env.get("SPINAL_MAKE_CMD")
    if (envVal.isDefined)
      return envVal.get

    // Platform specific logic
    val osName = System.getProperty("os.name").toLowerCase
    val makeBinaryPath = osName match {
      case "freebsd" => "gmake"
      case _         => "make"
    }

    makeBinaryPath
  }
}

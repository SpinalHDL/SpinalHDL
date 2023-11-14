package spinal

object SpinalEnv {
  val makeCmd = sys.env.getOrElse("SPINAL_MAKE_CMD", "make")
}

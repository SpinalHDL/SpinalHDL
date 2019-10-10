package spinal.lib.dsptool

/**
  * Fixnum
  * @example{{{ import FixSwitchOff._
  *             val a = FixData(-3.785333,SQ(8,4))
  *             :FixData:-3.785333 FixSwitchOff
  *            }}}
  */
sealed class FixSwitch(val pref: String)

object FixSwitchOff{
  implicit val fixButton = new FixSwitch("off")
}

private object FixSwitchOn{
  val fixButton = new FixSwitch("on")
}

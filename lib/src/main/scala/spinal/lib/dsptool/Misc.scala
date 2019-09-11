package spinal.lib.dsptool

trait  RoundType ;
object Ceil             extends RoundType ;// RoundUp
object Floor            extends RoundType ;// RoundDown
object FloorToZero      extends RoundType ;// RoundToZero
object CeilToInf        extends RoundType ;// RoundToInf
object RoundUp          extends RoundType ;// RoundHalfUp
object RoundDown        extends RoundType ;// RoundHalfDown
object RoundToZero      extends RoundType ;// RoundHalfToZero
object RoundToInf       extends RoundType ;// RoundHalfToInf
object RoundToEven      extends RoundType ;// RoundHalfToEven
object RoundToOdd       extends RoundType ;// RoundHalfToOdd

/**
  * Fixnum
  * @example{{{ import FixSwitchOff._
  *             val a = FixData(-3.785333,SQ(8,4))
  *             :FixData:-3.785333 FixSwitchOff
  *            }}}
  */
sealed class FixSwitch(val pref: String);

object FixSwitchOff{
  implicit val fixButton = new FixSwitch("off")
}

private object FixSwitchOn{
  val fixButton = new FixSwitch("on")
}
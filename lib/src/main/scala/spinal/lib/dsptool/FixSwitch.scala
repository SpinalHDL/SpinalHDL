package spinal.lib.dsptool

import spinal.core._

/**Usage:
  * @example {{{ FixOff()
  *             val a = FixData(-3.785333,SQ(8,4)) }}}
  * @return {{{ :FixData:-3.78533 FixOff }}}
  */

object FixOn{
  def apply(): Boolean = FixSwitch on
}

object FixOff{
  def apply(): Boolean = FixSwitch off
}

protected object FixSwitch {
  private var switch: Boolean = true

  def apply(): Boolean = switch

  def state: Boolean = switch

  def on: Boolean = {
    SpinalInfo("FixPoint Switch on")
    switch = true
    state
  }

  def off: Boolean = {
    SpinalInfo("FixPoint Switch off")
    switch = false
    state
  }
}

object getFixSwitchState{
  def apply(): Boolean = FixSwitch.state
}

/* Another user interface:
* 'fix on'  is equivalent to FixOn()
* 'fix off' is equivalent to FixOff()
*/
//object fix{
//  def on: Boolean = FixSwitch on
//  def off: Boolean = FixSwitch off
//}

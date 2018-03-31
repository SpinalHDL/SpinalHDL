/** @todo: support SEL resize*/
package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

object WishboneAdapter{
  /** Create a Wishbone Adapter given the master/slave wishbone bus
    * @param master the wishbone master bus
    * @param slave the wishbone slave bus
    * @param allowAddressResize allow the resize of the address bus. Not allow by default
    * @param allowDataResize allow the resize of the data bus. Not allow by default
    * @param allowTagResize allow the resize of all the tagging busses. Not allow by default
    * @example {{{
    * val wishboneMaster = Wishbone(wishboneConfig(8,8).pipelined)
    * val wishboneSlave = Wishbone(wishboneConfig(16,8))
    * val adapter = WishboneAdapter(wishboneMaster, wishboneSlave, allowAddressResize = true)
    * }}}
    */
  def apply(master: Wishbone,
            slave: Wishbone,
            allowAddressResize : Boolean = false,
            allowDataResize : Boolean = false,
            allowTagResize : Boolean = false): WishboneAdapter = {
    val adapter = new WishboneAdapter(master.config,slave.config,allowAddressResize,allowDataResize,allowTagResize)
    master <> adapter.io.wbm
    slave <> adapter.io.wbs
    adapter.setPartialName(master,"adapter")
  }
}

/** Create a configurable adaptor fot the wishbone bus.
  * It can adapt from a wishbone pipelined interface to a standard one
  * or vice versa
  * @constructor Create a wishbone bus adapter
  * @param wbmConfig the wishboneConfig from the wishbone master bus
  * @param wbsConfig the wishboneConfig from the wishbone slave bus
  * @param allowAddressResize allow the resize of the address bus. Not allow by default
  * @param allowDataResize allow the resize of the data bus. Not allow by default
  * @param allowTagResize allow the resize of all the tagging busses. Not allow by default
  * @example {{{
  * val wishboneMaster = Wishbone(wishboneConfig(8,8).pipelined)
  * val wishboneSlave = Wishbone(wishboneConfig(16,8))
  * val adapter = new WishboneAdapter(wishboneMaster.config, wishboneSlave.config, allowAddressResize = true)
  * wishboneMaster <> adapter.io.wbm
  * adapter.io.wbs <> wishboneSlave
  * }}}
  */
class WishboneAdapter(wbmConfig : WishboneConfig,
                      wbsConfig: WishboneConfig,
                      allowAddressResize : Boolean = false,
                      allowDataResize : Boolean = false,
                      allowTagResize : Boolean = false) extends Component{
  val io = new Bundle {
    val wbm = slave(Wishbone(wbmConfig))
    val wbs = master(Wishbone(wbsConfig))
  }

  io.wbs.connectTo(io.wbm, allowDataResize, allowAddressResize, allowTagResize)

  (wbmConfig.isPipelined,wbsConfig.isPipelined) match{
    case (false,true) => {
      io.wbs.STB.removeAssignments()
      val wsm = new StateMachine{
        io.wbs.STB := False
        val idle : State = new State with EntryPoint{
          whenIsActive{
            io.wbs.STB := io.wbm.STB
            when(io.wbs.STB){
              goto(wait4ack)
            }
          }
        }

        val wait4ack : State = new State{
          whenIsActive{
            when(io.wbs.ACK){
              goto(idle)
            }
          }
        }
      }
    }
    case (true, false) => {
      io.wbm.STALL.removeAssignments()
      io.wbm.STALL := io.wbs.CYC? !io.wbs.ACK | False
    }
    case _ =>
  }
}

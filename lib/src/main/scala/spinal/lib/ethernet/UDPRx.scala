package spinal.lib.ethernet

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis._
import spinal.core.Mem
import ethernet.PacketMTUEnum
import spinal.core.internals.Operator
import spinal.lib.fsm._

import java.util.Calendar
import scala.math._

case class RxGenerics(
    IP_ADDR_WIDTH: Int = 32,
    PORT_WIDTH: Int = 16,
    DATA_WIDTH: Int = 256,
    DATA_BYTE_CNT: Int = 32,
    OCTETS: Int = 8,
    DATA_USE_TLAST: Boolean = true,
    DATA_USE_TUSER: Boolean = true,
    DATA_USE_TKEEP: Boolean = true,
    DATA_USE_TSTRB: Boolean = false,
    DATA_TUSER_WIDTH: Int = 1,
    INPUT_BUFFER_DEPTH: Int = 256
)

class RxTop(
    rxConfig: RxGenerics,
    headerConfig: HeaderRecognizerGenerics
) extends Component {
  val dataAxisCfg = Axi4StreamConfig(
    dataWidth = rxConfig.DATA_BYTE_CNT,
    userWidth = rxConfig.DATA_TUSER_WIDTH,
    useStrb = rxConfig.DATA_USE_TSTRB,
    useKeep = rxConfig.DATA_USE_TKEEP,
    useLast = rxConfig.DATA_USE_TLAST,
    useUser = rxConfig.DATA_USE_TUSER
  )

  val io = new Bundle {

    val dataAxisIn = slave(Axi4Stream(dataAxisCfg))
    val metaOut = master Stream MetaInterface()
    val dataAxisOut = master(Axi4Stream(dataAxisCfg))
  }

  val dataBuffered = io.dataAxisIn.queue(rxConfig.INPUT_BUFFER_DEPTH)

  val headerRecognizer = new HeaderRecognizer(headerConfig)
  headerRecognizer.io.dataAxisIn << dataBuffered

  io.dataAxisOut <-< headerRecognizer.io.dataAxisOut
  io.metaOut <-< headerRecognizer.io.metaOut
}

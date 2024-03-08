package spinal.lib.ethernet

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis._
import UserConfiguration._

case class RxGenerics(
    DATA_WIDTH: Int = DATA_WIDTH,
    DATA_BYTE_CNT: Int = DATA_BYTE_CNT,
    OCTETS: Int = BYTE_WIDTH,
    DATA_USE_TLAST: Boolean = true,
    DATA_USE_TUSER: Boolean = true,
    DATA_USE_TKEEP: Boolean = true,
    DATA_USE_TSTRB: Boolean = false,
    DATA_TUSER_WIDTH: Int = 1,
    DATA_BUFFER_DEPTH: Int = 256,
    HEADER_BUFFER_DEPTH: Int = 16
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
    val metaOut = master Stream MetaData()
    val dataAxisOut = master(Axi4Stream(dataAxisCfg))
  }

  val dataBuffered = io.dataAxisIn.queue(rxConfig.DATA_BUFFER_DEPTH)

  val headerRecognizer = new HeaderRecognizer(headerConfig)
  headerRecognizer.io.dataAxisIn << dataBuffered

  io.dataAxisOut <-/< headerRecognizer.io.dataAxisOut
  io.metaOut <-/< headerRecognizer.io.metaOut
}

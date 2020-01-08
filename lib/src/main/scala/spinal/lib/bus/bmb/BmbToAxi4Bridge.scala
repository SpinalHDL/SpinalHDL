package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

case class BmbToAxi4SharedBridge(bmbConfig : BmbParameter, pendingMax : Int = 7) extends Component{
  val axiConfig = Axi4Config(
    addressWidth = bmbConfig.addressWidth,
    dataWidth    = bmbConfig.dataWidth,
    idWidth      = 0,
    useId        = true,
    useSize      = true,
    useLen       = true,
    useLast      = true,
    useResp      = true,
    useStrb      = true,
    useProt      = true,
    useCache     = true,
    useQos       = false,
    useRegion    = false,
    useBurst     = false,
    useLock      = false
  )

  val io = new Bundle {
    val input = slave(Bmb(bmbConfig))
    val output = master(Axi4Shared(axiConfig))
  }

  val pendingCounter = CounterUpDown(
    stateCount = pendingMax + 1,
    incWhen = io.input.cmd.fire && io.input.cmd.last,
    decWhen = io.input.rsp.fire && io.input.rsp.last
  )
  val pendingWrite = RegNextWhen(io.input.cmd.isWrite, io.input.cmd.fire)

  val hazard = (io.input.cmd.isWrite =/= pendingWrite && pendingCounter =/= 0) || pendingCounter === pendingMax
  val (cmdFork, dataFork) = StreamFork2(io.input.cmd.haltWhen(hazard))
  val cmdStage  = cmdFork.throwWhen(!io.input.cmd.first)
  val dataStage = dataFork.throwWhen(!dataFork.isWrite)

  case class Info() extends Bundle {
    val source = cloneOf(io.input.cmd.source)
    val context = cloneOf(io.input.cmd.context)
  }

  val cmdInfo = Stream(Info())
  cmdInfo.valid := cmdStage.fire
  cmdInfo.source := cmdStage.source
  cmdInfo.context := cmdStage.context

  val rspInfo = cmdInfo.queueLowLatency(size = 1 << log2Up(pendingMax), latency = 1)

  io.output.arw.arbitrationFrom(cmdStage)
  io.output.arw.write  := io.input.cmd.isWrite
  io.output.arw.addr   := io.input.cmd.address
  io.output.arw.len    := io.input.cmd.transferBeatCountMinusOne.resized
  io.output.arw.size   := log2Up(bmbConfig.byteCount)
  io.output.arw.prot := "010"
  io.output.arw.cache := "1111"

  io.output.w.arbitrationFrom(dataStage)
  io.output.w.data := dataStage.data
  io.output.w.strb := dataStage.mask
  io.output.w.last := dataStage.last

  io.input.rsp.valid := io.output.b.valid | io.output.r.valid
  io.input.rsp.last := (pendingWrite ? True | io.output.r.last)
  io.input.rsp.data := io.output.r.data
  io.input.rsp.source := rspInfo.source
  io.input.rsp.context := rspInfo.context
  when((pendingWrite ? io.output.b.isOKAY() | io.output.r.isOKAY())){
    io.input.rsp.setSuccess()
  } otherwise {
    io.input.rsp.setError()
  }
  io.output.b.ready := io.input.rsp.ready
  io.output.r.ready := io.input.rsp.ready
  rspInfo.ready := io.input.rsp.fire && io.input.rsp.last

}

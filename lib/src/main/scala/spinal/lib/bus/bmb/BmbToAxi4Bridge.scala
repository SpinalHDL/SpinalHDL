package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

case class BmbToAxi4SharedBridge(bmbConfig : BmbParameter, pendingMax : Int = 7) extends Component{
  val axiConfig = Axi4Config(
    addressWidth = bmbConfig.access.addressWidth,
    dataWidth    = bmbConfig.access.dataWidth,
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

  val rspInfo = cmdInfo.queue(size = 1 << log2Up(pendingMax)).halfPipe()

  io.output.arw.arbitrationFrom(cmdStage)
  io.output.arw.write  := io.input.cmd.isWrite
  io.output.arw.addr   := io.input.cmd.address
  io.output.arw.len    := io.input.cmd.transferBeatCountMinusOne.resized
  io.output.arw.size   := log2Up(bmbConfig.access.byteCount)
  io.output.arw.prot := "010"
  io.output.arw.cache := "1111"

  io.output.w.arbitrationFrom(dataStage)
  io.output.w.data := dataStage.data
  io.output.w.strb := dataStage.mask
  io.output.w.last := dataStage.last

  io.input.rsp.valid := (io.output.b.valid | io.output.r.valid) && rspInfo.valid
  io.input.rsp.last := (pendingWrite ? True | io.output.r.last)
  io.input.rsp.data := io.output.r.data
  io.input.rsp.source := rspInfo.source
  io.input.rsp.context := rspInfo.context
  when((pendingWrite ? io.output.b.isOKAY() | io.output.r.isOKAY())){
    io.input.rsp.setSuccess()
  } otherwise {
    io.input.rsp.setError()
  }
  io.output.b.ready := io.input.rsp.ready && rspInfo.valid
  io.output.r.ready := io.input.rsp.ready && rspInfo.valid
  rspInfo.ready := io.input.rsp.fire && io.input.rsp.last
}



case class BmbToAxi4ReadOnlyBridge(p : BmbParameter) extends Component{
  val axiConfig = Axi4Config(
    addressWidth = p.access.addressWidth,
    dataWidth    = p.access.dataWidth,
    idWidth      = p.access.sourceWidth,
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
    val input = slave(Bmb(p))
    val output = master(Axi4ReadOnly(axiConfig))
  }

  val contextRemover = BmbContextRemover(p, pendingMax = 7)
  contextRemover.io.input << io.input

  io.output.ar.arbitrationFrom(contextRemover.io.output.cmd)
  io.output.ar.addr   := contextRemover.io.output.cmd.address
  io.output.ar.len    := contextRemover.io.output.cmd.transferBeatCountMinusOne.resized
  io.output.ar.size   := log2Up(p.access.byteCount)
  io.output.ar.prot   := "010"
  io.output.ar.cache  := "1111"

  contextRemover.io.output.rsp.arbitrationFrom(io.output.r)
  contextRemover.io.output.rsp.last    := io.output.r.last
  contextRemover.io.output.rsp.data    := io.output.r.data
  contextRemover.io.output.rsp.source  := io.output.r.id
  when(io.output.r.isOKAY()){
    contextRemover.io.output.rsp.setSuccess()
  } otherwise {
    contextRemover.io.output.rsp.setError()
  }
}



case class BmbToAxi4WriteOnlyBridge(p : BmbParameter) extends Component{
  val axiConfig = Axi4Config(
    addressWidth = p.access.addressWidth,
    dataWidth    = p.access.dataWidth,
    idWidth      = p.access.sourceWidth,
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
    val input = slave(Bmb(p))
    val output = master(Axi4WriteOnly(axiConfig))
  }

  val contextRemover = BmbContextRemover(p, pendingMax = 7)
  contextRemover.io.input << io.input

  val (cmdFork, dataFork) = StreamFork2(contextRemover.io.output.cmd)
  val cmdStage  = cmdFork.throwWhen(!contextRemover.io.output.cmd.first)

  io.output.aw.arbitrationFrom(cmdStage)
  io.output.aw.addr   := cmdStage.address
  io.output.aw.len    := cmdStage.transferBeatCountMinusOne.resized
  io.output.aw.size   := log2Up(p.access.byteCount)
  io.output.aw.prot   := "010"
  io.output.aw.cache  := "1111"

  io.output.w.arbitrationFrom(dataFork)
  io.output.w.data := dataFork.data
  io.output.w.strb := dataFork.mask
  io.output.w.last := dataFork.last

  contextRemover.io.output.rsp.arbitrationFrom(io.output.b)
  contextRemover.io.output.rsp.last    := True
  contextRemover.io.output.rsp.source  := io.output.b.id
  when(io.output.b.isOKAY()){
    contextRemover.io.output.rsp.setSuccess()
  } otherwise {
    contextRemover.io.output.rsp.setError()
  }
}

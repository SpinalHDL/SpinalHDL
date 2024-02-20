package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

object BmbToAxi4SharedBridge{
  def getAxi4Config(bmbConfig : BmbParameter) = Axi4Config(
    addressWidth = bmbConfig.access.addressWidth,
    dataWidth = bmbConfig.access.dataWidth,
    idWidth = 0,
    useId = true,
    useSize = true,
    useLen = true,
    useLast = true,
    useResp = true,
    useStrb = true,
    useProt = true,
    useCache = true,
    useQos = false,
    useRegion = false,
    useBurst = false,
    useLock = false
  )
}

case class BmbToAxi4SharedBridge(bmbConfig : BmbParameter, pendingMax : Int = 31, halfRateAw : Boolean = true) extends Component{
  val axiConfig = BmbToAxi4SharedBridge.getAxi4Config(bmbConfig)

  val io = new Bundle {
    val input = slave(Bmb(bmbConfig))
    val output = master(Axi4Shared(axiConfig))
  }


  val pendingWrite = Bool().assignDontCare()
  val pendingCounter = UInt(log2Up(pendingMax + 1) bits).assignDontCare()

  val states = for(sourceId <- bmbConfig.access.sources.keys) yield new Area{
    val counter = CounterUpDown(
      stateCount = pendingMax + 1,
      incWhen = io.input.cmd.source === sourceId && io.input.cmd.fire && io.input.cmd.last,
      decWhen = io.input.rsp.source === sourceId && io.input.rsp.fire && io.input.rsp.last
    )
    val write = RegNextWhen(io.input.cmd.isWrite, io.input.cmd.source === sourceId && io.input.cmd.fire)

    when(io.input.cmd.source === sourceId){
      pendingWrite := write
      pendingCounter := counter
    }
  }

  val hazard = (io.input.cmd.isWrite =/= pendingWrite && pendingCounter =/= 0) || pendingCounter === pendingMax
  val (cmdFork, dataFork) = StreamFork2(io.input.cmd.haltWhen(hazard))
  val cmdStage  = cmdFork.throwWhen(!io.input.cmd.first)
  val dataStage = dataFork.throwWhen(!dataFork.isWrite)

  case class Info() extends Bundle {
    val source = cloneOf(io.input.cmd.source)
    val context = cloneOf(io.input.cmd.context)
  }

  val writeCmdInfo, readCmdInfo = Stream(Info())
  writeCmdInfo.valid := cmdStage.fire && cmdStage.isWrite
  writeCmdInfo.source := cmdStage.source
  writeCmdInfo.context := cmdStage.context
  readCmdInfo.valid  := cmdStage.fire && cmdStage.isRead
  readCmdInfo.source := cmdStage.source
  readCmdInfo.context := cmdStage.context

  val writeRspInfo = writeCmdInfo.queue(size = 1 << log2Up(pendingMax)).pipelined(m2s = !halfRateAw, s2m = !halfRateAw, halfRate = halfRateAw)
  val readRspInfo = readCmdInfo.queue(size = 1 << log2Up(pendingMax)).halfPipe()

  io.output.arw.arbitrationFrom(cmdStage.haltWhen(!writeCmdInfo.ready || !readCmdInfo.ready))
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

  val rspSelLock = RegInit(False) setWhen(io.output.r.valid || io.output.b.valid) clearWhen((io.output.r.fire && io.output.r.last) || io.output.b.fire)
  val rspSelReadLast = RegNextWhen(io.output.r.valid, !rspSelLock)
  val rspSelRead = rspSelLock ? rspSelReadLast | io.output.r.valid

  io.output.b.ready := io.input.rsp.ready && !rspSelRead && writeRspInfo.valid
  io.output.r.ready := io.input.rsp.ready && rspSelRead && readRspInfo.valid
  writeRspInfo.ready := io.input.rsp.lastFire && !rspSelRead
  readRspInfo.ready  := io.input.rsp.lastFire && rspSelRead

  io.input.rsp.data := io.output.r.data
  when(rspSelRead){
    io.input.rsp.valid := io.output.r.valid && readRspInfo.valid
    io.input.rsp.last := io.output.r.last
    io.input.rsp.source :=  readRspInfo.source
    io.input.rsp.context := readRspInfo.context
  } otherwise {
    io.input.rsp.valid := io.output.b.valid && writeRspInfo.valid
    io.input.rsp.last := True
    io.input.rsp.source := writeRspInfo.source
    io.input.rsp.context := writeRspInfo.context
  }
  when((rspSelRead ? io.output.r.isOKAY() | io.output.b.isOKAY())){
    io.input.rsp.setSuccess()
  } otherwise {
    io.input.rsp.setError()
  }

}

//Assume that the AXI read and writes keep order
case class BmbToAxi4SharedBridgeAssumeInOrder(bmbConfig : BmbParameter, pendingMax : Int = 31, halfRateAw : Boolean = true) extends Component{
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


  val (cmdFork, dataFork, reorderFork) = StreamFork3(io.input.cmd)
  val cmdStage  = cmdFork.throwWhen(!io.input.cmd.first)
  val dataStage = dataFork.throwWhen(!dataFork.isWrite)

  case class Info() extends Bundle {
    val opcode = cloneOf(io.input.cmd.opcode)
    val source = cloneOf(io.input.cmd.source)
    val context = cloneOf(io.input.cmd.context)
  }

  val cmdInfo = Stream(Info())
  cmdInfo.arbitrationFrom(reorderFork.throwWhen(!reorderFork.first))
  cmdInfo.source := reorderFork.source
  cmdInfo.context := reorderFork.context
  cmdInfo.opcode := reorderFork.opcode

  val rspInfo = cmdInfo.queue(size = 1 << log2Up(pendingMax)).pipelined(m2s = true, s2m = true)
  rspInfo.ready := io.input.rsp.lastFire

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

  io.input.rsp.valid := rspInfo.valid
  io.input.rsp.data := io.output.r.data
  io.input.rsp.source :=  rspInfo.source
  io.input.rsp.context := rspInfo.context

  io.output.b.ready := False
  io.output.r.ready := False

  when(rspInfo.opcode === Bmb.Cmd.Opcode.WRITE){
    io.output.b.ready := io.input.rsp.ready && rspInfo.valid
    io.input.rsp.valid.clearWhen(!io.output.b.valid)
    io.input.rsp.last := True
    when(io.output.b.isOKAY()){
      io.input.rsp.setSuccess()
    } otherwise {
      io.input.rsp.setError()
    }
  } otherwise {
    io.output.r.ready := io.input.rsp.ready && rspInfo.valid
    io.input.rsp.valid.clearWhen(!io.output.r.valid)
    io.input.rsp.last := io.output.r.last
    when(io.output.r.isOKAY()){
      io.input.rsp.setSuccess()
    } otherwise {
      io.input.rsp.setError()
    }
  }
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

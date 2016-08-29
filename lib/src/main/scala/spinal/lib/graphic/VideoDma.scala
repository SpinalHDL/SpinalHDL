package spinal.lib.graphic

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4ReadOnly}


case class VideoDmaMem[T <: Data](g: VideoDmaGeneric[T]) extends Bundle with IMasterSlave{
  val cmd = Stream(UInt(g.addressWidth bit))
  val rsp = Flow Fragment(Bits(g.dataWidth bit))
  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

  
  def getAxi4ReadOnlyConfig = Axi4Config(
    addressWidth = g.addressWidth + log2Up(g.dataWidth/8) + log2Up(g.beatPerAccess),
    dataWidth = g.dataWidth,
    useId = false,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useQos = false,
    useLen = false,
    useResp = false,
    useSize = false
  )

  def toAxi4ReadOnly : Axi4ReadOnly = {
    val ret = Axi4ReadOnly(getAxi4ReadOnlyConfig)
    ret.readCmd.valid := this.cmd.valid
    ret.readCmd.addr  := this.cmd.payload << log2Up(g.dataWidth/8) + log2Up(g.beatPerAccess)
    ret.readCmd.prot  := "010"
    ret.readCmd.cache := "1111"
    ret.readCmd.len   := g.beatPerAccess-1
    ret.readCmd.size  := log2Up(g.dataWidth/8)
    this.cmd.ready := ret.readCmd.ready

    this.rsp.last := ret.readRsp.last
    this.rsp.fragment := ret.readRsp.data
    ret.readRsp.ready := True

    ret
  }
}


case class VideoDmaGeneric[T <: Data](addressWidth : Int,
                                      dataWidth : Int,
                                      beatPerAccess : Int,
                                      sizeWidth : Int,
                                      frameFragmentType : T,
                                      pendingRequetMax : Int,
                                      fifoSize : Int,
                                      frameClock : ClockDomain = null)

case class VideoDma[T <: Data](g : VideoDmaGeneric[T]) extends Component{
  import g._
  require(dataWidth >= widthOf(g.frameFragmentType))
  val io = new Bundle{
    val start = in Bool

    val base  = in UInt(addressWidth bits) //base and size are in burst count, not in word, nor in byte
    val size  = in UInt(sizeWidth bits)

    val mem   = master(VideoDmaMem(g))
    val frame = master(Stream(Fragment(frameFragmentType)))
  }

  val pendingMemCmd = CounterUpDown(
    stateCount=pendingRequetMax + 1,
    incWhen = io.mem.cmd.fire,
    decWhen = io.mem.rsp.fire && io.mem.rsp.last
  )

  val pendingMemRsp = CounterMultiRequest(
    width=log2Up(pendingRequetMax*beatPerAccess + 1),
    io.mem.cmd.fire -> (_ + beatPerAccess),
    io.mem.rsp.fire -> (_ - 1)
  )

  val isActive = RegInit(False)

  val memCmdCounter = Reg(UInt(sizeWidth bits))
  val memCmdDone = memCmdCounter =/= io.size

  io.mem.cmd.valid := False
  io.mem.cmd.payload := io.base + memCmdCounter

  when(!isActive) {
    when(io.start) {
      memCmdCounter := 0
      isActive := True
    }
  } otherwise {
    when(!memCmdDone){
      io.mem.cmd.valid := !toManyPendingCmd && !toManyPendingRsp
    }.elsewhen(pendingMemRsp === 0) {
      isActive := False
    }
  }

  when(io.mem.cmd.fire) {
    memCmdCounter := memCmdCounter - 1
  }

  val memRsp = cloneOf(io.mem.rsp)
  memRsp.valid := io.mem.rsp.valid
  memRsp.last := memCmdDone && pendingMemRsp === 1
  memRsp.fragment := io.mem.rsp.fragment

  val toManyPendingCmd = pendingMemCmd > pendingRequetMax-1
  val toManyPendingRsp = Bool


  val fifoPop = Stream(Fragment(Bits(dataWidth bits)))
  val rspArea = if(this.clockDomain == frameClock) new Area{
    val pendingMemToFifo = CounterMultiRequest(
      width=log2Up(fifoSize + 1),
      io.mem.cmd.fire -> (_ + beatPerAccess),
      io.frame.fire -> (_ - 1)
    )
    toManyPendingRsp := pendingMemToFifo > fifoSize-beatPerAccess
    fifoPop << memRsp.toStream.queue(fifoSize)
  } else new Area{
    val fifo = new StreamFifoCC(Fragment(Bits(dataWidth bit)),fifoSize,pushClock = ClockDomain.current,popClock = frameClock)
    fifo.io.push << memRsp.toStream
    fifo.io.pop >> fifoPop

    toManyPendingRsp := RegNext(fifo.io.pushOccupancy) + pendingMemRsp > fifoSize-beatPerAccess-1    //-1 because of regnext fifo occupancy
  }

  val fifoPopArea = new ClockingArea(frameClock){
    StreamFragmentWidthAdapter(fifoPop,io.frame)
  }
}

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

  def toAxi4ReadOnly : Axi4ReadOnly = {
    val ret = Axi4ReadOnly(g.getAxi4ReadOnlyConfig)
    ret.readCmd.valid := this.cmd.valid
    ret.readCmd.addr  := this.cmd.payload << log2Up(g.dataWidth/8) + log2Up(g.beatPerAccess)
    ret.readCmd.prot  := "010"
    ret.readCmd.cache := "1111"
    ret.readCmd.len   := g.beatPerAccess-1
    ret.readCmd.size  := log2Up(g.dataWidth/8)
    this.cmd.ready := ret.readCmd.ready

    this.rsp.valid := ret.readRsp.valid
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
                                      frameClock : ClockDomain = null){
  def getAxi4ReadOnlyConfig = Axi4Config(
    addressWidth = addressWidth + log2Up(dataWidth/8) + log2Up(beatPerAccess),
    dataWidth = dataWidth,
    useId = false,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useQos = false,
    useResp = false
  )
}

case class VideoDma[T <: Data](g : VideoDmaGeneric[T]) extends Component{
  import g._
  require(dataWidth >= widthOf(g.frameFragmentType))
  val io = new Bundle{
    val start = in Bool
    val busy  = out Bool

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

  val toManyPendingCmd = pendingMemCmd > pendingRequetMax-1
  val toManyPendingRsp = Bool

  val isActive = RegInit(False)
  val cmdActive = RegInit(False)
  io.busy := isActive

  val memCmdCounter = Reg(UInt(sizeWidth bits))
  val memCmdLast = memCmdCounter === io.size

  io.mem.cmd.valid := False
  io.mem.cmd.payload := io.base + memCmdCounter

  when(!isActive) {
    when(io.start) {
      memCmdCounter := 0
      isActive := True
      cmdActive := True
    }
  } otherwise {
    when(cmdActive){
      when(!toManyPendingCmd && !toManyPendingRsp){
        io.mem.cmd.valid := True
        when(memCmdLast && io.mem.cmd.ready){
          cmdActive := False
        }
      }
    }elsewhen(pendingMemRsp === 0) {
      isActive := False
    }
  }

  when(io.mem.cmd.fire) {
    memCmdCounter := memCmdCounter + 1
  }

  val memRsp = cloneOf(io.mem.rsp)
  memRsp.valid := io.mem.rsp.valid
  memRsp.last := !cmdActive && pendingMemRsp === 1
  memRsp.fragment := io.mem.rsp.fragment




  val fifoPop = Stream(Fragment(Bits(dataWidth bits)))
  val rspArea = if(this.clockDomain == frameClock) new Area{
    fifoPop << memRsp.toStream.queue(fifoSize)
    val pendingMemToFifo = CounterMultiRequest(
      width=log2Up(fifoSize + 1),
      io.mem.cmd.fire -> (_ + beatPerAccess),
      fifoPop.fire -> (_ - 1)
    )
    toManyPendingRsp := pendingMemToFifo > fifoSize-beatPerAccess
  } else new Area{
    require(isPow2(fifoSize))
    val fifo = new StreamFifoCC(Fragment(Bits(dataWidth bit)),fifoSize,pushClock = ClockDomain.current,popClock = frameClock)
    fifo.io.push << memRsp.toStream
    fifo.io.pop >> fifoPop

    val grayWidth = log2Up(fifoSize/beatPerAccess)+1
    require(grayWidth >= 3)

    val frameClockArea = new ClockingArea(frameClock){
      val popBeatCounter = Counter(beatPerAccess)
      when(fifo.io.pop.fire){
        popBeatCounter.increment()
      }

      val popCmdGray = GrayCounter(grayWidth, popBeatCounter.willOverflow)
    }

    val popCmdGray  = BufferCC(frameClockArea.popCmdGray)
    val pushCmdGray = GrayCounter(grayWidth,io.mem.cmd.fire)

    toManyPendingRsp :=  pushCmdGray(grayWidth - 1 downto grayWidth - 2) === ~popCmdGray(grayWidth - 1 downto grayWidth - 2) && pushCmdGray(grayWidth - 3 downto 0) === popCmdGray(grayWidth - 3 downto 0)
  }

  val fifoPopArea = new ClockingArea(frameClock){
    StreamFragmentWidthAdapter(fifoPop,io.frame)
  }
}

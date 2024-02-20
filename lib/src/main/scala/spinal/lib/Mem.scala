package spinal.lib

import spinal.core._


case class ReadRetLinked[T <: Data, T2 <: Data](readType: HardType[T], linkedType: HardType[T2]) extends Bundle {
  val value = readType()
  val linked = linkedType()
}

class MemPimped[T <: Data](mem: Mem[T]) {

  def formalCount(word : T): UInt ={
    CountOne((0 until mem.wordCount).map(mem(_) === word))
  }
  def formalCount(cond : T => Bool) : UInt ={
    CountOne((0 until mem.wordCount).map(i => cond(mem(i))))
  }

  //def streamReadSync[T2 <: Data](event : Event,address: UInt, linkedData: T2) : (Event,T,T2) = {

  def streamReadSync[T2 <: Data](cmd: Stream[UInt], linkedData: T2, crossClock:Boolean = false) : Stream[ReadRetLinked[T,T2]] = {
    val ret = Stream(new ReadRetLinked(mem.wordType, linkedData))

    val retValid = RegInit(False)
    val retData = mem.readSync(cmd.payload, cmd.fire, clockCrossing = crossClock)
    val retLinked = RegNextWhen(linkedData, cmd.ready)

    when(ret.ready) {
      retValid := Bool(false)
    }
    when(cmd.ready) {
      retValid := cmd.valid
    }

    cmd.ready := ret.isFree

    ret.valid := retValid
    ret.value := retData
    ret.linked := retLinked
    ret
  }

  def streamReadSync(cmd: Stream[UInt]): Stream[T] = streamReadSync(cmd, crossClock = false)
  def streamReadSync(cmd: Stream[UInt], crossClock : Boolean): Stream[T] = {
    val ret = Stream(mem.wordType)

    val retValid = RegInit(False)
    val retData = mem.readSync(cmd.payload, cmd.fire, clockCrossing = crossClock)

    when(ret.ready) {
      retValid := Bool(false)
    }
    when(cmd.ready) {
      retValid := cmd.valid
    }

    cmd.ready := ret.isFree

    ret.valid := retValid
    ret.payload := retData
    ret
  }

  def streamReadSyncMultiPort(cmd: Seq[Stream[UInt]], crossClock:Boolean = false) : Vec[Stream[T]] = {
    val ret = Vec(Stream(mem.wordType), cmd.length)


    val selectOh = OHMasking.first(cmd.map(_.valid))
    val selectCmd = Stream(mem.addressType)
    selectCmd.valid := cmd.map(_.valid).orR
    selectCmd.payload := MuxOH(selectOh.asBits, cmd.map(_.payload))
    val retOh = RegNextWhen(selectOh, selectCmd.ready)
    val retRsp = mem.streamReadSync(selectCmd)
    retRsp.ready := (ret, retOh).zipped.map(_.ready && _).orR
    for(i <- 0 until cmd.length){
      cmd(i).ready := selectCmd.ready && selectOh(i)
      ret(i).valid := retRsp.valid && retOh(i)
      ret(i).payload := retRsp.payload
    }
    ret
  }

  def flowReadSync(cmd : Flow[UInt]) : Flow[T] = {
    val ret = Flow(mem.wordType)
    ret.valid := RegNext(cmd.valid)
    ret.payload := mem.readSync(cmd.payload)
    ret
  }

  def flowReadSync[T2 <: Data](cmd: Flow[UInt], linkedData: T2) : Flow[ReadRetLinked[T,T2]] = {
    val ret = Flow(ReadRetLinked(mem.wordType, linkedData))
    ret.valid := RegNext(cmd.valid)
    ret.linked := RegNext(linkedData)
    ret.value := mem.readSync(cmd.payload)
    ret
  }


  /**
    * Create a write port of memory.
    */
  def writePort() : Flow[MemWriteCmd[T]] = {
    val ret = Flow(MemWriteCmd(mem))
    when(ret.valid){
      mem.write(ret.address,ret.data)
    }
    ret
  }

  /**
    * Create a write port of memory with masking.
    */
  def writePortWithMask(maskWidth : Int) : Flow[MemWriteCmdWithMask[T]] = {
    val ret = Flow(MemWriteCmdWithMask(mem, maskWidth))
    mem.write(ret.address,ret.data, ret.valid, ret.mask)
    ret
  }

  def readSyncPort : MemReadPort[T] = readSyncPort()
  def readSyncPort(readUnderWrite: ReadUnderWritePolicy = dontCare, clockCrossing: Boolean = false) : MemReadPort[T] = {
    val ret : MemReadPort[T] = MemReadPort(mem.wordType(),mem.addressWidth)
    ret.rsp := mem.readSync(ret.cmd.payload,ret.cmd.valid, readUnderWrite, clockCrossing)
    ret
  }

  def readAsyncPort() : MemReadPortAsync[T] = {
    val ret : MemReadPortAsync[T] = MemReadPortAsync(mem.wordType(),mem.addressWidth)
    ret.data := mem.readAsync(ret.address)
    ret
  }

  def readAsyncPortBySyncReadRevertedClk : MemReadPortAsync[T] = {
    val ret : MemReadPortAsync[T] = MemReadPortAsync(mem.wordType(),mem.addressWidth)
    ret.data := ClockDomain.current.withRevertedClockEdge()(mem.readSync(ret.address))
    ret
  }

  def readWriteSyncPort(
    maskWidth     : Int = -1,
    readUnderWrite: ReadUnderWritePolicy = dontCare,
    clockCrossing : Boolean = false,
    duringWrite   : DuringWritePolicy = dontCare) : MemReadWritePort[T] = {
    val ret : MemReadWritePort[T] = MemReadWritePort(
      mem.wordType(),
      mem.addressWidth,
      maskWidth = maskWidth
    )
    ret.rdata := mem.readWriteSync(
      ret.address,
      ret.wdata,
      ret.enable,
      ret.write,
      ret.mask,
      readUnderWrite= readUnderWrite,
      clockCrossing = clockCrossing ,
      duringWrite   = duringWrite
    )
    ret
  }
}


object MemWriteCmd{
  def apply[T <: Data](mem : Mem[T]) : MemWriteCmd[T] = {
    MemWriteCmd(mem.wordType, mem.addressWidth, -1)
  }
  def apply[T <: Data](mem : Mem[T], maskWidth : Int) : MemWriteCmd[T] = {
    MemWriteCmd(mem.wordType, mem.addressWidth, maskWidth)
  }
}

case class MemWriteCmd[T <: Data](dataType : HardType[T], addressWidth : Int, maskWidth : Int = -1) extends Bundle{
  def useMask = maskWidth >= 0
  val address = UInt(addressWidth bits)
  val data    = dataType()
  val mask    = ifGen(useMask)(Bits(maskWidth bits))
}

case class MemWriteCmdWithMask[T <: Data](mem : Mem[T], maskWidth : Int) extends Bundle {
  val address = mem.addressType()
  val data    = mem.wordType()
  val mask    = Bits(maskWidth bits)
}

case class MemReadPort[T <: Data](dataType : T,addressWidth : Int) extends Bundle with IMasterSlave{
  val cmd = Flow(UInt(addressWidth bit))
  val rsp = cloneOf(dataType)

  override def asMaster(): Unit = {
    master(cmd)
    in(rsp)
  }

  def writeFirst(writeLast : Flow[MemWriteCmd[T]]): Unit = new Composite(this, "bypassWriteFirst", true){
    val cmdLast = RegNextWhen(cmd.payload, cmd.valid)
    val hit     = cmdLast === writeLast.address && writeLast.valid
    when(hit){
      rsp := writeLast.data
    }
  }

  def writeFirstAndUpdate(write : Flow[MemWriteCmd[T]]): Unit = new Composite(this, "bypass", true){
    val bypassValid = RegInit(False)
    val bypassData = Reg(dataType)
    bypassValid clearWhen(cmd.valid)
    when(bypassValid){
      rsp := bypassData
    }

    val readAddressBuffer = RegNextWhen(cmd.payload, cmd.valid)
    val addressToCheck = cmd.valid ? cmd.payload | readAddressBuffer
    val hit = addressToCheck === write.address && write.valid
    when(hit){
      bypassValid := True
      bypassData := write.data
    }
  }

  def gotReadDuringWrite(write: Flow[MemWriteCmd[T]]): Bool = new Composite(this, "gotReadDurringWrite", true) {
    val hit = cmd.valid && write.valid && cmd.payload === write.address
    val buffer = RegNextWhen(hit, cmd.valid) init(False)
  }.buffer
}

case class MemReadStreamFlowPort[T <: Data](dataType : T,addressWidth : Int) extends Bundle with IMasterSlave{
  val cmd = Stream(UInt(addressWidth bit))
  val rsp = Flow(dataType)

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}


case class MemReadPortAsync[T <: Data](dataType : T,addressWidth : Int) extends Bundle with IMasterSlave{
  val address = UInt(addressWidth bit)
  val data = cloneOf(dataType)

  override def asMaster(): Unit = {
    out(address)
    in(data)
  }
}

case class MemReadWritePort[T <: Data](
  dataType : T,
  addressWidth : Int,
  maskWidth     : Int = -1) extends Bundle with IMasterSlave{
  def useMask = maskWidth >= 0
  val address = UInt(addressWidth bit)
  val rdata   = cloneOf(dataType)
  val wdata   = cloneOf(dataType)
  val enable  = Bool()
  val write   = Bool()
  val mask    = ifGen(useMask)(Bits(maskWidth bits))
  override def asMaster(): Unit = {
    out(address,wdata,enable,write)
    if(useMask) out(mask)
    in(rdata)
  }
}

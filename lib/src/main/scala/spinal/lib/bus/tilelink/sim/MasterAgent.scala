package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink._
import spinal.lib.sim.{StreamDriver, StreamDriverOoo, StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable

class OrderingArgs(val offset : Int, val bytes : Int){
  override def toString = f"offset=$offset%x bytes=$bytes"
}

class Block(val source : Int,
            val address : Long,
            var cap : Int,
            var bytes : Int,
            var dirty : Boolean = false,
            var data : Array[Byte] = null,
            var retains : Int = 0){
  def retain() = retains += 1
  def release() = {
    assert(retains > 0)
    retains -= 1
  }
  var probe = Option.empty[Probe]

  override def toString = f"src=$source%02x addr=$address%x cap=$cap"

  def makeDataDirty(): Unit ={
    dirty = true
    for (i <- 0 until data.size if simRandom.nextBoolean()) data(i) = simRandom.nextInt().toByte
  }

  def setCap(cap : Int): Unit ={
//    if(address == 0x24c0) println(f"$source $address%04x setcap ${this.cap} $cap $simTime")
    this.cap = cap
  }
}
case class Probe(source : Int, param : Int, address : Long, size : Int, perm : Boolean){

}

class MasterAgent (val bus : Bus, val cd : ClockDomain, val blockSize : Int = 64)(implicit idAllocator: IdAllocator) extends MonitorSubscriber{
  var debug = false

  val driver = new MasterDriver(bus, cd)
  val monitor = new Monitor(bus, cd).add(this)
  val callbackOnAtoD, callbackOnCtoD = Array.fill[TransactionD => Unit](1 << bus.p.sourceWidth)(null)


  val releaseIds = Array.fill(1 << bus.p.sourceWidth) (SimMutex())

  def waitAtoD(source : Int) : TransactionD = {
    var value : TransactionD = null
    val lock = SimMutex().lock()
    callbackOnAtoD(source) = { d =>
      value = d
      lock.unlock()
    }
    lock.await()
    callbackOnAtoD(source) = null
    value
  }

  def waitCtoD(source: Int): TransactionD = {
    var value: TransactionD = null
    val lock = SimMutex().lock()
    callbackOnCtoD(source) = { d =>
      value = d
      lock.unlock()
    }
    lock.await()
    callbackOnCtoD(source) = null
    value
  }

  override def onA(a : TransactionA) : Unit = {

  }
  override def onB(b : TransactionB) : Unit = {
    val address =  b.address.toLong
    val probe = Probe(b.source, b.param, address, b.size, b.opcode == Opcode.B.PROBE_PERM)
    block.blocks.get(block.sourceToMaster(b.source) -> address) match {
      case Some(b) => {
        b.probe match {
          case Some(x) => ???
          case None =>
        }
        b.retains match {
          case 0 => block.executeProbe(probe)
          case _ =>  {
            b.probe = Some(probe)
            if(debug) println(f"Retained ${b.address}%x ${simTime()}")
          }
        }
      }
      case None => block.executeProbe(probe)
    }
    probeBlock(b.source, b.param, address, 1 << b.size)
  }
  override def onC(c : TransactionC) : Unit = {

  }
  override def onD(d : TransactionD) : Unit = {
    import Opcode.D._
    d.opcode match {
      case RELEASE_ACK =>  callbackOnCtoD(d.source.toInt)(d)
      case ACCESS_ACK | ACCESS_ACK_DATA | GRANT | GRANT_DATA =>  callbackOnAtoD(d.source.toInt)(d)
    }
  }
  override def onE(e : TransactionE) : Unit = {

  }


  val block = new BlockManager(this)

  def probeBlock(source : Int,
                 param : Int,
                 address : Long,
                 bytes : Int): Unit ={

  }


  def onGrant(source : Int, address : Long, param : Int) : Unit = {}




  def get(source : Int, address : Long, bytes : Int) : TransactionD = {
    val debugId = idAllocator.allocate()
    val a = TransactionA()
    a.opcode  = Opcode.A.GET
    a.size    = log2Up(bytes)
    a.source  = source
    a.address = address
    a.debugId = debugId
    driver.scheduleA(a)

    val d = waitAtoD(source)
    assert(d.opcode == Opcode.D.ACCESS_ACK_DATA, s"Unexpected transaction on $bus")
    assert(d.bytes == bytes, s"Unexpected transaction on $bus")
    idAllocator.remove(debugId)
    d
  }


  def putPartialData(source : Int, address : Long, data : Seq[Byte], mask : Seq[Boolean]) : TransactionD = {
    val bytes = data.size
    val debugId = idAllocator.allocate()
    val a = TransactionA()
    a.opcode  = Opcode.A.PUT_PARTIAL_DATA
    a.size    = log2Up(bytes)
    a.source  = source
    a.address = address
    a.debugId = debugId
    a.data = data.toArray
    a.mask = mask.toArray
    driver.scheduleA(a)

    val d = waitAtoD(source)
    assert(d.opcode == Opcode.D.ACCESS_ACK, s"Unexpected transaction on $bus")
    assert(d.bytes == bytes, s"Unexpected transaction on $bus")
    idAllocator.remove(debugId)
    d
  }

  def putFullData(source : Int, address : Long, data : Seq[Byte]) : TransactionD = {
    val bytes = data.size
    val debugId = idAllocator.allocate()
    val a = TransactionA()
    a.opcode  = Opcode.A.PUT_FULL_DATA
    a.size    = log2Up(bytes)
    a.source  = source
    a.address = address
    a.debugId = debugId
    a.data = data.toArray
    a.mask = Array.fill(data.size)(true)
    driver.scheduleA(a)

    val d = waitAtoD(source)
    assert(d.opcode == Opcode.D.ACCESS_ACK, s"Unexpected transaction on $bus")
    assert(d.bytes == bytes, s"Unexpected transaction on $bus")
    idAllocator.remove(debugId)
    d
  }



  def probeAck(source : Int,
               param : Int,
               address : Long,
               bytes : Int): Unit ={
    val c = TransactionC()
    c.opcode  = Opcode.C.PROBE_ACK
    c.param   = param
    c.size    = log2Up(bytes)
    c.source  = source
    c.address = address
    driver.scheduleC(c)
  }

  def probeAckData(source : Int,
                   param : Int,
                   address : Long,
                   data : Seq[Byte]) : Unit = {
    val size = log2Up(data.length)
    val c = TransactionC()
    c.opcode  = Opcode.C.PROBE_ACK_DATA
    c.param   = param
    c.size    = size
    c.source  = source
    c.address = address
    c.data    = data.toArray
    c.corrupt = false
    driver.scheduleC(c)
  }

  def probeAck(source : Int,
               toCap : Int,
               block  :Block): Unit ={
    probeAck(source, Param.reportPruneToCap(block.cap, toCap), block.address, blockSize)
    this.block.probeCap(block, toCap)
  }

  def probeAckData(source : Int,
                   toCap : Int,
                   block  : Block) : Unit = {

    probeAckData(source, Param.reportPruneToCap(block.cap, toCap), block.address, block.data)
    this.block.probeCap(block, toCap)
  }


  def acquireBlock(source : Int,
                   param : Int,
                   address : Long,
                   bytes : Int): Block ={
    val debugId = idAllocator.allocate()
    val a = new TransactionA()
    a.opcode  = Opcode.A.ACQUIRE_BLOCK
    a.param   = param
    a.size    = log2Up(bytes)
    a.source  = source
    a.address = address
    a.debugId = debugId
    driver.scheduleA(a)

    val d = waitAtoD(source)
    var b : Block = null
    d.opcode match {
      case Opcode.D.GRANT => {
        val param = d.param
        b = block(source, address)
        assert(b.cap == Param.Cap.toB)
        b.setCap(Param.Cap.toT)
        if(debug) println(f"acquireBlock src=$source%02x addr=$address%x 1 -> 0 time=${simTime()}")
        onGrant(source, address, param)
      }
      case Opcode.D.GRANT_DATA => { //TODO on naxriscv, may sent a aquire BtoT but may have been probed out meanwhile => test
        assert(!block.contains(source, address))
        onGrant(source, address, param)
        b = new Block(source, address, d.param, bytes, false, d.data){
          override def release() = {
            super.release()
            if(retains == 0) {
              probe.foreach(block.executeProbe)
            }
            block.updateBlock(this)
          }
        }
        if(d.denied){
          b.setCap(Param.Cap.toN)
          b.data = null
        }
        if(debug && !d.denied) println(f"acquireBlock src=$source%02x addr=$address%x 2 -> ${d.param} time=${simTime()}")
        block(source -> address) = b
      }
    }


    val e = TransactionE(d.sink)
    driver.scheduleE(e)

    idAllocator.remove(debugId)
    assert(b.cap <= Param.Grow.getCap(param))
    b
  }


  def acquirePerm (source : Int,
                   param : Int,
                   address : Long,
                   bytes : Int): Block ={
    val debugId = idAllocator.allocate()
    val a = new TransactionA()
    a.opcode  = Opcode.A.ACQUIRE_PERM
    a.param   = param
    a.size    = log2Up(bytes)
    a.source  = source
    a.address = address
    a.debugId = debugId
    driver.scheduleA(a)

    val d = waitAtoD(source)
    var blk : Block = null
    assert(d.opcode == Opcode.D.GRANT)
    assert(d.param == Param.Cap.toT)
    block.get(source, address) match {
      case Some(x) => {
        blk = x
        if(debug) println(f"acquirePerm  src=$source%02x addr=$address%x ${blk.cap} -> 0 time=${simTime()}")
        blk.setCap(Param.Cap.toT)
      }
      case None => {
        if(debug && !d.denied) println(f"acquirePerm  src=$source%02x addr=$address%x 2 -> 0 time=${simTime()}")
        blk = new Block(source, address, d.param, bytes, false, null){
          override def release() = {
            super.release()
            if(retains == 0) {
              probe.foreach(block.executeProbe)
            }
            block.updateBlock(this)
          }
        }
        if(d.denied){
          blk.setCap(Param.Cap.toN)
          blk.data = null
        }
        block(source -> address) = blk
      }
    }
    onGrant(source, address, d.param)

    val e = TransactionE(d.sink)
    driver.scheduleE(e)

    idAllocator.remove(debugId)
    assert(blk.cap <= Param.Grow.getCap(param))
    blk
  }

  def releaseAuto(source : Int, toCap : Int, block : Block) : Unit = {
    if(block.dirty){
      releaseData(source, toCap, block)
    } else {
      release(source, toCap, block)
    }
  }
  def release(source : Int, toCap : Int, block : Block) : Unit = {
    block.retain()

    assert(!block.dirty)
    assert(block.cap < toCap)

    val blockCap = block.cap
    this.block.releaseCap(block, toCap)
    assert(block.cap == toCap)

    releaseIds(source).lock()

    val c = new TransactionC()
    c.opcode  = Opcode.C.RELEASE
    c.param   = Param.Prune.fromTo(blockCap, toCap)
    c.size    = log2Up(blockSize)
    c.source  = source
    c.address = block.address
    driver.scheduleC(c)

    val d = waitCtoD(source)
    assert(d.opcode == Opcode.D.RELEASE_ACK)

    releaseIds(source).unlock()
    block.release()
    this.block.updateBlock(block)
  }

  def releaseData(source : Int, toCap : Int, block : Block) : Unit = {
    block.retain()

    assert(block.dirty)
    assert(block.cap < toCap)

    block.dirty = false

    val blockCap = block.cap
    this.block.releaseCap(block, toCap)
    assert(block.cap == toCap)

    releaseIds(source).lock()

    val c = new TransactionC()
    c.opcode  = Opcode.C.RELEASE_DATA
    c.param   = Param.Prune.fromTo(blockCap, toCap)
    c.size    = log2Up(blockSize)
    c.source  = source
    c.address = block.address
    c.data    = block.data
    driver.scheduleC(c)

    val d = waitCtoD(source)
    assert(d.opcode == Opcode.D.RELEASE_ACK)

    releaseIds(source).unlock()
    block.release()
    this.block.updateBlock(block)
  }
}

import spinal.core.sim._
import spinal.lib.sim._

class BlockManager(ma : MasterAgent, var allowReleaseOnProbe : Boolean = false){
  import ma._
  val sourceToMaster = (0 until  1 << bus.p.sourceWidth).map(source => bus.p.node.m.getMasterFromSource(source))
  val blocks = mutable.LinkedHashMap[(M2sAgent, Long), Block]()
  val blockHistorySize = 16
  val blockRandomHistory = mutable.LinkedHashMap[M2sAgent, Array[Block]]()
  ma.bus.p.node.m.masters.foreach(blockRandomHistory(_) = Array.fill[Block](blockHistorySize)(null))

  def getRandomBlock(m : M2sAgent) = blockRandomHistory(m)(simRandom.nextInt(blockHistorySize))
  def apply(source : Int, address : Long) = blocks(sourceToMaster(source) -> address)
  def get(source : Int, address : Long) : Option[Block] = blocks.get(sourceToMaster(source) -> address)
  def contains(source : Int, address : Long) = blocks.contains(sourceToMaster(source) -> address)
  def update(key : (Int, Long), block : Block) = {
    val key2 = (sourceToMaster(key._1) -> key._2)
    assert(!blocks.contains(key2))
    blocks(key2) = block
    blockRandomHistory(key2._1)(simRandom.nextInt(blockHistorySize)) = block
  }
  def removeBlock(source : Int, address : Long) = {
    val m = sourceToMaster(source)
    val key = m -> address
//    val block = blocks(key)
//    blocksPerAgent(m) -= block
    blocks.remove(key)
  }
  def probeCap(block : Block, cap : Int) = {
    if(debug) if(cap != block.cap) println(f"probeCap     src=${block.source}%02x addr=${block.address}%x ${block.cap} -> $cap time=${simTime()}")
    block.setCap(cap)
    updateBlock(block)
  }

  def releaseCap(block : Block, cap : Int) = {
    if(debug) if(cap != block.cap) println(f"releaseCap   src=${block.source}%02x addr=${block.address}%x ${block.cap} -> $cap time=${simTime()}")
    block.setCap(cap)
    updateBlock(block)
  }

  def executeProbe(probe : Probe): Unit ={
    blocks.get(sourceToMaster(probe.source) -> probe.address) match {
      case Some(b : Block) => {
        b.probe = None
        b.retains match {
          case 0 => {
            def doProbeAck(): Unit = {
              b.cap < probe.param match {
                case false => probeAck(
                  source = probe.source,
                  toCap = b.cap,
                  block = b
                )
                case true => {
                  (b.dirty && !probe.perm) match {
                    case false => probeAck(
                      source = probe.source,
                      toCap = probe.param,
                      block = b
                    )
                    case true => {
                      b.dirty = false
                      probeAckData(
                        toCap = probe.param,
                        source = probe.source,
                        block = b
                      )
                    }
                  }
                }
              }
            }

            //Sporadic release
            if(allowReleaseOnProbe && b.cap != Param.Cap.toN && simRandom.nextFloat() < 0.2f) fork{
              releaseAuto(
                source = sourceToMaster(probe.source).mapping.randomPick().id.randomPick().toInt,
                toCap =  (b.cap+1 to Param.Cap.toN).randomPick(),
                block = b
              )
              doProbeAck()
            } else {
              doProbeAck()
            }

          }
          case _ => ???
        }
      }
      case None => probeAck(
        param   = Param.Report.NtoN,
        source  = probe.source,
        address = probe.address,
        bytes   = blockSize
      )
    }
    probeBlock(probe.source, probe.param, probe.address, blockSize)
  }

  def updateBlock(block : Block): Unit ={
    if(block.retains == 0) {
      if(block.cap == Param.Cap.toN){
        removeBlock(block.source, block.address)
      }
    }
  }
}
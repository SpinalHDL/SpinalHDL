package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink._
import spinal.lib.sim.{StreamDriver, StreamDriverOoo, StreamMonitor, StreamReadyRandomizer}

import scala.collection.{breakOut, mutable}
import scala.util.Random

class OrderingArgs(val address : BigInt, val bytes : Int){
  override def toString = f"addr=$address%x bytes=$bytes"
}

class Block(val source : Int,
            val address : Long,
            var cap : Int,
            var dirty : Boolean = false,
            var data : Array[Byte] = null,
            var retains : Int = 0){
  def retain() = retains += 1
  def release() = {
    assert(retains > 0)
    retains -= 1
  }
  var probe = Option.empty[Probe]
}
case class Probe(source : Int, param : Int, address : Long, size : Int, perm : Boolean){

}

class MasterAgent (val bus : Bus, cd : ClockDomain, val blockSize : Int = 64)(implicit idAllocator: IdAllocator) extends MonitorSubscriber{
  var debug = true

  val driver = new MasterDriver(bus, cd)
  val monitor = new Monitor(bus, cd).add(this)
  val callbackOnD = Array.fill[TransactionD => Unit](1 << bus.p.sourceWidth)(null)

  def waitD(source : Int) : TransactionD = {
    var value : TransactionD = null
    val lock = SimMutex().lock()
    callbackOnD(source) = { d =>
      value = d
      lock.unlock()
    }
    lock.await()
    callbackOnD(source) = null
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
    callbackOnD(d.source.toInt)(d)
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



//
//  def releaseData(source : Int, toCap : Int, block : Block) : Boolean = {
//    assert(block.dirty)
//    block.dirty = false
//    block.retain()
//    val size = log2Up(blockSize)
//    driver.c.burst { push =>
//      for (offset <- 0 until blockSize by bus.p.dataBytes) {
//        push { p =>
//          val buf = new Array[Byte](bus.p.dataBytes)
//          (0 until bus.p.dataBytes).foreach(i => buf(i) = block.data(offset + i))
//          p.opcode #= Opcode.C.RELEASE_DATA
//          p.param #= Param.reportPruneToCap(block.cap, toCap)
//          p.size #= size
//          p.source #= source
//          p.address #= block.address + offset
//          p.data #= buf
//          p.corrupt #= false
//        }
//      }
//    }
//    val mutex = SimMutex().lock()
//    var denied = false
//    monitor.d(source) = {d =>
//      monitor.d(source) = null
//      assert(d.opcode.toEnum == Opcode.D.RELEASE_ACK)
//      mutex.unlock()
//    }
//    mutex.await()
////    ordering.checkDone(source)
//
////    val block = this.block(source, address)
//    this.block.changeCap(block, toCap)
//
//    block.release()
//    denied
//  }
//
//
//  def release(source : Int, toCap : Int, block : Block) : Boolean = {
//    block.retain()
//    val mutex = SimMutex().lock()
//    driver.c.single { p =>
//      p.opcode #= Opcode.C.RELEASE
//      p.param #= Param.reportPruneToCap(block.cap, toCap)
//      p.size #= log2Up(blockSize)
//      p.source #= source
//      p.address #= block.address
//      p.data.randomize()
//      p.corrupt #= false
//    }
//    var denied = false
//    monitor.d(source) = {d =>
//      monitor.d(source) = null
//      assert(d.opcode.toEnum == Opcode.D.RELEASE_ACK)
//      mutex.unlock()
//    }
//    mutex.await()
////    ordering.checkDone(source)
//
//    this.block.changeCap(block, toCap)
//    block.release()
//    denied
//  }
//
//
//  def putFullData(source : Int, address : Long, data : Seq[Byte]) : Boolean = {
//    val size = log2Up(data.length)
//    driver.a.burst { push =>
//      for (offset <- 0 until data.length by bus.p.dataBytes) {
//        push { p =>
//          val buf = new Array[Byte](bus.p.dataBytes)
//          val byteOffset = (address & (bus.p.dataBytes - 1)).toInt
//          val bytes = data.size
//          for (i <- 0 until bus.p.dataBytes) {
//            val ptr = offset + i - byteOffset
//            (ptr >= 0 && ptr < bytes) match {
//              case false => buf(i) = Random.nextInt().toByte
//              case true => buf(i) = data(ptr)
//            }
//          }
//          p.opcode #= Opcode.A.PUT_FULL_DATA
//          p.param #= 0
//          p.size #= size
//          p.source #= source
//          p.address #= address + offset
//          p.mask #= ((BigInt(1) << bus.p.dataBytes.min(data.size)) - 1) << (address.toInt & (bus.p.dataBytes - 1))
//          p.data #= buf
//          p.corrupt #= false
//        }
//      }
//    }
//    val mutex = SimMutex().lock()
//    var denied = false
//    monitor.d(source) = {d =>
//      monitor.d(source) = null
//      assert(d.opcode.toEnum == Opcode.D.ACCESS_ACK)
//      denied = d.denied.toBoolean
//      mutex.unlock()
//    }
//    mutex.await()
////    ordering.checkDone(source)
//    denied
//  }
//

  def get(source : Int, address : Long, bytes : Int) : TransactionD = {
    val debugId = idAllocator.allocate()
    val a = TransactionA()
    a.opcode  = Opcode.A.GET
    a.size    = log2Up(bytes)
    a.source  = source
    a.address = address
    a.debugId = debugId
    driver.scheduleA(a)

    val d = waitD(source)
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

    val d = waitD(source)
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

    val d = waitD(source)
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

    val d = waitD(source)
    var b : Block = null
    d.opcode match {
      case Opcode.D.GRANT => {
        val param = d.param
        b = block(source, address)
        assert(b.cap == Param.Cap.toB)
        b.cap = Param.Cap.toT
        if(debug) println(f"acquireBlock src=$source%02x addr=$address%x 1 -> 0 time=${simTime()}")
        onGrant(source, address, param)
      }
      case Opcode.D.GRANT_DATA => { //TODO on naxriscv, may sent a aquire BtoT but may have been probed out meanwhile => test
        assert(!block.contains(source, address))
        onGrant(source, address, param)
        b = new Block(source, address, d.param, false, d.data){
          override def release() = {
            super.release()
            if(retains == 0) {
              probe.foreach(block.executeProbe)
            }
            block.updateBlock(this)
          }
        }
        if(debug) println(f"acquireBlock src=$source%02x addr=$address%x 2 -> ${d.param} time=${simTime()}")
        block(source -> address) = b
      }
    }


    val e = TransactionE(d.sink)
    driver.scheduleE(e)

    idAllocator.remove(debugId)
    b
  }
}


class BlockManager(ma : MasterAgent){
  import ma._
  val sourceToMaster = (0 until  1 << bus.p.sourceWidth).map(source => bus.p.node.m.getMasterFromSource(source))
  val blocks = mutable.LinkedHashMap[(M2sAgent, Long), Block]()
  def apply(source : Int, address : Long) = blocks(sourceToMaster(source) -> address)
  def get(source : Int, address : Long) : Option[Block] = blocks.get(sourceToMaster(source) -> address)
  def contains(source : Int, address : Long) = blocks.contains(sourceToMaster(source) -> address)
  def update(key : (Int, Long), block : Block) = {
    val key2 = (sourceToMaster(key._1) -> key._2)
    assert(!blocks.contains(key2))
    blocks(key2) = block
  }
  def removeBlock(source : Int, address : Long) = {
    blocks.remove(sourceToMaster(source) -> address)
  }
  def probeCap(block : Block, cap : Int) = {
    if(debug) if(cap != block.cap) println(f"probeCap     src=${block.source}%02x addr=${block.address}%x ${block.cap} -> $cap time=${simTime()}")
    block.cap = cap
    updateBlock(block)
  }

  def executeProbe(probe : Probe): Unit ={
    probe.perm match{
      case false => {
        blocks.get(sourceToMaster(probe.source) -> probe.address) match {
          case Some(b : Block) => {
            b.probe = None
            b.retains match {
              case 0 => {
                b.cap < probe.param match {
                  case false => probeAck(
                    source = probe.source,
                    toCap =  b.cap,
                    block = b
                  )
                  case true  => {
                    b.dirty match {
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
              case _ => ???
            }
            updateBlock(b)
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
    }
  }

  def updateBlock(block : Block): Unit ={
    if(block.retains == 0) {
      if(block.cap == Param.Cap.toN){
        removeBlock(block.source, block.address)
      }
    }
  }
}
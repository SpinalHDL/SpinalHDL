package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink._
import spinal.lib.sim.{StreamDriver, StreamDriverOoo, StreamMonitor, StreamReadyRandomizer}

import scala.collection.{breakOut, mutable}
import scala.util.Random


abstract class Monitor (val bus : Bus, cd : ClockDomain) {
  var debug = false

  def onA(f : TransactionA) : Unit
  def onB(f : TransactionB) : Unit
  def onC(f : TransactionC) : Unit
  def onD(f : TransactionD) : Unit
  def onE(f : TransactionE) : Unit

  val faa = new TransactionAggregator[TransactionA](bus.p.dataBytes)(onA)
  val fab = bus.p.withBCE generate new TransactionAggregator[TransactionB](bus.p.dataBytes)(onB)
  val fac = bus.p.withBCE generate new TransactionAggregator[TransactionC](bus.p.dataBytes)(onC)
  val fad = new TransactionAggregator[TransactionD](bus.p.dataBytes)(onD)

  val aToD = Array.fill(1 << bus.p.sourceWidth)(BigInt(0))
  val cToD = Array.fill(1 << bus.p.sourceWidth)(BigInt(0))
  val a = StreamMonitor(bus.a, cd){p =>
    val f = TransactionA(p)
    if(f.opcode == Opcode.A.GET || f.opcode == Opcode.A.ACQUIRE_BLOCK) aToD(f.source) = f.address
    faa.push(f)
  }
  val b = bus.p.withBCE generate StreamMonitor(bus.b, cd)(p => fab.push(TransactionB(p)))
  val c = bus.p.withBCE generate StreamMonitor(bus.c, cd){p =>
    val f = TransactionC(p)
    if(f.opcode == Opcode.C.RELEASE_DATA || f.opcode == Opcode.C.RELEASE) cToD(f.source) = f.address
    fac.push(f)
  }
  val e = bus.p.withBCE generate StreamMonitor(bus.e, cd)(p => onE(TransactionE(p)))
  val d = StreamMonitor(bus.d, cd) {p =>
    val address = p.opcode.toEnum match {
      case Opcode.D.ACCESS_ACK | Opcode.D.ACCESS_ACK_DATA => {
        val v = aToD(p.source.toInt)
        aToD(p.source.toInt) += bus.p.dataBytes
        v
      }
      case Opcode.D.GRANT | Opcode.D.GRANT_DATA | Opcode.D.RELEASE_ACK =>{
        val v = aToD(p.source.toInt)
        cToD(p.source.toInt) += bus.p.dataBytes
        v
      }
    }
    fad.push(TransactionD(p, address))
  }


//
//
//
//
//
//  def probeBlock(source : Int,
//                 param : Int,
//                 address : Long,
//                 bytes : Int): Unit ={
////    ???
//  }
//
//  def probeAck(source : Int,
//               param : Int,
//               address : Long,
//               bytes : Int): Unit ={
//    driver.c.single{p =>
//      p.opcode  #= Opcode.C.PROBE_ACK
//      p.param   #= param
//      p.size    #= log2Up(bytes)
//      p.source  #= source
//      p.address #= address
//      if(bus.p.withBCE) {
//        p.data.randomize()
//        p.corrupt.randomize()
//      }
//    }
//  }
//
//  def probeAck(source : Int,
//               toCap : Int,
//               block  :Block): Unit ={
//    this.block.changeCap(block, toCap)
//    probeAck(source, Param.reportPruneToCap(block.cap, toCap), block.address, blockSize)
//  }
//
//  def probeAckData(source : Int,
//                   toCap : Int,
//                   block  : Block)
//                  (orderingBody : => Unit) : Unit = {
//    this.block.changeCap(block, toCap)
//    probeAckData(source, Param.reportPruneToCap(block.cap, toCap), block.address, block.data)(orderingBody)
//  }
//
//
//  def probeAckData(source : Int,
//                   param : Int,
//                   address : Long,
//                   data : Seq[Byte])
//                 (orderingBody : => Unit) : Unit = {
//    //    ordering(source)(orderingBody)
//    orderingBody
//    val size = log2Up(data.length)
//    driver.c.burst { push =>
//      for (offset <- 0 until data.length by bus.p.dataBytes) {
//        push { p =>
//          val buf = new Array[Byte](bus.p.dataBytes)
//          (0 until bus.p.dataBytes).foreach(i => buf(i) = data(offset + i))
//          p.opcode #= Opcode.C.PROBE_ACK_DATA
//          p.param #= param
//          p.size #= size
//          p.source #= source
//          p.address #= address + offset
//          p.data #= buf
//          p.corrupt #= false
//        }
//      }
//    }
//  }
//
//  def onGrant(source : Int, address : Long, param : Int) : Unit = {}
//  def acquireBlock(source : Int,
//                   param : Int,
//                   address : Long,
//                   bytes : Int)
//                  (orderingBody : OrderingArgs => Unit): Block ={
//    ordering(source)(orderingBody)
//    driver.a.single{p =>
//      p.opcode  #= Opcode.A.ACQUIRE_BLOCK
//      p.param   #= param
//      p.size    #= log2Up(bytes)
//      p.source  #= source
//      p.address #= address
//      if(p.withData) {
//        p.mask.randomize()
//        p.data.randomize()
//        p.corrupt.randomize()
//      }
//    }
//
//    val mutex = SimMutex().lock()
//    val data = new Array[Byte](bytes)
//    var offset = 0
//    var b : Block = null
//    var sink = -1
//    monitor.d(source) = {d =>
//      d.opcode.toEnum match {
//        case Opcode.D.GRANT => {
//          val param = d.param.toInt
//          sink = d.sink.toInt
//          b = block(source, address)
//          assert(b.cap == Param.Cap.toB)
//          b.cap = Param.Cap.toT
//          monitor.d(source) = null
//          mutex.unlock()
//          if(debug) println(f"src=$source addr=$address%x 1 -> 0 time=${simTime()}")
//          onGrant(source, address, param)
//        }
//        case Opcode.D.GRANT_DATA => { //TODO on naxriscv, may sent a aquire BtoT but may have been probed out meanwhile => test
//          assert(!block.contains(source, address))
//          val raw = d.data.toBytes
//          for(i <- 0 until bus.p.dataBytes){
//            data(offset + i) = raw(i)
//          }
//          assert(!d.denied.toBoolean)
//          assert(!d.corrupt.toBoolean)
//
//          offset += bus.p.dataBytes
//          if(offset == bytes){
//            monitor.d(source) = null
//            mutex.unlock()
//            val param = d.param.toInt
//            onGrant(source, address, param)
//            b = new Block(source, address, param, false, data){
//              override def release() = {
//                super.release()
//                if(retains == 0) {
//                  probe.foreach(block.executeProbe)
//                }
//                block.updateBlock(this)
//              }
//            }
//            if(debug) println(f"src=$source addr=$address%x 2 -> $param time=${simTime()}")
//            block(source -> address) = b
//            sink = d.sink.toInt
//          }
//        }
//      }
//    }
//    mutex.await()
//    ordering.checkDone(source)
//    driver.e.single{p =>
//      p.sink  #= sink
//    }
//
//    b
//  }
//
//  def get(source : Int, address : Long, bytes : Int)
//         (orderingBody : OrderingArgs => Unit) : Seq[Byte] = {
//    val debugId = DebugId.manager.allocate(orderingBody)
//    ordering(source)(orderingBody)
//    driver.a.single{p =>
//      p.opcode  #= Opcode.A.GET
//      p.param   #= 0
//      p.size    #= log2Up(bytes)
//      p.source  #= source
//      p.address #= address
//      if(p.withData) {
//        p.mask.randomize()
//        p.data.randomize()
//        p.corrupt.randomize()
//      }
//      if(p.debugId.getWidth != 0) p.debugId #= debugId
//    }
//
//    val mutex = SimMutex().lock()
//    val data = new Array[Byte](bytes)
//    var offset = 0
//    val byteOffset = (address & (bus.p.dataBytes-1)).toInt
//    monitor.d(source) = {d =>
//      assert(d.opcode.toEnum == Opcode.D.ACCESS_ACK_DATA, s"Unexpected transaction on $bus")
//      val raw = d.data.toBytes
//      for(i <- 0 until bus.p.dataBytes){
//        val ptr = offset + i - byteOffset
//        if(ptr >= 0 && ptr < bytes) data(ptr) = raw(i)
//      }
//      assert(!d.denied.toBoolean)
//      assert(!d.corrupt.toBoolean)
//
//      offset += bus.p.dataBytes
//      if(offset >= bytes){
//        monitor.d(source) = null
//        mutex.unlock()
//      }
//    }
//    mutex.await()
//    ordering.checkDone(source)
//    DebugId.manager.remove(debugId)
//    data
//  }
//
//  def releaseData(source : Int, toCap : Int, block : Block)
//                 (orderingBody : OrderingArgs => Unit) : Boolean = {
//    assert(block.dirty)
//    block.dirty = false
//    block.retain()
//    ordering(source)(orderingBody)
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
//    ordering.checkDone(source)
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
//    ordering.checkDone(source)
//
//    this.block.changeCap(block, toCap)
//    block.release()
//    denied
//  }
//
//
//  def putFullData(source : Int, address : Long, data : Seq[Byte])
//                 (orderingBody : OrderingArgs => Unit) : Boolean = {
//    ordering(source)(orderingBody)
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
//    ordering.checkDone(source)
//    denied
//  }
//
//  def putPartialData(source : Int, address : Long, data : Seq[Byte], mask : Seq[Boolean])
//                    (orderingBody : OrderingArgs => Unit) : Boolean = {
//    val debugId = DebugId.manager.allocate(orderingBody)
//    ordering(source)(orderingBody)
//    val size = log2Up(data.length)
//    driver.a.burst { push =>
//      for (offset <- 0 until data.length by bus.p.dataBytes) {
//        push { p =>
//          val buf = new Array[Byte](bus.p.dataBytes)
//          //        (0 until bus.p.dataBytes).foreach(i => buf(i) = data(offset + i))
//          val buf2 = Array.fill[Byte]((bus.p.dataBytes + 7) / 8)(0)
//          //        (0 until bus.p.dataBytes).foreach(i => buf2(i >> 3) = (buf2(i >> 3) | (mask(offset + i).toInt << (i & 7))).toByte)
//
//          val byteOffset = (address & (bus.p.dataBytes - 1)).toInt
//          val bytes = data.size
//          for (i <- 0 until bus.p.dataBytes) {
//            val ptr = offset + i - byteOffset
//            (ptr >= 0 && ptr < bytes) match {
//              case false => buf(i) = Random.nextInt().toByte
//              case true => {
//                buf(i) = data(ptr)
//                buf2(i >> 3) = (buf2(i >> 3) | (mask(ptr).toInt << (i & 7))).toByte
//              }
//            }
//          }
//
//          p.opcode #= Opcode.A.PUT_PARTIAL_DATA
//          p.param #= 0
//          p.size #= size
//          p.source #= source
//          p.address #= address + offset
//          p.mask #= buf2
//          p.data #= buf
//          p.corrupt #= false
//          p.debugId #= debugId
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
//    ordering.checkDone(source)
//    DebugId.manager.remove(debugId)
//    denied
//  }
}

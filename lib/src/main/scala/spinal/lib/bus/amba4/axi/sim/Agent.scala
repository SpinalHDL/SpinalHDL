package spinal.lib.bus.amba4.axi.sim


import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4WriteOnly}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.sim.{SimData, StreamDriver, StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable
import scala.util.Random



abstract class Axi4WriteOnlyMasterAgent(bus : Axi4WriteOnly, clockDomain: ClockDomain){
  val awQueue = mutable.Queue[() => Unit]()
  val wQueue = mutable.Queue[() => Unit]()
  val bQueue = Array.fill(1 << bus.config.idWidth)(mutable.Queue[() => Unit]())
  var allowGen = true
  var rspCounter = 0
  def pending = bQueue.exists(_.nonEmpty)
  StreamReadyRandomizer(bus.b, clockDomain)

  def genAddress() : BigInt
  def mappingAllocate(mapping : SizeMapping) : Boolean
  def mappingFree(mapping : SizeMapping) : Unit

  def onCmdWrite(address : BigInt, data : Byte) : Unit = {}
  def bursts = List(0,1,2)

  def genCmd() : Unit = {
    if(!allowGen) return
    val region = bus.aw.region.randomizedInt()
    val burst = bursts(Random.nextInt(bursts.size))
    val len = if(burst == 2) List(2,4,8,16)(Random.nextInt(4))-1 else Random.nextInt(16)
    val lenBeat = len + 1
    val size = Random.nextInt(log2Up(bus.config.bytePerWord) + 1)
    val sizeByte = 1 << size
    val id = bus.aw.id.randomizedInt()
    var mapping : SizeMapping = null
    var address, startAddress, endAddress  : BigInt = null
    val byteCount = sizeByte*lenBeat
    do{
      address = genAddress()
      burst match {
        case 0 =>
          startAddress = address
          endAddress   = startAddress + sizeByte
        case 1 =>
          startAddress = address
          endAddress = startAddress + byteCount
        case 2 =>
          address = address & ~BigInt(sizeByte-1)
          startAddress = address & ~(byteCount-1)
          endAddress = startAddress + byteCount
      }
      mapping = SizeMapping(startAddress, endAddress - startAddress)
    } while(!mappingAllocate(mapping));

    val firstBeatOffset = (startAddress & (sizeByte-1)).toInt

    awQueue.enqueue { () =>
      bus.aw.addr #= address
      if (bus.config.useId) bus.aw.id #= id
      if (bus.config.useRegion) bus.aw.region #= region
      if (bus.config.useLen) bus.aw.len #= len
      if (bus.config.useSize) bus.aw.size #= size
      if (bus.config.useBurst) bus.aw.burst #= burst
      if (bus.config.useLock) bus.aw.lock.randomize()
      if (bus.config.useCache) bus.aw.cache.randomize()
      if (bus.config.useQos) bus.aw.qos.randomize()
      if (bus.config.awUserWidth >= 0) bus.aw.user.randomize()
      if (bus.config.useProt) bus.aw.prot.randomize()
    }

    var beatOffset = (address & (bus.config.bytePerWord-1)).toInt
    for (beat <- 0 until lenBeat) {
      val beatOffsetCache = beatOffset
//      println(beatOffsetCache)
      wQueue.enqueue { () =>
        bus.w.data.randomize()
        if(bus.config.useStrb)  bus.w.strb #= (Random.nextInt(1 << sizeByte) << beatOffsetCache) & ((1 << bus.config.bytePerWord)-1)
        if(bus.config.useWUser) bus.w.user.randomize()
        if(bus.config.useLast)  bus.w.last #= beat == lenBeat-1
      }
      beatOffset += sizeByte
      beatOffset &= (bus.config.bytePerWord-1)
    }

    //WRITE RSP
    bQueue(id).enqueue { () =>
      assert(bus.b.resp.toInt == 0)
      mappingFree(mapping)
    }
  }

  def maskRandom() = Random.nextBoolean()
  StreamDriver(bus.aw, clockDomain){ _ =>
    if(awQueue.isEmpty) genCmd()
    if(awQueue.nonEmpty) { awQueue.dequeue().apply(); true } else false
  }

  StreamDriver(bus.w, clockDomain){ _ =>
    if(wQueue.isEmpty) genCmd()
    if(wQueue.nonEmpty) { wQueue.dequeue().apply(); true } else false
  }

  val rspMonitor = StreamMonitor(bus.b, clockDomain){_ =>
    bQueue(bus.b.id.toInt).dequeue()()
    rspCounter+=1
  }
}

class Axi4WriteOnlySlaveAgent(bus : Axi4WriteOnly, clockDomain: ClockDomain) {
  val awQueue = mutable.Queue[Int]()
  var wCount = 0
  val bQueue = Array.fill(1 << bus.config.idWidth)(mutable.Queue[() => Unit]())

  def update(): Unit ={
    if(awQueue.nonEmpty && wCount > 0){
      val id = awQueue.dequeue()
      wCount -= 1
      bQueue(id) += {() =>
        bus.b.id #= id
        bus.b.resp #= 0
      }
    }
  }

  val awMonitor = StreamMonitor(bus.aw, clockDomain){aw =>
    val id = aw.id.toInt
    awQueue += id
    update()
  }
  val wMonitor = StreamMonitor(bus.w, clockDomain){w =>
    if(w.last.toBoolean) {
      wCount = wCount + 1
      update()
    }
  }

  StreamDriver(bus.b, clockDomain){ _ =>
    val queues = bQueue.filter(_.nonEmpty)
    if(queues.nonEmpty) {
      queues(Random.nextInt(queues.size)).dequeue().apply()
      true
    }else{
      false
    }
  }

  StreamReadyRandomizer(bus.aw, clockDomain)
  StreamReadyRandomizer(bus.w, clockDomain)
}

abstract class Axi4WriteOnlyMonitor(bus : Axi4WriteOnly, clockDomain: ClockDomain){

  def onWriteByte(address : BigInt, data : Byte) : Unit

  case class WTransaction(data : BigInt, strb : BigInt, last : Boolean){

  }
  val wQueue = mutable.Queue[WTransaction]()
  val wProcess = mutable.Queue[(WTransaction) => Unit]()

  def update(): Unit ={
    while(wQueue.nonEmpty && wProcess.nonEmpty){
      wProcess.dequeue().apply(wQueue.dequeue())
    }
  }

  val awMonitor = StreamMonitor(bus.aw, clockDomain){_ =>
    val size = bus.aw.size.toInt
    val len = bus.aw.len.toInt
    val burst = bus.aw.burst.toInt
    val addr = bus.aw.addr.toBigInt
    val bytePerBeat = (1 << size)
    val bytes = (len + 1) * bytePerBeat
    for(beat <- 0 to len) {
      val beatAddress = burst match {
        case 0 => addr
        case 1 => (addr + bytePerBeat*beat) & ~BigInt(bus.config.bytePerWord-1)
        case 2 => {
          val base = addr & ~BigInt(bytes-1)
          (base + ((addr + bytePerBeat*beat) & BigInt(bytes-1))) &  ~BigInt(bus.config.bytePerWord-1)
        }
      }
      wProcess += { (w : WTransaction) =>
        assert(w.last == (beat == len))
        val strb = w.strb
        val data = w.data
        for(i <- 0 until bus.config.bytePerWord){
          if(((strb >> i) & 1) != 0){
            onWriteByte(beatAddress + i, ((data >> (8*i)).toInt & 0xFF).toByte)
          }
        }
      }
    }
    update()
  }

  val wMonitor = StreamMonitor(bus.w, clockDomain){w =>
    wQueue += WTransaction(w.data.toBigInt, w.strb.toBigInt, w.last.toBoolean)
    update()
  }
}
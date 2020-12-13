package spinal.lib.bus.amba4.axi.sim


import spinal.core._
import spinal.core.sim._
import spinal.lib.Stream
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Ar, Axi4Aw, Axi4B, Axi4R, Axi4ReadOnly, Axi4W, Axi4WriteOnly}
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
    val len = if(burst == 2) List(2,4,8,16)(Random.nextInt(4))-1 else Random.nextInt(64)
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
        val bytesInBeat = sizeByte - (beatOffsetCache % sizeByte)
        if(bus.config.useStrb)  bus.w.strb #= ((Random.nextInt(1 << bytesInBeat)) << beatOffsetCache) & ((1 << bus.config.bytePerWord)-1)
//        if(bus.config.useStrb)  bus.w.strb #= (((1 << bytesInBeat)-1) << beatOffsetCache) & ((1 << bus.config.bytePerWord)-1)
        if(bus.config.useWUser) bus.w.user.randomize()
        if(bus.config.useLast)  bus.w.last #= beat == lenBeat-1
      }
      beatOffset += sizeByte
      beatOffset &= (bus.config.bytePerWord-1)
      beatOffset &= ~(sizeByte-1)
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

abstract class Axi4ReadOnlyMasterAgent(bus : Axi4ReadOnly, clockDomain: ClockDomain){
  val arQueue = mutable.Queue[() => Unit]()
  val rQueue = Array.fill(1 << bus.config.idWidth)(mutable.Queue[() => Unit]())
  var allowGen = true
  var rspCounter = 0

  def genAddress() : BigInt
  def mappingAllocate(mapping : SizeMapping) : Boolean
  def mappingFree(mapping : SizeMapping) : Unit
  def bursts = List(0,1,2)


  def pending = rQueue.exists(_.nonEmpty)
  StreamReadyRandomizer(bus.r, clockDomain)

  def genCmd() : Unit = {
    if(!allowGen) return
    val region = bus.ar.region.randomizedInt()
    val burst = bursts(Random.nextInt(bursts.size))
    val len = if(burst == 2) List(2,4,8,16)(Random.nextInt(4))-1 else Random.nextInt(16)
    val lenBeat = len + 1
    val size = Random.nextInt(log2Up(bus.config.bytePerWord) + 1)
    val sizeByte = 1 << size
    val id = bus.ar.id.randomizedInt()
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

    arQueue.enqueue { () =>
      bus.ar.addr #= address
      if (bus.config.useId) bus.ar.id #= id
      if (bus.config.useRegion) bus.ar.region #= region
      if (bus.config.useLen) bus.ar.len #= len
      if (bus.config.useSize) bus.ar.size #= size
      if (bus.config.useBurst) bus.ar.burst #= burst
      if (bus.config.useLock) bus.ar.lock.randomize()
      if (bus.config.useCache) bus.ar.cache.randomize()
      if (bus.config.useQos) bus.ar.qos.randomize()
      if (bus.config.arUserWidth >= 0) bus.ar.user.randomize()
      if (bus.config.useProt) bus.ar.prot.randomize()
    }

    //READ RSP
    for(beat <- 0 to len) rQueue(id).enqueue { () =>
      assert(bus.r.resp.toInt == 0)
      assert(bus.r.last.toBoolean == (beat == len))
      if(bus.r.last.toBoolean) {
        mappingFree(mapping)
        rspCounter+=1
      }
    }
  }

  def maskRandom() = Random.nextBoolean()
  StreamDriver(bus.ar, clockDomain){ _ =>
    if(arQueue.isEmpty) genCmd()
    if(arQueue.nonEmpty) { arQueue.dequeue().apply(); true } else false
  }

  val rspMonitor = StreamMonitor(bus.r, clockDomain){_ =>
    rQueue(bus.r.id.toInt).dequeue()()
  }
}

class Axi4WriteOnlySlaveAgent(aw : Stream[Axi4Aw], w : Stream[Axi4W], b : Stream[Axi4B], clockDomain: ClockDomain) {
  def this(bus: Axi4WriteOnly, clockDomain: ClockDomain) {
    this(bus.aw, bus.w, bus.b, clockDomain);
  }
  def this(bus: Axi4, clockDomain: ClockDomain) {
    this(bus.aw, bus.w, bus.b, clockDomain);
  }
  val busConfig = aw.config
  val awQueue = mutable.Queue[Int]()
  var wCount = 0
  val bQueue = Array.fill(1 << busConfig.idWidth)(mutable.Queue[() => Unit]())

  def update(): Unit ={
    if(awQueue.nonEmpty && wCount > 0){
      val id = awQueue.dequeue()
      wCount -= 1
      bQueue(id) += {() =>
        b.id #= id
        b.resp #= 0
      }
    }
  }

  val awMonitor = StreamMonitor(aw, clockDomain){aw =>
    val id = aw.id.toInt
    awQueue += id
    update()
  }
  val wMonitor = StreamMonitor(w, clockDomain){w =>
    if(w.last.toBoolean) {
      wCount = wCount + 1
      update()
    }
  }

  val bDriver = StreamDriver(b, clockDomain){ _ =>
    val queues = bQueue.filter(_.nonEmpty)
    if(queues.nonEmpty) {
      queues(Random.nextInt(queues.size)).dequeue().apply()
      true
    }else{
      false
    }
  }

  val awDriver = StreamReadyRandomizer(aw, clockDomain)
  val wDriver = StreamReadyRandomizer(w, clockDomain)
}


class Axi4ReadOnlySlaveAgent(ar : Stream[Axi4Ar], r : Stream[Axi4R], clockDomain: ClockDomain) {
  def this(bus: Axi4ReadOnly, clockDomain: ClockDomain) {
    this(bus.ar, bus.r, clockDomain);
  }
  def this(bus: Axi4, clockDomain: ClockDomain) {
    this(bus.ar, bus.r, clockDomain);
  }
  val busConfig = ar.config
  val rQueue = Array.fill(1 << busConfig.idWidth)(mutable.Queue[() => Unit]())
  def readByte(address : BigInt) : Byte = Random.nextInt().toByte

  val arMonitor = StreamMonitor(ar, clockDomain){ar =>
    val size = ar.size.toInt
    val len = ar.len.toInt
    val id = ar.id.toInt
    val burst = ar.burst.toInt
    val addr = ar.addr.toBigInt
    val bytePerBeat = (1 << size)
    val bytes = (len + 1) * bytePerBeat
    for(beat <- 0 to len) {
      val beatAddress = burst match {
        case 0 => addr
        case 1 => (addr + bytePerBeat*beat) & ~BigInt(busConfig.bytePerWord-1)
        case 2 => {
          val base = addr & ~BigInt(bytes-1)
          (base + ((addr + bytePerBeat*beat) & BigInt(bytes-1))) &  ~BigInt(busConfig.bytePerWord-1)
        }
      }
      rQueue(id) += { () =>
        r.id #= id
        r.resp #= 0
        r.last #= (beat == len)
        var data = BigInt(0)
        for(i <- 0 until busConfig.bytePerWord){
          data = data | (BigInt(readByte(beatAddress + i).toInt & 0xFF)) << i*8
        }
        r.data #= data
      }
    }
  }


  val rDriver = StreamDriver(r, clockDomain){ _ =>
    val queues = rQueue.filter(_.nonEmpty)
    if(queues.nonEmpty) {
      queues(Random.nextInt(queues.size)).dequeue().apply()
      true
    }else{
      false
    }
  }

  val arDriver = StreamReadyRandomizer(ar, clockDomain)
}

abstract class Axi4WriteOnlyMonitor(aw : Stream[Axi4Aw], w : Stream[Axi4W], b : Stream[Axi4B], clockDomain: ClockDomain) {
  def this(bus: Axi4WriteOnly, clockDomain: ClockDomain) {
    this(bus.aw, bus.w, bus.b, clockDomain);
  }
  def this(bus: Axi4, clockDomain: ClockDomain) {
    this(bus.aw, bus.w, bus.b, clockDomain);
  }
  val busConfig = aw.config
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

  val awMonitor = StreamMonitor(aw, clockDomain){_ =>
    val size = aw.size.toInt
    val len = aw.len.toInt
    val burst = aw.burst.toInt
    val addr = aw.addr.toBigInt
    val bytePerBeat = (1 << size)
    val bytes = (len + 1) * bytePerBeat
    for(beat <- 0 to len) {
      val beatAddress = burst match {
        case 0 => addr
        case 1 => (addr + bytePerBeat*beat) & ~BigInt(busConfig.bytePerWord-1)
        case 2 => {
          val base = addr & ~BigInt(bytes-1)
          (base + ((addr + bytePerBeat*beat) & BigInt(bytes-1))) &  ~BigInt(busConfig.bytePerWord-1)
        }
      }
      wProcess += { (w : WTransaction) =>
        assert(w.last == (beat == len))
        val strb = w.strb
        val data = w.data
        for(i <- 0 until busConfig.bytePerWord){
          if(((strb >> i) & 1) != 0){
            onWriteByte(beatAddress + i, ((data >> (8*i)).toInt & 0xFF).toByte)
          }
        }
      }
    }
    update()
  }

  val wMonitor = StreamMonitor(w, clockDomain){w =>
    wQueue += WTransaction(w.data.toBigInt, w.strb.toBigInt, w.last.toBoolean)
    update()
  }
}




abstract class Axi4ReadOnlyMonitor(bus : Axi4ReadOnly, clockDomain: ClockDomain){
  def onReadByte(address : BigInt, data : Byte, id : Int) : Unit
  def onLast(id : Int) : Unit

  val rQueue = mutable.Queue[() => Unit]()

  val arMonitor = StreamMonitor(bus.ar, clockDomain){_ =>
    val size = bus.ar.size.toInt
    val len = bus.ar.len.toInt
    val id = bus.ar.id.toInt
    val burst = bus.ar.burst.toInt
    val addr = bus.ar.addr.toBigInt
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
      rQueue += { () =>
        assert(bus.r.last.toBoolean == (beat == len))
        assert(bus.r.resp.toInt == 0)
        val data = bus.r.data.toBigInt
        for(i <- 0 until bus.config.bytePerWord){
          onReadByte(beatAddress + i, ((data >> (8*i)).toInt & 0xFF).toByte, id)
        }
        if(bus.r.last.toBoolean) onLast(id)
      }
    }
  }

  val rMonitor = StreamMonitor(bus.r, clockDomain){r =>
    rQueue.dequeue().apply()
  }
}




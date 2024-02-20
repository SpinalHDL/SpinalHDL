package spinal.lib.bus.amba4.axi.sim


import spinal.core._
import spinal.core.sim._
import spinal.lib.Stream
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Ar, Axi4Aw, Axi4B, Axi4R, Axi4ReadOnly, Axi4W, Axi4WriteOnly}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.sim.{SimData, StreamDriver, StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable



abstract class Axi4WriteOnlyMasterAgent(aw : Stream[Axi4Aw], w : Stream[Axi4W], b : Stream[Axi4B], clockDomain: ClockDomain){
  def this(bus: Axi4WriteOnly, clockDomain: ClockDomain) {
    this(bus.aw, bus.w, bus.b, clockDomain);
  }
  def this(bus: Axi4, clockDomain: ClockDomain) {
    this(bus.aw, bus.w, bus.b, clockDomain);
  }

  val pageAlignBits = Axi4.boundaryWidth
  val busConfig = aw.config
  if(!busConfig.useLen){
    SpinalWarning("The Axi4Config with useLen == false is only tested by assigning len = 0, " 
      + "determine the burst length of transcation by last signal should not work.")
  }

  val awQueue = mutable.Queue[() => Unit]()
  val wQueue = mutable.Queue[() => Unit]()
  val idCount = if(busConfig.useId) (1 << busConfig.idWidth) else 1
  val bQueue = Array.fill(idCount)(mutable.Queue[() => Unit]())
  var allowGen = true
  var cmdCounter = 0
  var rspCounter = 0
  val assertOkResp = true
  def pending = bQueue.exists(_.nonEmpty)
  val bDriver = StreamReadyRandomizer(b, clockDomain)

  def genAddress() : BigInt
  def mappingAllocate(mapping : SizeMapping) : Boolean
  def mappingFree(mapping : SizeMapping) : Unit

  def onCmdWrite(address : BigInt, data : Byte) : Unit = {}
  def bursts = List(0,1,2)
  def sizes  = (0 to log2Up(busConfig.bytePerWord)).toList
  def lens   = (0 to 64).toList ++ List(255)

  def genCmd() : Unit = {
    if(!allowGen) return
    val region        = if (busConfig.useRegion) aw.region.randomizedInt() else 0
    val id            = if (busConfig.useId) aw.id.randomizedInt() else 0
    val burst         = if (busConfig.useBurst) bursts(simRandom.nextInt(bursts.size)) else 1
    var len: Int      = 0
    var size: Int     = log2Up(busConfig.bytePerWord)
    var sizeByte: Int = busConfig.bytePerWord    

    var mapping: SizeMapping                      = null
    var address, startAddress, endAddress: BigInt = null
    var addrValid                                 = false

    var attempts = 0
    do{
      if (busConfig.useLen) {
        len = burst match {
          case 0 => simRandom.nextInt(16)
          case 1 => lens(simRandom.nextInt(lens.size))
          case 2 => List(2, 4, 8, 16)(simRandom.nextInt(4)) - 1
        }
      }
      val lenBeat = len + 1
      if (busConfig.useSize) size = sizes(simRandom.nextInt(sizes.size))
      sizeByte = 1 << size
      val byteCount = sizeByte * lenBeat

      address = genAddress()
      val boundAddress = ((address >> pageAlignBits) + 1) << pageAlignBits
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
      endAddress = endAddress & ~BigInt(sizeByte - 1)
      addrValid = endAddress <= boundAddress
      if (addrValid) mapping = SizeMapping(startAddress, endAddress - startAddress)
      attempts += 1
      if(attempts == 10) return
    } while(!addrValid || !mappingAllocate(mapping));

    cmdCounter += 1
    awQueue.enqueue { () =>
      aw.addr #= address
      if (busConfig.useId) aw.id #= id
      if (busConfig.useRegion) aw.region #= region
      if (busConfig.useLen) aw.len #= len
      if (busConfig.useSize) aw.size #= size
      if (busConfig.useBurst) aw.burst #= burst
      if (busConfig.useLock) aw.lock.randomize()
      if (busConfig.useCache) aw.cache.randomize()
      if (busConfig.useQos) aw.qos.randomize()
      if (busConfig.awUserWidth >= 0) aw.user.randomize()
      if (busConfig.useProt) aw.prot.randomize()
    }

    var beatOffset = (address & (busConfig.bytePerWord-1)).toInt
    for (beat <- 0 to len) {
      val beatOffsetCache = beatOffset
      wQueue.enqueue { () =>
        w.data.randomize()
        val bytesInBeat = sizeByte - (beatOffsetCache % sizeByte)
        if(busConfig.useStrb)  w.strb #= ((BigInt(bytesInBeat, simRandom)) << beatOffsetCache) & ((BigInt(1) << size)-1)
        if(busConfig.useWUser) w.user.randomize()
        if(busConfig.useLast)  w.last #= beat == len
      }
      beatOffset += sizeByte
      beatOffset &= (busConfig.bytePerWord-1)
      beatOffset &= ~(sizeByte-1)
    }

    //WRITE RSP
    bQueue(id).enqueue { () =>
      if(busConfig.useResp && assertOkResp) assert(b.resp.toInt == 0)
      mappingFree(mapping)
    }
  }

  def maskRandom() = simRandom.nextBoolean()
  val awDriver = StreamDriver(aw, clockDomain){ _ =>
    if(awQueue.isEmpty) genCmd()
    if(awQueue.nonEmpty) { awQueue.dequeue().apply(); true } else false
  }

  val wDriver = StreamDriver(w, clockDomain){ _ =>
    if(wQueue.isEmpty) genCmd()
    if(wQueue.nonEmpty) { wQueue.dequeue().apply(); true } else false
  }

  val rspMonitor = StreamMonitor(b, clockDomain){_ =>
    val id = if(busConfig.useId) b.id.toInt else 0
    bQueue(id).dequeue()()
    rspCounter+=1
  }

  def reset(){
    wQueue.clear()
    bQueue.map(q => q.clear())
    awQueue.clear()
    awDriver.reset()
    wDriver.reset()
  }
}

abstract class Axi4ReadOnlyMasterAgent(ar : Stream[Axi4Ar], r : Stream[Axi4R], clockDomain: ClockDomain){
  def this(bus: Axi4ReadOnly, clockDomain: ClockDomain) {
    this(bus.ar, bus.r, clockDomain);
  }
  def this(bus: Axi4, clockDomain: ClockDomain) {
    this(bus.ar, bus.r, clockDomain);
  }
  
  val pageAlignBits = 12
  val busConfig = ar.config
  if(!busConfig.useLen){
    SpinalWarning("The Axi4Config with useLen == false is only tested by assigning len = 0, " 
      + "determine the burst length of transcation by last signal should not work.")
  }

  val arQueue = mutable.Queue[() => Unit]()
  val idCount = if(busConfig.useId) (1 << busConfig.idWidth) else 1
  val rQueue = Array.fill(idCount)(mutable.Queue[() => Unit]())
  var allowGen = true
  var cmdCounter = 0
  var rspCounter = 0
  val assertOkResp = true

  def genAddress() : BigInt
  def mappingAllocate(mapping : SizeMapping) : Boolean
  def mappingFree(mapping : SizeMapping) : Unit
  def bursts = List(0,1,2)
  def sizes  = (0 to log2Up(busConfig.bytePerWord)).toList
  def lens   = (0 to 64).toList ++ List(255)

  def pending = rQueue.exists(_.nonEmpty)
  val rDriver = StreamReadyRandomizer(r, clockDomain)

  def genCmd() : Unit = {
    if(!allowGen) return
    val region        = if (busConfig.useRegion) ar.region.randomizedInt() else 0
    val id            = if (busConfig.useId) ar.id.randomizedInt() else 0
    val burst         = if (busConfig.useBurst) bursts(simRandom.nextInt(bursts.size)) else 1
    var len: Int      = 0
    var size: Int     = log2Up(busConfig.bytePerWord)
    var sizeByte: Int = busConfig.bytePerWord    

    var mapping: SizeMapping                      = null
    var address, startAddress, endAddress: BigInt = null
    var addrValid                                 = false
    var attempts = 0

    do{
      attempts += 1
      if(attempts == 10) return
      if (busConfig.useLen) {
        len = burst match {
          case 0 => simRandom.nextInt(16)
          case 1 => lens(simRandom.nextInt(lens.size))
          case 2 => List(2, 4, 8, 16)(simRandom.nextInt(4)) - 1
        }
      }
      val lenBeat = len + 1
      if (busConfig.useSize) size = sizes(simRandom.nextInt(sizes.size))
      sizeByte = 1 << size
      val byteCount = sizeByte * lenBeat

      address = genAddress()
      val boundAddress = ((address >> pageAlignBits) + 1) << pageAlignBits
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
      endAddress = endAddress & ~BigInt(sizeByte - 1)
      addrValid = endAddress <= boundAddress
      if (addrValid) mapping = SizeMapping(startAddress, endAddress - startAddress)
    } while(!addrValid || !mappingAllocate(mapping));

    cmdCounter += 1
    arQueue.enqueue { () =>
      ar.addr #= address
      if (busConfig.useId) ar.id #= id
      if (busConfig.useRegion) ar.region #= region
      if (busConfig.useLen) ar.len #= len
      if (busConfig.useSize) ar.size #= size
      if (busConfig.useBurst) ar.burst #= burst
      if (busConfig.useLock) ar.lock.randomize()
      if (busConfig.useCache) ar.cache.randomize()
      if (busConfig.useQos) ar.qos.randomize()
      if (busConfig.arUserWidth >= 0) ar.user.randomize()
      if (busConfig.useProt) ar.prot.randomize()
    }

    //READ RSP
    for(beat <- 0 to len) rQueue(id).enqueue { () =>
      if(busConfig.useResp && assertOkResp) assert(r.resp.toInt == 0)
      if(busConfig.useLast) assert(r.last.toBoolean == (beat == len))
      if((busConfig.useLast && r.last.toBoolean) || (beat == len)) {
        mappingFree(mapping)
        rspCounter+=1
      }
    }
  }

  def maskRandom() = simRandom.nextBoolean()
  val arDriver = StreamDriver(ar, clockDomain){ _ =>
    if(arQueue.isEmpty) genCmd()
    if(arQueue.nonEmpty) { arQueue.dequeue().apply(); true } else false
  }

  val rspMonitor = StreamMonitor(r, clockDomain){_ =>
    val id = if(busConfig.useId) r.id.toInt else 0
    rQueue(id).dequeue()()
  }
  
  def reset(){
    rQueue.map(q => q.clear())
    arQueue.clear()
    arDriver.reset()
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
  if(!busConfig.useLen){
    SpinalWarning("The Axi4Config with useLen == false is only tested by assigning len = 0, " 
      + "determine the burst length of transcation by last signal should not work.")
  }

  var awQueueDepth = 8
  var bQueueDepth = 4
  val awQueue = mutable.Queue[Int]()
  val idCount = if(busConfig.useId) (1 << busConfig.idWidth) else 1
  val bQueue = Array.fill(idCount)(mutable.Queue[() => Unit]())
  var qPending = 0

  val wQueue = mutable.Queue[Boolean]()
  val wProcess = mutable.Queue[(Boolean) => Unit]()

  def update(): Unit ={    
    while(wQueue.nonEmpty && wProcess.nonEmpty){
      wProcess.dequeue().apply(wQueue.dequeue())
    }
  }

  val awMonitor = StreamMonitor(aw, clockDomain){aw =>
    val id = if(busConfig.useId) aw.id.toInt else 0
    awQueue += id

    val len = if(busConfig.useLen) aw.len.toInt else 0
    for(beat <- 0 to len) {
      wProcess += { (last : Boolean) =>
        if(busConfig.useLast) assert(last == (beat == len))
        if(beat == len){
          val id = awQueue.dequeue()
          qPending += 1
          bQueue(id) += {() =>
            if(busConfig.useId) b.id #= id
            if(busConfig.useResp) b.resp #= 0
          }
        }
      }
    }
    update()
  }
  val wMonitor = StreamMonitor(w, clockDomain){w =>
    val last = if(busConfig.useLast) w.last.toBoolean else false
    wQueue += last
    update()
  }

  val bDriver = StreamDriver(b, clockDomain){ _ =>
    val queues = bQueue.filter(_.nonEmpty)
    if(queues.nonEmpty) {
      queues(simRandom.nextInt(queues.size)).dequeue().apply()
      qPending -= 1
      true
    }else{
      false
    }
  }

  val awDriver = StreamReadyRandomizer(aw, clockDomain, () => awQueue.size < awQueueDepth && qPending < bQueueDepth)
  val wDriver = StreamReadyRandomizer(w, clockDomain,  () => qPending < bQueueDepth)
}


class Axi4ReadOnlySlaveAgent(ar : Stream[Axi4Ar], r : Stream[Axi4R], clockDomain: ClockDomain, withReadInterleaveInBurst : Boolean = true, withArReordering : Boolean = true) {
  def this(bus: Axi4ReadOnly, clockDomain: ClockDomain) {
    this(bus.ar, bus.r, clockDomain);
  }
  def this(bus: Axi4, clockDomain: ClockDomain) {
    this(bus.ar, bus.r, clockDomain);
  }
  def this(bus: Axi4ReadOnly, clockDomain: ClockDomain, withReadInterleaveInBurst : Boolean, withArReordering : Boolean) {
    this(bus.ar, bus.r, clockDomain, withReadInterleaveInBurst, withArReordering);
  }
  def this(bus: Axi4, clockDomain: ClockDomain, withReadInterleaveInBurst : Boolean, withArReordering : Boolean) {
    this(bus.ar, bus.r, clockDomain, withReadInterleaveInBurst, withArReordering);
  }


  var baseLatency = 0l
  val busConfig = ar.config
  if(!busConfig.useLen){
    SpinalWarning("The Axi4Config with useLen == false is only tested by assigning len = 0, " 
      + "determine the burst length of transcation by last signal should not work.")
  }

  var arQueueDepth = 8
  var rQueueDepth = 256
  var rPending = 0
  val arQueue = mutable.Queue[Int]()
  val arIdQueue = !withArReordering generate mutable.Queue[Int]()
  val idCount = if(busConfig.useId) (1 << busConfig.idWidth) else 1
  val rQueue = Array.fill(idCount)(mutable.Queue[(Boolean, () => Unit)]())
  def readByte(address : BigInt) : Byte = simRandom.nextInt().toByte
  def onReadStart(address : BigInt, size : Int, length : Int) : Unit = {}

  val arMonitor = StreamMonitor(ar, clockDomain){ar =>
    val size = if(busConfig.useSize) ar.size.toInt else log2Up(busConfig.dataWidth / 8)
    val len = if(busConfig.useLen) ar.len.toInt else 0
    val id = if(busConfig.useId) ar.id.toInt else 0
    val burst = if(busConfig.useBurst) ar.burst.toInt else 1
    val addr = ar.addr.toBigInt
    val bytePerBeat = (1 << size)
    val bytes = (len + 1) * bytePerBeat
    onReadStart(addr, size, len)
    delayed(baseLatency) {
      for(beat <- 0 to len) {
        val beatAddress = burst match {
          case 0 => addr
          case 1 => (addr + bytePerBeat*beat) & ~BigInt(busConfig.bytePerWord-1)
          case 2 => {
            val base = addr & ~BigInt(bytes-1)
            (base + ((addr + bytePerBeat*beat) & BigInt(bytes-1))) &  ~BigInt(busConfig.bytePerWord-1)
          }
        }
        rPending += 1
        rQueue(id) += (beat == len) -> { () =>
          if (busConfig.useId) r.id #= id
          if (busConfig.useResp) r.resp #= 0
          if (busConfig.useLast) r.last #= (beat == len)
          var data = BigInt(0)
          for (i <- 0 until busConfig.bytePerWord) {
            data = data | (BigInt(readByte(beatAddress + i).toInt & 0xFF)) << i * 8
          }
          r.data #= data
          if (beat == len) {
            arQueue.dequeue()
          }
        }
      }
      if(!withArReordering) arIdQueue += id
    }
    arQueue.enqueue(id)
  }

  var rQueueLock : mutable.Queue[(Boolean, () => Unit)] = null
  val rDriver = StreamDriver(r, clockDomain){ _ =>
    if(rQueueLock == null){
      withArReordering match {
        case false => if(!withArReordering && arIdQueue.nonEmpty) {
          rQueueLock = rQueue(arIdQueue.dequeue())
        }
        case true => {
          val queues = rQueue.filter(_.nonEmpty)
          if(queues.nonEmpty) {
            rQueueLock = queues(simRandom.nextInt(queues.size))
          }
        }
      }
    }
    if(rQueueLock != null) {
      val item = rQueueLock.dequeue()
      item._2()
      rPending -= 1
      if(withReadInterleaveInBurst || item._1) {
        rQueueLock = null
      }
      true
    }else{
      false
    }
  }

  val arDriver = StreamReadyRandomizer(ar, clockDomain, () => arQueue.size < arQueueDepth && rPending < rQueueDepth)
}

abstract class Axi4WriteOnlyMonitor(aw : Stream[Axi4Aw], w : Stream[Axi4W], b : Stream[Axi4B], clockDomain: ClockDomain) {
  def this(bus: Axi4WriteOnly, clockDomain: ClockDomain) {
    this(bus.aw, bus.w, bus.b, clockDomain);
  }
  def this(bus: Axi4, clockDomain: ClockDomain) {
    this(bus.aw, bus.w, bus.b, clockDomain);
  }

  val busConfig = aw.config
  if(!busConfig.useLen){
    SpinalWarning("The Axi4Config with useLen == false is only tested by assigning len = 0, " 
      + "determine the burst length of transcation by last signal should not work.")
  }

  def onWriteStart(address: BigInt, id: Int, size: Int, len: Int, burst: Int): Unit = {}
  def onWriteByteAlways(address: BigInt, data: Byte, strobe: Boolean, id: Int): Unit = {}
  def onWriteByte(address : BigInt, data : Byte, id: Int) : Unit = {}
  @deprecated("Use onWriteByte with ID argument")
  def onWriteByte(address : BigInt, data : Byte) : Unit = {} // Legacy, use onWriteByte for more ID information
  def onResponse(id: Int, resp: Byte): Unit = {}
  case class WTransaction(data : BigInt, strb : BigInt, last : Boolean)
  case class BTransaction(resp: Byte, id: Int)

  var awCounter, wCounter, bCounter = 0

  val wQueue = mutable.Queue[WTransaction]()
  val wProcess = mutable.Queue[(WTransaction) => Unit]()
  val bQueue = mutable.Queue[BTransaction]()

  def update(): Unit ={
    while(wQueue.nonEmpty && wProcess.nonEmpty){
      wProcess.dequeue().apply(wQueue.dequeue())
    }
    while(bQueue.nonEmpty) {
      val bTxn = bQueue.dequeue()
      onResponse(bTxn.id, bTxn.resp)
    }
  }

  val awMonitor = StreamMonitor(aw, clockDomain){_ =>
    val size = if(busConfig.useSize) aw.size.toInt else log2Up(busConfig.bytePerWord)
    val len = if(busConfig.useLen) aw.len.toInt else 0
    val burst = if(busConfig.useBurst) aw.burst.toInt else 1
    val id = if (busConfig.useId) aw.id.toInt.toByte else 0
    val addr = aw.addr.toBigInt
    val bytePerBeat = (1 << size)
    val bytes = (len + 1) * bytePerBeat
    val bytePerBus = 1 << log2Up(busConfig.dataWidth / 8)

    onWriteStart(addr, id, size, len, burst)

    for(beat <- 0 to len) {
      val beatAddress = burst match {
        case 0 => addr
        case 1 => (addr + bytePerBeat*beat)
        case 2 => {
          val base = addr & ~BigInt(bytes-1)
          (base + ((addr + bytePerBeat*beat) & BigInt(bytes-1)))
        }
      }
      val accessAddress = beatAddress & ~BigInt(busConfig.bytePerWord-1)

      wProcess += { (w : WTransaction) =>
        if(busConfig.useLast) assert(w.last == (beat == len))
        val strb = if(busConfig.useStrb) w.strb else ((BigInt(1) << busConfig.bytePerWord) - 1)
        val data = w.data
        val start = ((beatAddress & ~BigInt(bytePerBeat-1)) - accessAddress).toInt
        val end = start + bytePerBeat
        for(i <- 0 until bytePerBus){
          val _byte = ((data >> (8*i)).toInt & 0xFF).toByte
          val strobe = ((strb >> i) & 1) != 0
          onWriteByteAlways(accessAddress + i, _byte, strobe, id)
          if (start <= i && i < end && strobe) {
            onWriteByte(accessAddress + i, _byte)
            onWriteByte(accessAddress + i, _byte, id)
          }
        }
      }
    }
    awCounter += 1
    update()
  }

  val wMonitor = StreamMonitor(w, clockDomain){w =>
    val strb = if(busConfig.useStrb) w.strb.toBigInt else ((BigInt(1) << busConfig.bytePerWord) - 1)
    val last = if(busConfig.useLast) w.last.toBoolean else false
    wQueue += WTransaction(w.data.toBigInt, strb, last)
    wCounter += 1
    update()
  }

  val bMonitor = StreamMonitor(b, clockDomain){b =>
    val id = if (busConfig.useId) b.id.toInt.toByte else 0
    val resp: Byte = if (busConfig.useResp) b.resp.toInt.toByte else 0
    bQueue += BTransaction(resp, id)
    bCounter += 1
    update()
  }

  def reset(){
    wProcess.clear()
    wQueue.clear()
    bQueue.clear()
  }
}




abstract class Axi4ReadOnlyMonitor(ar : Stream[Axi4Ar], r : Stream[Axi4R], clockDomain: ClockDomain, withReadInterleaveInBurst : Boolean = true){
  def this(bus: Axi4ReadOnly, clockDomain: ClockDomain){
    this(bus.ar, bus.r, clockDomain);
  }
  def this(bus: Axi4, clockDomain: ClockDomain){
    this(bus.ar, bus.r, clockDomain);
  }

  val busConfig = ar.config
  if(!busConfig.useLen){
    SpinalWarning("The Axi4Config with useLen == false is only tested by assigning len = 0, " 
      + "determine the burst length of transcation by last signal should not work.")
  }

  val assertOkResp = true
  var arCounter, rCounter, rLastCounter = 0

  def onReadStart(address: BigInt, id: Int, size: Int, len: Int, burst: Int): Unit = {}
  def onReadByteAlways(address: BigInt, data: Byte, id: Int): Unit = {}
  def onReadByte(address : BigInt, data : Byte, id : Int) : Unit = {}
  def onResponse(address: BigInt, id: Int, last: Boolean, resp: Byte): Unit = {}
  @deprecated("Use onResponse which provides more transaction information")
  def onLast(id: Int): Unit = {} // Legacy, use onResponse for more transaction information

  val rQueue = Array.fill((1 << ar.payload.config.idWidth) max 1)(mutable.Queue[(Boolean, () => Unit)]())

  val arMonitor = StreamMonitor(ar, clockDomain){_ =>
    val size = if(busConfig.useSize) ar.size.toInt else log2Up(busConfig.dataWidth / 8)
    val len = if(busConfig.useLen) ar.len.toInt else 0
    val id = if(busConfig.useId) ar.id.toInt else 0
    val burst = if(busConfig.useBurst) ar.burst.toInt else 1
    val addr = ar.addr.toBigInt
    val bytePerBeat = (1 << size)
    val bytes = (len + 1) * bytePerBeat
    val bytePerBus = 1 << log2Up(busConfig.dataWidth / 8)

    onReadStart(addr, id, size, len, burst)

    for(beat <- 0 to len) {
      val beatAddress = burst match {
        case 0 => addr
        case 1 => (addr + bytePerBeat*beat)
        case 2 => {
          val base = addr & ~BigInt(bytes-1)
          (base + ((addr + bytePerBeat*beat) & BigInt(bytes-1)))
        }
      }
      val accessAddress = beatAddress & ~BigInt(busConfig.bytePerWord-1)

      rQueue(id) += (beat == len) -> { () =>
        if(busConfig.useLast) assert(r.last.toBoolean == (beat == len))
        if(busConfig.useResp && assertOkResp) assert(r.resp.toInt == 0)
        val data = r.data.toBigInt
        val start = ((beatAddress & ~BigInt(bytePerBeat-1)) - accessAddress).toInt
        val end = start + bytePerBeat
        for(i <- 0 until bytePerBus){
          val _byte = ((data >> (8*i)).toInt & 0xFF).toByte
          onReadByteAlways(accessAddress + i, _byte, id)
          if (start <= i && i < end) {
            onReadByte(accessAddress + i, _byte, id)
          }
        }
        val last = (busConfig.useLast && r.last.toBoolean) || (beat == len)
        if (last) onLast(id)
        val resp: Byte = if (busConfig.useResp) r.resp.toInt.toByte else 0
        onResponse(accessAddress, id, last, resp)
      }
    }
    arCounter += 1
  }

  var rIdLock = -1
  val rMonitor = StreamMonitor(r, clockDomain){r =>
    val id = if(ar.payload.config.useId) r.id.toInt else 0
    if(!withReadInterleaveInBurst) rIdLock match {
      case -1 => rIdLock = id
      case ref => assert(ref == id, "Bad read id")
    }
    val item = rQueue(id).dequeue()
    item._2.apply()
    if(!withReadInterleaveInBurst && item._1) rIdLock = -1
    if(r.config.useLast) assert(r.last.toBoolean == item._1, "Bad read last")
    rCounter += 1
    rLastCounter += (if(r.config.useLast)r.last.toBoolean.toInt else 1)
  }

  def reset(){
    rQueue.foreach(_.clear())
  }
}

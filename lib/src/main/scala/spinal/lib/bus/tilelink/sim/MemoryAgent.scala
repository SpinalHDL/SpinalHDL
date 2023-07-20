package spinal.lib.bus.tilelink.sim

import spinal.core.{ClockDomain, log2Up}
import spinal.lib.bus.tilelink.{Bus, Opcode, Param}
import spinal.lib.sim._
import spinal.core.sim._

import scala.collection.mutable
import scala.util.Random


class MemoryAgent(bus: Bus, cd: ClockDomain, seed : Long = Random.nextInt(), blockSize : Int = 64)(implicit idCallback : IdCallback) extends MonitorSubscriber{
  val mem = SparseMemory(seed)

  val monitor = new Monitor(bus, cd).add(this)
  val driver = new SlaveDriver(bus, cd)
  val sinkAllocator = new BlockingIdAllocator(bus.p.sinkWidth)
  val locks = mutable.LinkedHashMap[Long, SimMutex]()
  def reserve(address : Long) = {
    locks.getOrElseUpdate(address, SimMutex(randomized=true)).lock
  }
  def release(address : Long) = {
    val l = locks(address)
    l.unlock()
    if(!l.locked){
      locks.remove(address)
    }
  }

  override def onA(a: TransactionA) = fork{
    val r = Random.nextFloat()
    cd.waitSampling((r*r*20).toInt) //Will enable out of order handeling
    val blockAddress = a.address.toLong & ~(blockSize-1)
    reserve(blockAddress)
    a.opcode match {
      case Opcode.A.GET => {
        handleCoherency(a, Param.Cap.toN)
        if(idCallback != null) idCallback.call(a.debugId)(new OrderingArgs(0, a.bytes))
        val d = TransactionD(a)
        d.opcode = Opcode.D.ACCESS_ACK_DATA
        d.data = mem.readBytes(a.address.toLong, a.bytes)
        driver.scheduleD(d)
      }
      case Opcode.A.PUT_PARTIAL_DATA | Opcode.A.PUT_FULL_DATA => { //TODO probePerm not tested
        handleCoherency(a, Param.Cap.toN, a.bytes == blockSize && a.opcode == Opcode.A.PUT_FULL_DATA)
        if(idCallback != null) idCallback.call(a.debugId)(new OrderingArgs(0, a.bytes))
        mem.write(a.address.toLong, a.data, a.mask)
        val d = TransactionD(a)
        d.opcode = Opcode.D.ACCESS_ACK
        driver.scheduleD(d)
      }
      case Opcode.A.ACQUIRE_BLOCK => {
        val probe = handleCoherency(a, Param.Grow.getCap(a.param))
        if(idCallback != null) idCallback.call(a.debugId)(new OrderingArgs(0, a.bytes))

        val sink = sinkAllocator.allocate()
        val d = TransactionD(a)
        d.sink = sink
        d.param = if(probe.unique) Param.Cap.toT else Param.Cap.toB
        probe.needData match {
          case false =>{
            d.opcode = Opcode.D.GRANT
          }
          case true => {
            d.opcode = Opcode.D.GRANT_DATA
            d.data = mem.readBytes(a.address.toLong, a.bytes)
          }
        }
        driver.scheduleD(d)
        waitE(sink)
        sinkAllocator.remove(sink)
      }
      case Opcode.A.ACQUIRE_PERM => {
        val probe = handleCoherency(a, Param.Grow.getCap(a.param))
        if(idCallback != null) idCallback.call(a.debugId)(new OrderingArgs(0, a.bytes))
        assert(probe.unique)

        val sink = sinkAllocator.allocate()
        val d = TransactionD(a)
        d.sink = sink
        d.param = Param.Cap.toT
        d.opcode = Opcode.D.GRANT
        driver.scheduleD(d)
        waitE(sink)
        sinkAllocator.remove(sink)
      }
    }
    release(blockAddress)
  }


  case class CoherencyReport(val needData : Boolean,
                             val unique : Boolean)
  def handleCoherency(a: TransactionA, cap : Int, allowProbePerm : Boolean = false): CoherencyReport ={
    val blockAddress = a.address.toLong & ~(blockSize-1)
    var needData = false
    var unique = true
    val inflights = for(m <- bus.p.node.m.masters if m.emits.withBCE) yield fork{
      val isSelf = m.sourceHit(a.source) && (a.opcode == Opcode.A.ACQUIRE_BLOCK || a.opcode == Opcode.A.ACQUIRE_PERM)
      val mapping = m.mapping.randomPick()
      val b = TransactionB()
      b.opcode = Opcode.B.PROBE_BLOCK
      if(allowProbePerm && Random.nextBoolean()) {
        b.opcode = Opcode.B.PROBE_PERM
      }
      b.address = blockAddress
      b.param = cap match {
        case Param.Cap.toT => if(isSelf) Param.Cap.toT else Param.Cap.toN
        case Param.Cap.toB => Param.Cap.toB
        case Param.Cap.toN => Param.Cap.toN
      }
      b.source = mapping.id.randomPick().toInt
      b.size = a.size
      driver.scheduleB(b)

      val c = waitC(b.source, blockAddress)
      c.opcode match{
        case Opcode.C.PROBE_ACK =>
        case Opcode.C.PROBE_ACK_DATA => {
          mem.write(c.address.toLong, c.data)
        }
      }
      if(isSelf && c.param == Param.Report.NtoN){
        needData = true
      }
      if(!isSelf && Param.reportPruneToCap(c.param) != Param.Cap.toN) unique = false
    }
    inflights.foreach(_.join())
    CoherencyReport(needData, unique)
  }


  override def onB(b: TransactionB) = {}

  val callbackOnC = mutable.LinkedHashMap[(Int, Long), TransactionC => Unit]()
  override def onC(c: TransactionC) = {
    import Opcode.C._
    c.opcode match {
      case PROBE_ACK | PROBE_ACK_DATA => callbackOnC(c.source -> c.address.toLong).apply(c)
      case RELEASE | RELEASE_DATA => {
        if(c.opcode == RELEASE_DATA){
          mem.write(c.address.toLong, c.data)
        }

        val d = TransactionD(c)
        d.opcode = Opcode.D.RELEASE_ACK
        driver.scheduleD(d)
      }
    }
  }

  override def onD(d: TransactionD) = {}

  val callbackOnE = mutable.LinkedHashMap[BigInt, TransactionE => Unit]()
  override def onE(e: TransactionE) = {
    callbackOnE(e.sink).apply(e)
  }


  def waitC(source : Int, address : Long) : TransactionC = {
    var value : TransactionC = null
    val lock = SimMutex().lock()
    callbackOnC(source -> address) = { c =>
      value = c
      lock.unlock()
    }
    lock.await()
    callbackOnC(source -> address) = null
    value
  }

  def waitE(sink : BigInt) : TransactionE = {
    var value : TransactionE = null
    val lock = SimMutex().lock()
    callbackOnE(sink) = { e =>
      value = e
      lock.unlock()
    }
    lock.await()
    callbackOnE(sink) = null
    value
  }

}
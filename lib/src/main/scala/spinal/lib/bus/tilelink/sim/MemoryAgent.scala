package spinal.lib.bus.tilelink.sim

import spinal.core.{ClockDomain, log2Up}
import spinal.lib.bus.tilelink.{Bus, M2sAgent, Opcode, Param}
import spinal.lib.sim._
import spinal.core.sim._
import spinal.sim.SimThread

import scala.collection.mutable


class MemoryAgent(bus: Bus,
                  cd: ClockDomain,
                  seed : Long = simRandom.nextInt(),
                  blockSize : Int = 64,
                  var randomProberFactor : Float = 0.0f,
                  var randomProberDelayMax : Int = 1000
                 )(implicit idCallback : IdCallback) extends MonitorSubscriber{
  implicit val _ = sm

  val mem = SparseMemory(seed)

  val monitor = new Monitor(bus, cd).add(this)
  val driver = new SlaveDriver(bus, cd)
  val sinkAllocator = new BlockingIdAllocator(bus.p.sinkWidth)
  val locks = mutable.LinkedHashMap[Long, SimMutex]()
  def reserve(address : Long) = {
    locks.get(address) match {
      case Some(x) => x.lock(); cd.waitSampling(simRandom.nextInt(10))
      case None => locks(address) = SimMutex(randomized=true).lock
    }
  }
  def release(address : Long) = {
    val l = locks(address)
    l.unlock()
    if(!l.locked){
      locks.remove(address)
    }
  }

  val capMap = mutable.LinkedHashMap[M2sAgent, mutable.LinkedHashMap[Long, Int]]()
  val sourceToM2s = new Array[M2sAgent](1 << bus.p.sourceWidth)
  for(m <- bus.p.node.m.masters){
    capMap(m) = mutable.LinkedHashMap[Long, Int]()
    for(source <- m.mapping){
      source.id.foreach{sourceId =>
        sourceToM2s(sourceId.toInt) = m
      }
    }
  }

  def getCap(source: Int, address : Long) = capMap(sourceToM2s(source)).getOrElse(address, Param.Cap.toN)

  def getCap(m2sAgent: M2sAgent, address : Long) = capMap(m2sAgent).getOrElse(address, Param.Cap.toN)

  def changeCap(source : Int, address : Long, cap : Int) = {
    val m2s = sourceToM2s(source)
    if(cap == Param.Cap.toN)
      capMap(m2s).remove(address)
    else
      capMap(m2s)(address) = cap
  }

  override def onA(a: TransactionA) = {
    if(bus.p.withBCE && simRandom.nextFloat() < randomProberFactor) fork {
      cd.waitSampling(simRandom.nextInt(randomProberDelayMax))
      val address = a.address.toLong
      reserve(address)
      handleCoherency(
        address = address,
        isAquire = false,
        sourceAgent = null,
        cap = List(Param.Cap.toN, Param.Cap.toB).randomPick(),
        allowProbePerm = false
      )
      release(address)
    }

    fork{
      val r = simRandom.nextFloat()
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
          changeCap(a.source, a.address.toLong, d.param)
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
          changeCap(a.source, a.address.toLong, d.param)
          waitE(sink)
          sinkAllocator.remove(sink)
        }
      }
      release(blockAddress)
    }
  }


  case class CoherencyReport(val needData : Boolean,
                             val unique : Boolean)
  def handleCoherency(a: TransactionA, cap : Int, allowProbePerm : Boolean = false): CoherencyReport = {
    handleCoherency(
      address = a.address.toLong,
      isAquire = a.opcode == Opcode.A.ACQUIRE_BLOCK || a.opcode == Opcode.A.ACQUIRE_PERM,
      sourceAgent = sourceToM2s(a.source),
      cap = cap,
      allowProbePerm = allowProbePerm
    )
  }

  def handleCoherency(address : Long,
                      isAquire : Boolean,
                      sourceAgent : M2sAgent,
                      cap : Int,
                      allowProbePerm : Boolean): CoherencyReport ={
    val blockAddress = address & ~(blockSize-1)
    var needData = false
    var unique = true
    val inflights = mutable.ArrayBuffer[SimThread]()
    for(m <- bus.p.node.m.masters if m.emits.withBCE) {
      val isSelf = m == sourceAgent && isAquire
      //We check if we realy need to probe the agent, based on its known cap
      val needProbe = isSelf match {
        case false => getCap(m, address) == Param.Cap.toT || cap != Param.Cap.toB
        case true =>  false
      }
      val doProbe = needProbe || simRandom.nextBoolean() //Randomly generate unecessary probe, to simulate a lack of knowledge
      if(!doProbe) {
        if(isSelf && getCap(m, address) == Param.Cap.toN) needData = true
        if(!isSelf && getCap(m, address) != Param.Cap.toN) unique = false
      } else inflights += fork{
        val mapping = m.mapping.randomPick()
        val b = TransactionB()
        b.opcode = Opcode.B.PROBE_BLOCK
        if(allowProbePerm && simRandom.nextBoolean()) {
          b.opcode = Opcode.B.PROBE_PERM
        }
        b.address = blockAddress
        b.param = cap match {
          case Param.Cap.toT => if(isSelf) Param.Cap.toT else Param.Cap.toN
          case Param.Cap.toB => Param.Cap.toB
          case Param.Cap.toN => Param.Cap.toN
        }
        b.source = mapping.id.randomPick().toInt
        b.size = log2Up(blockSize)
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
    }
    inflights.foreach(_.join())
    CoherencyReport(needData, unique)
  }


  override def onB(b: TransactionB) = {}

  val callbackOnC = mutable.LinkedHashMap[(Int, Long), TransactionC => Unit]()
  override def onC(c: TransactionC) = {
    import Opcode.C._

    changeCap(c.source, c.address.toLong, Param.reportPruneToCap(c.param) max getCap(c.source, c.address.toLong))

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
package spinal.lib.bus.localbus

import spinal.core._
import spinal.core.sim._
class MemVIP{
  private val mem = scala.collection.mutable.HashMap[Long, BigInt]()
  def load(data:List[(Long, BigInt)]): Unit = mem ++= data
  def add(addr: Long, data: BigInt): Unit = mem += addr -> data
  def lookUp(addr: Long): BigInt = mem.getOrElse(addr, BigInt("FEFEFEFE", 16))
  def loadData(data: Map[Long, BigInt]) = mem ++= data
  def showMem = mem.map(t => s"${t._1.toHexString}_${t._2.toString(16)}").map(println)
  def hang(bus: MemBus)(cd: ClockDomain): MemVIP = {
    hangwrite(bus)(cd)
    hangread(bus)(cd)
    this
  }

  private def hangread(bus: MemBus)(cd: ClockDomain) ={
    fork{
      bus.rdat  #= 0
      while(true){
        cd.waitSampling()
        if(bus.ce.toBoolean && !bus.wr.toBoolean ){
          bus.rdat #= lookUp(bus.addr.toLong)
        } else {
          bus.rdat #= BigInt(0)
        }
      }
    }
  }

  private def hangwrite(bus: MemBus)(cd: ClockDomain) = {
    fork{
      while(true){
        cd.waitSampling()
        if(bus.ce.toBoolean && bus.wr.toBoolean){
          add(bus.addr.toLong, bus.wdat.toBigInt)
        }
      }
    }
  }

  def hang(bus: MinBus, delay: Int = 10)(cd: ClockDomain): MemVIP = {
    require(delay > 0, "bus read delay at least 1 cycle")
    SpinalInfo(s"${bus} hung a MemVIP(Verifcation IP module) with ${delay} bus delay")
    hangwrite(bus, delay)(cd)
    hangread(bus, delay)(cd)
    this
  }

  private def hangread(bus: MinBus, delay: Int = 10)(cd: ClockDomain) = {
    val delayQ = scala.collection.mutable.Queue[(Boolean, Long)](List.fill(delay)((false, 0L)): _*)
    fork {
      while (true) {
        if (bus.ce.toBoolean && !bus.wr.toBoolean && bus.rdy.toBoolean) {
          val addr = bus.addr.toLong
          delayQ.enqueue((true, addr))
        } else {
          delayQ.enqueue((false, 0L))
        }
        cd.waitSampling()
      }
    }
    fork {
      bus.rdat #= 0
      bus.rvld #= false
      bus.rdy  #= true
      waitUntil(!delayQ.isEmpty)
      while (true) {
        val (rd, raddr) = delayQ.dequeue()
        if (rd) {
          bus.rvld #= true
          bus.rdat #= lookUp(raddr)
          cd.waitSampling()
//          while (!bus.rready.toBoolean) {
//            cd.waitSampling()
//          }
        } else {
          bus.rvld #= false
          bus.rdat #= BigInt(0)
          cd.waitSampling()
        }
        bus.rvld #= false
      }
    }
  }

  private def hangwrite(bus: MinBus, delay: Int = 10)(cd: ClockDomain) = {
    val delayQ = scala.collection.mutable.Queue[(Boolean, Long, BigInt)](List.fill(delay)((false, 0L, BigInt(0))): _*)
    fork {
//      bus.simClear()
      while (true) {
        cd.waitSampling()
        if (bus.ce.toBoolean && bus.wr.toBoolean && bus.rdy.toBoolean) {
          val waddr = bus.addr.toLong
          val wdata = bus.wdat.toBigInt
          delayQ.enqueue((true, waddr, wdata))
        } else {
          delayQ.enqueue((false, 0L, BigInt(0)))
        }
        val (wr, waddr, wdata) = delayQ.dequeue()
        if (wr) {
          add(waddr, wdata)
        }
      }
    }
  }
}

package spinal.lib.bus.tilelink.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink._
import spinal.lib.sim.{StreamDriver, StreamDriverOoo, StreamMonitor, StreamReadyRandomizer}

import scala.util.Random

class SlaveAgent(bus : Bus, cd : ClockDomain) {
  val driver = new Area{
    val a = StreamReadyRandomizer(bus.a, cd)
    val b = bus.p.withBCE generate StreamDriverOoo(bus.b, cd)
    val c = bus.p.withBCE generate StreamReadyRandomizer(bus.c, cd)
    val d = StreamDriverOoo(bus.d, cd)
    val e = bus.p.withBCE generate StreamReadyRandomizer(bus.e, cd)
  }

  def onGet(debugId : Long,
            source : Int,
            address : Long,
            bytes : Int): Unit ={
    ???
  }

  def onPutPartialData(source : Int,
                       address : Long,
                       size : Int,
                       mask : Array[Boolean],
                       data : Array[Byte]): Unit ={
    ???
  }

  def accessAckData(source : Int,
                    address : Long,
                    data : Seq[Byte],
                    denied : Boolean = false,
                    corrupt : Boolean = false): Unit ={
    val size =  log2Up(data.size)
    if(data.size >= bus.p.dataBytes){
      accessAckDataImpl(source, size, data, denied, corrupt)
      return
    }

    val bytes = data.size
    val alignedAddr = address & ~(bus.p.dataBytes-1)
    val alignedBytes = bytes max bus.p.dataBytes
    val dataPatched = Array.fill(bus.p.dataBytes)(0.toByte)
    val offset = address & (bus.p.dataBytes-1)
    var ptr = 0
    if(data.size < bus.p.dataBytes){
      for(i <- 0 until bus.p.dataBytes) if(alignedAddr+i < address || alignedAddr+i >= address + bytes){
        dataPatched(i) = Random.nextInt().toByte
      } else {
        dataPatched(i) = data(ptr)
        ptr += 1
      }
    }
    accessAckDataImpl(source, size, dataPatched, denied, corrupt)
  }
  def accessAckDataImpl(source : Int,
                        size : Int,
                    data : Seq[Byte],
                    denied : Boolean = false,
                    corrupt : Boolean = false): Unit ={
    driver.d.burst { push =>
      for (offset <- 0 until data.size by bus.p.dataBytes) {
        push { p =>
          val buf = new Array[Byte](bus.p.dataBytes)
          (0 until bus.p.dataBytes).foreach(i => buf(i) = data(offset + i))
          p.opcode #= Opcode.D.ACCESS_ACK_DATA
          p.param #= 0
          p.size #= size
          p.source #= source
          p.sink #= 0
          p.denied #= denied
          p.corrupt #= false
          if (bus.p.withDataD) {
            p.data #= buf
            p.corrupt #= corrupt
          }
        }
      }
    }
  }
  def accessAck(source : Int,
                size : Int,
                denied : Boolean = false): Unit ={
    driver.d.burst { push =>
      push { p =>
        val buf = new Array[Byte](bus.p.dataBytes)
        p.opcode #= Opcode.D.ACCESS_ACK
        p.param #= 0
        p.size #= size
        p.source #= source
        p.sink #= 0
        p.denied #= denied
        if (bus.p.withDataD) {
          p.data.randomize()
          p.corrupt #= false
        }
      }
    }
  }

  //TODO the monitor need to buffer incoming request and notify them out of order to provide a better coverage
  val monitor = new Area{
    val a = StreamMonitor(bus.a, cd){ p =>
      val opcode = p.opcode.toEnum
      val source = p.source.toInt
      val address = p.address.toLong
      val debugId = p.debugId.toLong
      val size = p.size.toInt
      val bytes = 1 << size
      val offset = (address & (bus.p.dataBytes-1)).toInt
      opcode match {
        case Opcode.A.GET => {
//          DebugId.manager.call(debugId)(new OrderingArgs(address, bytes))
          onGet(debugId, source, address, bytes)
        }
        case Opcode.A.PUT_PARTIAL_DATA => {
//          DebugId.manager.call(debugId)(new OrderingArgs(address, bytes min p.p.dataBytes))
          onPutPartialData(source, address, size, p.mask.toBytes.flatMap(v => (0 to 7).map(i => ((v>>i)&1).toBoolean)).drop(offset), p.data.toBytes.drop(offset))
        }
      }
    }

    val c = bus.p.withBCE generate StreamMonitor(bus.c, cd){ p =>

    }
    val e = bus.p.withBCE generate StreamMonitor(bus.e, cd){ p =>

    }
  }
}

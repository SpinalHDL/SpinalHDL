package spinal.lib.wishbone.sim

import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.wishbone._
import scala.collection.mutable._
import scala.util.Random

//TODO: Adjust tag sampling/writing
//TODO: Verify STALL

case class WishboneTransaction( address : BigInt ,
                                data : BigInt  = 0,
                                tga : BigInt  = 0,
                                tgc : BigInt  = 0,
                                tgd : BigInt  = 0){
  override def toString : String = "Address: %d | Data: %d | TGA: %d | TGC: %d | TGD: %d".format(address,data,tga,tgc,tgd)
}

class WishboneDrive(wishbone: Wishbone, clockdomain: ClockDomain){
  def send(transaction : WishboneTransaction): Unit@suspendable = {
    //clockdomain.waitActiveEdgeWhere(wishbone.isCycle.toBoolean)
    //clockdomain.waitActiveEdge()
    wishbone.STB #= true
    wishbone.WE #= true
    wishbone.ADR #= transaction.address
    wishbone.DAT_MOSI #= transaction.data
    if(wishbone.config.useTGA) wishbone.TGA #= transaction.tga
    if(wishbone.config.useTGC) wishbone.TGC #= transaction.tgc
    if(wishbone.config.useTGD) wishbone.TGD_MOSI #= transaction.tgd
    //clockdomain.waitActiveEdgeWhere(wishbone.isAcknoledge.toBoolean)
    clockdomain.waitSamplingWhere(wishbone.ACK.toBoolean)
    wishbone.STB #= false
  }

  def receive(transaction : WishboneTransaction): WishboneTransaction@suspendable = {
    //clockdomain.waitSamplingWhere(wishbone.isCycle.toBoolean)
    //clockdomain.waitSamplingWhere(wishbone.CYC.toBoolean)
    wishbone.STB #= true
    wishbone.WE #= false
    wishbone.ADR #= transaction.address
    var tga : BigInt  = 0
    var tgc : BigInt  = 0
    var tgd : BigInt  = 0
    if(wishbone.config.useTGA) tga = wishbone.TGA.toBigInt
    if(wishbone.config.useTGC) tgc = wishbone.TGC.toBigInt
    if(wishbone.config.useTGD) tgd = wishbone.TGD_MISO.toBigInt

    //clockdomain.waitSamplingWhere(wishbone.isAcknoledge.toBoolean)
    clockdomain.waitSamplingWhere(wishbone.ACK.toBoolean && wishbone.CYC.toBoolean)
    wishbone.STB #= false
    WishboneTransaction(wishbone.ADR.toBigInt, wishbone.DAT_MISO.toBigInt, tga, tgc, tgd)
  }

  def write(transaction: WishboneTransaction): Unit@suspendable = {
      wishbone.CYC #= true
      send(transaction)
      wishbone.CYC #= false
  }

  def write(transactions: collection.immutable.Seq[WishboneTransaction]): Unit@suspendable = {
      wishbone.CYC #= true
      transactions.suspendable.foreach{ transaction => send(transaction) }
      wishbone.CYC #= false
  }

  def read(transaction: WishboneTransaction): WishboneTransaction@suspendable = {
      wishbone.CYC #= true
      val ret = receive(transaction)
      wishbone.CYC #= false
      ret
  }

  def read(transactions: Seq[WishboneTransaction]): Seq[WishboneTransaction]@suspendable = {
      wishbone.CYC #= true
      val ret = Seq[WishboneTransaction]()
      transactions.suspendable.foreach{ transaction => ret :+ receive(transaction) }
      wishbone.CYC #= false
      ret
  }
}

class WishbonePipelinedDrive(wishbone: Wishbone, clockdomain: ClockDomain){
  def send(transaction : WishboneTransaction): Unit@suspendable = {
    wishbone.STB #= true
    wishbone.WE #= true
    wishbone.STALL #= false
    wishbone.ADR #= transaction.address
    clockdomain.waitSamplingWhere(!wishbone.STALL.toBoolean)
    wishbone.DAT_MOSI #= transaction.data
    if(wishbone.config.useTGA) wishbone.TGA #= transaction.tga
    if(wishbone.config.useTGC) wishbone.TGC #= transaction.tgc
    if(wishbone.config.useTGD) wishbone.TGD_MOSI #= transaction.tgd
  }

  def receive( transaction : WishboneTransaction): WishboneTransaction@suspendable = {
    //clockdomain.waitSamplingWhere(wishbone.isCycle.toBoolean)
    //clockdomain.waitSamplingWhere(wishbone.CYC.toBoolean)
    wishbone.STB #= true
    wishbone.WE #= false
    wishbone.ADR #= transaction.address
    var tga : BigInt  = 0
    var tgc : BigInt  = 0
    var tgd : BigInt  = 0
    if(wishbone.config.useTGA) tga = wishbone.TGA.toBigInt
    if(wishbone.config.useTGC) tgc = wishbone.TGC.toBigInt
    if(wishbone.config.useTGD) tgd = wishbone.TGD_MISO.toBigInt

    //clockdomain.waitSamplingWhere(wishbone.isAcknoledge.toBoolean)
    clockdomain.waitSamplingWhere(wishbone.ACK.toBoolean && wishbone.CYC.toBoolean)
    WishboneTransaction(wishbone.ADR.toBigInt, wishbone.DAT_MISO.toBigInt, tga, tgc, tgd)
  }

  def write(transaction: WishboneTransaction): Unit@suspendable = {
      wishbone.CYC #= true
      send(transaction)
      wishbone.STB #= false
      clockdomain.waitSamplingWhere(wishbone.ACK.toBoolean)
      //waitUntil(wishbone.ACK.toBoolean && !wishbone.STB.toBoolean)
      wishbone.CYC #= false
  }

  def write(transactions: collection.immutable.Seq[WishboneTransaction]): Unit@suspendable = {
      wishbone.CYC #= true
      var count = 0
      val ackCounter = fork{
        while(count < transactions.size){
          clockdomain.waitSamplingWhere(wishbone.ACK.toBoolean && !wishbone.STALL.toBoolean)
          count += 1
        }
      }
      transactions.suspendable.foreach{ transaction => send(transaction) }
      wishbone.STB #= false
      ackCounter.join()
      wishbone.CYC #= false
  }

  def read(transaction: WishboneTransaction): WishboneTransaction@suspendable = {
      wishbone.CYC #= true
      val ret = receive(transaction)
      clockdomain.waitSamplingWhere(wishbone.ACK.toBoolean && !wishbone.STALL.toBoolean)
      wishbone.CYC #= false
      ret
  }

  def read(transactions: Seq[WishboneTransaction]): Seq[WishboneTransaction]@suspendable = {
      wishbone.CYC #= true
      val ret = Seq[WishboneTransaction]()
      transactions.suspendable.foreach{ transaction => ret :+ receive(transaction) }
      clockdomain.waitSamplingWhere(wishbone.ACK.toBoolean && !wishbone.STALL.toBoolean)
      wishbone.CYC #= false
      ret
  }

}
class Wishbonetest extends Component {
  val io = new Bundle{
    val bus = slave(Wishbone(WishboneConfig(8,8)))
    val data_out = out Bits(8 bits)
  }

  val feed = RegNext(io.bus.CYC && io.bus.STB) init(False)
  io.bus.ACK := feed && io.bus.STB
  val value = RegNextWhen(io.bus.DAT_MOSI, io.bus.CYC && io.bus.STB) init(0)
  io.data_out := value
  io.bus.DAT_MISO := value
}

object test{
  def main(args: Array[String]): Unit = {
    SimConfig(rtl = new Wishbonetest).withWave.doManagedSim{ dut =>
      dut.io.bus.CYC #= false
    //val transaction = collection.immutable.Seq(WishboneTransaction(42,12),WishboneTransaction(142,112),WishboneTransaction(242,212))
    val transaction : collection.immutable.Seq[WishboneTransaction] = for(x <- 0 to 20)yield {
      val coso = Random.nextInt(100)
      WishboneTransaction(42,coso)
    }
      dut.clockDomain.forkStimulus(period=10)
      dut.io.bus.CYC #= false
      dut.io.bus.STB #= false
      dut.io.bus.ACK #= false
      val test = new WishboneDrive(dut.io.bus, dut.clockDomain)
      test.write(transaction)
      sleep(100)
    }
  }
}

object testpipelined{
  class WishboneMasterPipelined extends Component{
    val io = new Bundle{
      val masterSide = slave(Wishbone(WishboneConfig(8,8, useSTALL = true)))
      val slaveSide = master(Wishbone(WishboneConfig(8,8, useSTALL = true)))
    }
    val dummy = Reg(Bits(8 bits))
    io.masterSide <> io.slaveSide
  }


  def main(args: Array[String]): Unit = {
    SimConfig(rtl = new WishboneMasterPipelined).withWave.doManagedSim{ dut =>
      dut.io.masterSide.CYC #= false
      dut.io.slaveSide.CYC #= false
      dut.io.masterSide.STB #= false
      dut.io.slaveSide.ACK #= false
      dut.io.masterSide.STALL #= false
      dut.io.slaveSide.STALL #= false
      dut.io.masterSide.WE #= false
      dut.clockDomain.forkStimulus(period=10)

      val slv = new WishbonePipelinedSlave(dut.io.slaveSide, dut.clockDomain)
      val mst = new WishbonePipelinedDrive(dut.io.masterSide, dut.clockDomain)

      slv.addTrigger(AddressRange(1,100)){bus =>
        if(!bus.WE.toBoolean){
          bus.DAT_MISO #= Random.nextInt(100)
        }

        bus.STALL.randomize()
        //bus.ACK #= true

      }

      dut.clockDomain.waitSampling(10)
      mst.write(List(
        WishboneTransaction(10,10),
        WishboneTransaction(11,20),
        WishboneTransaction(12,20),
        WishboneTransaction(13,20),
        WishboneTransaction(14,30),
        WishboneTransaction(15,40)))

        dut.clockDomain.waitSampling(10)
        mst.write(WishboneTransaction(10,10))

      sleep(100)
    }
  }
}
//class WishboneMasterFactory(wishbone: Wishbone, clockdomain: ClockDomain){
//  val blockCycle = ListBuffer[(Wishbone) => Unit]()
//  var cycleStart : (Wishbone) => Unit = {bus =>}
//
//
//  def addToBlockCycle(transaction: (Wishbone) => Unit) : Unit = {
//    blockCycle += transaction
//  }
//
//  def onCycleStart(transaction: (Wishbone) => Unit) : Unit = {
//    cycleStart = transaction
//  }
//  def write()(callbacks: List[(Wishbone) => Unit]) : Unit@suspendable = classicTransaction(isWrite = true, callbacks)
//  def pipelinedWrite()(callbacks: List[(Wishbone) => Unit]) : Unit@suspendable = pipelinedTransaction(isWrite = true, callbacks)
//  def read()(callbacks: List[(Wishbone) => Unit]) : Unit@suspendable = classicTransaction(isWrite = false, callbacks)
//  def pipelinedRead()(callbacks: List[(Wishbone) => Unit]) : Unit@suspendable = pipelinedTransaction(isWrite = false, callbacks)
//  def classicTransaction(isWrite : Boolean, callbacks: List[(Wishbone) => Unit]) : Unit@suspendable = {
//    def send(callback: (Wishbone) => Unit) : Unit@suspendable = {
//      callback(wishbone)
//      wishbone.STB #= true
//      clockdomain.waitActiveEdgeWhere(wishbone.CYC.toBoolean && wishbone.ACK.toBoolean && wishbone.STB.toBoolean) //TODO: support ERR
//      wishbone.STB #= false
//    }
//
//    def receive(callback: (Wishbone) => Unit) : Unit@suspendable = {
//
//    }
//
//    clockdomain.waitActiveEdge()
//    wishbone.CYC #= true
//    wishbone.WE #= isWrite
//    cycleStart(wishbone)
//    callbacks.init.suspendable.foreach { callback =>
//      send(callback)
//      clockdomain.waitActiveEdge()
//    }
//    send(callbacks.last)
//    wishbone.STB #= false
//    wishbone.CYC #= false
//  }
//
//
//  def pipelinedTransaction(isWrite : Boolean, callbacks: List[(Wishbone) => Unit]) : Unit@suspendable = {
//    clockdomain.waitActiveEdge()
//    wishbone.CYC #= true
//
//    val ackcount = fork{
//      callbacks.suspendable.foreach{ _ => clockdomain.waitActiveEdgeWhere(wishbone.isAcknoledge.toBoolean) }
//    }
//
//    wishbone.WE #= isWrite
//    callbacks.suspendable.foreach { callback =>
//      callback(wishbone)
//      wishbone.STB #= true
//      clockdomain.waitActiveEdgeWhere(wishbone.isStalled.toBoolean)
//    }
//    wishbone.STB #= false
//
//    ackcount.join()
//    wishbone.CYC #= false
//  }
//
//
//
//
//}
//
//class Wishbonetest extends Component {
//  val io = new Bundle{
//    val bus = slave(Wishbone(WishboneConfig(8,8)))
//    val data_out = out Bits(8 bits)
//  }
//
//  val feed = RegNext(io.bus.CYC && io.bus.STB) init(False)
//  io.bus.ACK := feed && io.bus.STB
//  val value = RegNextWhen(io.bus.DAT_MOSI, io.bus.CYC && io.bus.STB) init(0)
//  io.data_out := value
//  io.bus.DAT_MISO := value
//}
//
//object test{
//  def main(args: Array[String]): Unit = {
//    SimConfig(rtl = new Wishbonetest).withWave.doManagedSim{ dut =>
//    //SimConfig.withWave.compile(new Wishbonetest).doSim{ dut =>
//      dut.clockDomain.forkStimulus(period=10)
//      dut.io.bus.CYC #= false
//      dut.io.bus.STB #= false
//      dut.io.bus.ACK #= false
//      val wbm = new WishboneMasterFactory(dut.io.bus, dut.clockDomain)
//      wbm.write(){ List({bus =>
//          bus.ADR #= 42
//          bus.DAT_MOSI #= 100
//          },{bus =>
//
//          bus.ADR #= 21
//          bus.DAT_MOSI #= 50
//          })
//      }
//      sleep(100)
//    }
//  }
//}

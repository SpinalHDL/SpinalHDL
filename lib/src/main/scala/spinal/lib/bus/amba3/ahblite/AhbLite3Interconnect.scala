package spinal.lib.bus.amba3.ahblite

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class AhbLite3CrossbarSlaveConnection(master : AhbLite3/*,priority : Int*/)
case class AhbLite3CrossbarSlaveConfig(mapping : SizeMapping){
  val masters = ArrayBuffer[AhbLite3CrossbarSlaveConnection]()
}
case class AhbLite3CrossbarFactory(AhbLite3Config: AhbLite3Config){
  val slavesConfigs = mutable.HashMap[AhbLite3,AhbLite3CrossbarSlaveConfig]()

  def addSlave(ahb: AhbLite3,mapping: SizeMapping) : this.type = {
    slavesConfigs(ahb) = AhbLite3CrossbarSlaveConfig(mapping)
    this
  }

  def addSlaves(orders : (AhbLite3,SizeMapping)*) : this.type = {
    orders.foreach(order => addSlave(order._1,order._2))
    this
  }

  def addConnection(ahb: AhbLite3,AhbLite3Slave: Iterable[AhbLite3]) : this.type = {
    AhbLite3Slave.foreach(slavesConfigs(_).masters += AhbLite3CrossbarSlaveConnection(ahb))
    this
  }
//  def addConnection(AhbLite3: AhbLite3)(AhbLite3Slave: AhbLite3Slave*) : this.type = {
//    AhbLite3Slave.foreach(slavesConfigs(_).masters += AhbLite3CrossbarSlaveConnection(AhbLite3))
//    this
//  }

  def addConnection(order: (AhbLite3,Iterable[AhbLite3])) : this.type = addConnection(order._1,order._2)

  def addConnections(orders : (AhbLite3,Iterable[AhbLite3])*) : this.type = {
    orders.foreach(addConnection(_))
    this
  }

  def build() = new Area{
    val masters = slavesConfigs.values.map(_.masters.map(_.master)).flatten.toSet
    val masterToDecodedSlave = mutable.HashMap[AhbLite3,Map[AhbLite3,AhbLite3]]()
    val decoders = for(master <- masters) yield new Area{
      val slaves = slavesConfigs.filter{
        case (slave,config) => config.masters.exists(connection => connection.master == master)
      }
      val decoder = AhbLite3Decoder(
        AhbLite3Config = AhbLite3Config,
        decodings = slaves.map(_._2.mapping)
      )

      masterToDecodedSlave(master) = (slaves.map(_._1),decoder.io.outputs).zipped.toMap
      decoder.io.input <> master

      decoder.setPartialName(master,"decoder")
    }

    val arbiters = for((slave,config) <- slavesConfigs) yield new Area{
      val arbiter = AhbLite3Arbiter(
        AhbLite3Config = AhbLite3Config,
        inputsCount = config.masters.length
      )
      for((input,master) <- (arbiter.io.inputs,config.masters).zipped){
        input <> masterToDecodedSlave(master.master)(slave)
      }
      arbiter.io.output <> slave
      arbiter.setPartialName(slave,"arbiter")
    }
  }
}


//case class AhbLite3Crossbar() extends Component{
//  def apply()
//
//}

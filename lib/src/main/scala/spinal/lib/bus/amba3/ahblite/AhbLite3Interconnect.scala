package spinal.lib.bus.amba3.ahblite

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class AhbLite3InterconnectSlaveConnection(master : AhbLite3Master/*,priority : Int*/)
case class AhbLite3InterconnectSlaveConfig(mapping : SizeMapping){
  val masters = ArrayBuffer[AhbLite3InterconnectSlaveConnection]()
}
case class AhbLite3InterconnectFactory(AhbLite3Config: AhbLite3Config){
  val slavesConfigs = mutable.HashMap[AhbLite3Slave,AhbLite3InterconnectSlaveConfig]()

  def addSlave(AhbLite3Slave: AhbLite3Slave,mapping: SizeMapping) : this.type = {
    slavesConfigs(AhbLite3Slave) = AhbLite3InterconnectSlaveConfig(mapping)
    this
  }

  def addSlaves(orders : (AhbLite3Slave,SizeMapping)*) : this.type = {
    orders.foreach(order => addSlave(order._1,order._2))
    this
  }

  def addConnection(AhbLite3Master: AhbLite3Master,AhbLite3Slave: Iterable[AhbLite3Slave]) : this.type = {
    AhbLite3Slave.foreach(slavesConfigs(_).masters += AhbLite3InterconnectSlaveConnection(AhbLite3Master))
    this
  }
//  def addConnection(AhbLite3Master: AhbLite3Master)(AhbLite3Slave: AhbLite3Slave*) : this.type = {
//    AhbLite3Slave.foreach(slavesConfigs(_).masters += AhbLite3InterconnectSlaveConnection(AhbLite3Master))
//    this
//  }

  def addConnection(order: (AhbLite3Master,Iterable[AhbLite3Slave])) : this.type = addConnection(order._1,order._2)

  def addConnections(orders : (AhbLite3Master,Iterable[AhbLite3Slave])*) : this.type = {
    orders.foreach(addConnection(_))
    this
  }

  def build() = new Area{
    val masters = slavesConfigs.values.map(_.masters.map(_.master)).flatten.toSet
    val masterToDecodedSlave = mutable.HashMap[AhbLite3Master,Map[AhbLite3Slave,AhbLite3Slave]]()
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


//case class AhbLite3Interconnect() extends Component{
//  def apply()
//
//}

package spinal.lib.bus.amba3.ahb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahb._
import spinal.lib.bus.misc.BaseSize

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Ahb3InterconnectSlaveConnection(master : Ahb3Master/*,priority : Int*/)
case class Ahb3InterconnectSlaveConfig(mapping : BaseSize){
  val masters = ArrayBuffer[Ahb3InterconnectSlaveConnection]()
}
case class Ahb3InterconnectFactory(ahb3Config: Ahb3Config){
  val slavesConfigs = mutable.HashMap[Ahb3Slave,Ahb3InterconnectSlaveConfig]()

  def addSlave(ahb3Slave: Ahb3Slave,mapping: BaseSize) : this.type = {
    slavesConfigs(ahb3Slave) = Ahb3InterconnectSlaveConfig(mapping)
    this
  }

  def addConnection(ahb3Master: Ahb3Master,ahb3Slave: Ahb3Slave*) : this.type = {
    ahb3Slave.foreach(slavesConfigs(_).masters += Ahb3InterconnectSlaveConnection(ahb3Master))
    this
  }

  def addConnection(connection: (Ahb3Master,Ahb3Slave)) : this.type = addConnection(connection._1,connection._2)

  def build() = new Area{
    val masters = slavesConfigs.values.map(_.masters.map(_.master)).flatten.toSet
    val masterToDecodedSlave = mutable.HashMap[Ahb3Master,Map[Ahb3Slave,Ahb3Slave]]()
    val decoders = for(master <- masters) yield new Area{
      val slaves = slavesConfigs.filter{
        case (slave,config) => config.masters.exists(connection => connection.master == master)
      }
      val decoder = Ahb3Decoder(
        ahb3Config = ahb3Config,
        decodings = slaves.map(_._2.mapping)
      )

      masterToDecodedSlave(master) = (slaves.map(_._1),decoder.io.outputs).zipped.toMap
      decoder.io.input <> master
    }

    val arbiters = for((slave,config) <- slavesConfigs) yield new Area{
      val arbiter = Ahb3Arbiter(
        ahb3Config = ahb3Config,
        inputsCount = config.masters.length
      )
      for((input,master) <- (arbiter.io.inputs,config.masters).zipped){
        input <> masterToDecodedSlave(master.master)(slave)
      }
      arbiter.io.output <> slave
    }
  }
}


//case class Ahb3Interconnect() extends Component{
//  def apply()
//
//}

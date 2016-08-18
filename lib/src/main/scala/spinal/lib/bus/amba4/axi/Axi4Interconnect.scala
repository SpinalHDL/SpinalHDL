package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Axi4InterconnectSlaveConnection(master : Axi4Bus/*,priority : Int*/)
case class Axi4InterconnectSlaveConfig(mapping : SizeMapping){
  val connections = ArrayBuffer[Axi4InterconnectSlaveConnection]()
}


case class Axi4InterconnectFactory(/*axiConfig: Axi4Config*/){
  val slavesConfigs = mutable.HashMap[Axi4Bus,Axi4InterconnectSlaveConfig]()
  val axi4SlaveToReadWriteOnly = mutable.HashMap[Axi4,Seq[Axi4Bus]]()
  val masters = ArrayBuffer[Axi4Bus]()
  def addSlave(axi: Axi4Bus,mapping: SizeMapping) : this.type = {
    axi match {
      case axi: Axi4 => {
        val readOnly = Axi4ReadOnly(axi.config)
        val writeOnly = Axi4WriteOnly(axi.config)
        readOnly >> axi
        writeOnly >> axi
        axi4SlaveToReadWriteOnly(axi) = readOnly :: writeOnly :: Nil
        addSlave(readOnly,mapping)
        addSlave(writeOnly,mapping)
      }
      case _ => {
        slavesConfigs(axi) = Axi4InterconnectSlaveConfig(mapping)
      }
    }
    this
  }

  def addSlaves(orders : (Axi4Bus,SizeMapping)*) : this.type = {
    orders.foreach(order => addSlave(order._1,order._2))
    this
  }

  def addConnection(axi: Axi4Bus,slaves: Iterable[Axi4Bus]) : this.type = {
    val translatedSlaves = slaves.map(_ match{
      case that : Axi4 => axi4SlaveToReadWriteOnly(that)
      case that : Axi4Bus => that :: Nil
    }).flatten
    axi match {
      case axi : Axi4 => {
        addConnection(axi.toReadOnly(),translatedSlaves.filter(!_.isInstanceOf[Axi4WriteOnly]))
        addConnection(axi.toWriteOnly(),translatedSlaves.filter(!_.isInstanceOf[Axi4ReadOnly]))
      }
      case axi : Axi4WriteOnly => {
        translatedSlaves.filter(!_.isInstanceOf[Axi4ReadOnly]).foreach(slavesConfigs(_).connections += Axi4InterconnectSlaveConnection(axi))
        masters += axi
      }
      case axi : Axi4ReadOnly => {
        translatedSlaves.filter(!_.isInstanceOf[Axi4WriteOnly]).foreach(slavesConfigs(_).connections += Axi4InterconnectSlaveConnection(axi))
        masters += axi
      }
      case axi : Axi4Shared => {
        translatedSlaves.foreach(slavesConfigs(_).connections += Axi4InterconnectSlaveConnection(axi))
        masters += axi
      }
    }
    this
  }


  def addConnection(order: (Axi4Bus,Iterable[Axi4Bus])) : this.type = addConnection(order._1,order._2)

  def addConnections(orders : (Axi4Bus,Iterable[Axi4Bus])*) : this.type = {
    orders.foreach(addConnection(_))
    this
  }


  def build(): Unit ={
    val masterToDecodedSlave = mutable.HashMap[Axi4Bus,Map[Axi4Bus,Axi4Bus]]()

    val decoders = for(master <- masters) yield master match {
      case master : Axi4ReadOnly => new Area{
        val slaves = slavesConfigs.filter{
          case (slave,config) => config.connections.exists(connection => connection.master == master)
        }
        val decoder = Axi4ReadOnlyDecoder(
          axiConfig = master.config,
          decodings = slaves.map(_._2.mapping)
        )
  
        masterToDecodedSlave(master) = (slaves.map(_._1),decoder.io.outputs).zipped.toMap
        decoder.io.input << master
  
        decoder.setPartialName(master,"readDecoder")
      }
      case master : Axi4WriteOnly => new Area{
        val slaves = slavesConfigs.filter{
          case (slave,config) => config.connections.exists(connection => connection.master == master)
        }
        val decoder = Axi4WriteOnlyDecoder(
          axiConfig = master.config,
          decodings = slaves.map(_._2.mapping)
        )

        masterToDecodedSlave(master) = (slaves.map(_._1),decoder.io.outputs).zipped.toMap
        decoder.io.input << master

        decoder.setPartialName(master,"writeDecoder")
      }
      case master : Axi4Shared => new Area{
        val slaves = slavesConfigs.filter{
          case (slave,config) => config.connections.exists(connection => connection.master == master)
        }
        val readOnlySlaves = slaves.filter(_._1.isInstanceOf[Axi4ReadOnly])
        val writeOnlySlaves = slaves.filter(_._1.isInstanceOf[Axi4WriteOnly])
        val sharedSlaves = slaves.filter(_._1.isInstanceOf[Axi4Shared])
        val decoder = Axi4SharedDecoder(
          axiConfig = master.config,
          readDecodings = readOnlySlaves.map(_._2.mapping),
          writeDecodings = writeOnlySlaves.map(_._2.mapping),
          sharedDecodings = sharedSlaves.map(_._2.mapping)
        )

        masterToDecodedSlave(master) = (
          readOnlySlaves.map(_._1) ++ writeOnlySlaves.map(_._1) ++ sharedSlaves.map(_._1)
            -> List(decoder.io.readOutputs.toSeq , decoder.io.writeOutputs.toSeq , decoder.io.sharedOutputs.toSeq).flatten
        ).zipped.toMap


        decoder.io.input << master
      }
    }





    val arbiters = for((slave,config) <- slavesConfigs.toSeq.sortBy(_._1.asInstanceOf[Bundle].getInstanceCounter)) yield slave match {
      case slave : Axi4ReadOnly => new Area{
        val readConnections = config.connections
        readConnections.size match {
          case 0 => PendingError(s"$slave has no master}")
          case 1 => slave << readConnections.head.master.asInstanceOf[Axi4ReadOnly]
          case _ => new Area {
            val arbiter = Axi4ReadOnlyArbiter(
              outputConfig = slave.config,
              inputsCount = readConnections.length
            )
            for ((input, master) <- (arbiter.io.inputs, readConnections).zipped) {
              if(!masterToDecodedSlave(master.master)(slave).isInstanceOf[Axi4ReadOnly])
                println("???")
              input << masterToDecodedSlave(master.master)(slave).asInstanceOf[Axi4ReadOnly]
            }
            arbiter.io.output >> slave
          }
        }
      }
      case slave : Axi4WriteOnly => {
        val writeConnections = config.connections
        config.connections.size match {
          case 0 => PendingError(s"$slave has no master}")
          case 1 => slave << writeConnections.head.master.asInstanceOf[Axi4WriteOnly]
          case _ => new Area {
            val arbiter = Axi4WriteOnlyArbiter(
              outputConfig = slave.config,
              inputsCount = writeConnections.length,
              routeBufferSize = 4
            )
            for ((input, master) <- (arbiter.io.inputs, writeConnections).zipped) {
              input << masterToDecodedSlave(master.master)(slave).asInstanceOf[Axi4WriteOnly]
            }
            arbiter.io.output >> slave
          }
        }
      }
      case slave : Axi4Shared => {
        val connections = config.connections
        val readConnections = connections.filter(_.master.isInstanceOf[Axi4ReadOnly])
        val writeConnections = connections.filter(_.master.isInstanceOf[Axi4WriteOnly])
        val sharedConnections = connections.filter(_.master.isInstanceOf[Axi4Shared])
        if(readConnections.size + sharedConnections.size == 0){
          PendingError(s"$slave has no master able to read it}")
          return
        }

        if(writeConnections.size + sharedConnections.size == 0){
          PendingError(s"$slave has no master able to write it}")
          return
        }

        if(readConnections.size == 0 && writeConnections.size == 0 && sharedConnections.size == 0){
          slave << sharedConnections.head.master.asInstanceOf[Axi4Shared]
        }else{
          new Area {
            val arbiter = Axi4SharedArbiter(
              outputConfig = slave.config,
              readInputsCount = readConnections.size,
              writeInputsCount = writeConnections.size,
              sharedInputsCount = sharedConnections.size,
              routeBufferSize = 4
            )

            for ((input, master) <- (arbiter.io.readInputs, readConnections).zipped) {
              input << masterToDecodedSlave(master.master)(slave).asInstanceOf[Axi4ReadOnly]
            }
            for ((input, master) <- (arbiter.io.writeInputs, writeConnections).zipped) {
              input << masterToDecodedSlave(master.master)(slave).asInstanceOf[Axi4WriteOnly]
            }
            for ((input, master) <- (arbiter.io.sharedInputs, sharedConnections).zipped) {
              input << masterToDecodedSlave(master.master)(slave).asInstanceOf[Axi4Shared]
            }
            arbiter.io.output >> slave
          }
        }
      }
    }




  }
}

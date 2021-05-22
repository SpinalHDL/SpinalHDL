package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Axi4CrossbarSlaveConnection(master : Axi4Bus/*,priority : Int*/)
case class Axi4CrossbarSlaveConfig(mapping : SizeMapping){
  val connections = ArrayBuffer[Axi4CrossbarSlaveConnection]()
}

/*
object Axi4CrossbarFactory{
    def defaultDecoderToArbiterConnection(masterSlave : (Axi4Bus,Axi4Bus), decoderArbiter : (Axi4Bus,Axi4Bus)) : Axi4Bus = bus match {

    }
}*/

case class Axi4CrossbarFactory(/*decoderToArbiterConnection : (Axi4Bus, Axi4Bus) => Axi4Bus = Axi4CrossbarFactory.defaultDecoderToArbiterConnection*/){
  val slavesConfigs = mutable.LinkedHashMap[Axi4Bus,Axi4CrossbarSlaveConfig]()
  val axi4SlaveToReadWriteOnly = mutable.HashMap[Axi4,Seq[Axi4Bus]]()
  val sharedBridger = mutable.HashMap[Axi4Shared,(Axi4Shared,Axi4Shared) => Unit]()
  val readOnlyBridger = mutable.HashMap[Axi4ReadOnly,(Axi4ReadOnly,Axi4ReadOnly) => Unit]()
  val writeOnlyBridger = mutable.HashMap[Axi4WriteOnly,(Axi4WriteOnly,Axi4WriteOnly) => Unit]()
  val masters = ArrayBuffer[Axi4Bus]()
  var lowLatency = false

  def decoderToArbiterLink(bus : Axi4ReadOnly) = if(!lowLatency) bus.arValidPipe() else bus
  def decoderToArbiterLink(bus : Axi4WriteOnly) = if(!lowLatency) bus.awValidPipe() else bus
  def decoderToArbiterLink(bus : Axi4Shared) = if(!lowLatency) bus.arwValidPipe() else bus

  def addSlave(axi: Axi4Bus,mapping: SizeMapping) : this.type = {
    axi match {
      case axi: Axi4 => {
        val readOnly = Axi4ReadOnly(axi.config).setCompositeName(axi, "readOnly", true)
        val writeOnly = Axi4WriteOnly(axi.config).setCompositeName(axi, "writeOnly", true)
        readOnly >> axi
        writeOnly >> axi
        axi4SlaveToReadWriteOnly(axi) = readOnly :: writeOnly :: Nil
        addSlave(readOnly,mapping)
        addSlave(writeOnly,mapping)
      }
      case _ => {
        slavesConfigs(axi) = Axi4CrossbarSlaveConfig(mapping)
      }
    }
    this
  }

  def addSlaves(orders : (Axi4Bus,SizeMapping)*) : this.type = {
    orders.foreach(order => addSlave(order._1,order._2))
    this
  }

  def addConnection(axi: Axi4Bus,slaves: Seq[Axi4Bus]) : this.type = {
    val translatedSlaves = slaves.map(_ match{
      case that : Axi4 => axi4SlaveToReadWriteOnly(that)
      case that : Axi4Bus => that :: Nil
    }).flatten
    axi match {
      case axi : Axi4 => {
        addConnection(axi.toReadOnly().setCompositeName(axi, "readOnly", true),translatedSlaves.filter(!_.isInstanceOf[Axi4WriteOnly]))
        addConnection(axi.toWriteOnly().setCompositeName(axi, "writeOnly", true),translatedSlaves.filter(!_.isInstanceOf[Axi4ReadOnly]))
      }
      case axi : Axi4WriteOnly => {
        translatedSlaves.filter(!_.isInstanceOf[Axi4ReadOnly]).foreach(slavesConfigs(_).connections += Axi4CrossbarSlaveConnection(axi))
        masters += axi
      }
      case axi : Axi4ReadOnly => {
        translatedSlaves.filter(!_.isInstanceOf[Axi4WriteOnly]).foreach(slavesConfigs(_).connections += Axi4CrossbarSlaveConnection(axi))
        masters += axi
      }
      case axi : Axi4Shared => {
        translatedSlaves.foreach(slavesConfigs(_).connections += Axi4CrossbarSlaveConnection(axi))
        masters += axi
      }
    }
    this
  }


  def addConnection(order: (Axi4Bus,Seq[Axi4Bus])) : this.type = addConnection(order._1,order._2)

  def addConnections(orders : (Axi4Bus,Seq[Axi4Bus])*) : this.type = {
    orders.foreach(addConnection(_))
    this
  }

  def addPipelining(axi : Axi4Shared)(bridger : (Axi4Shared,Axi4Shared) => Unit): this.type ={
    this.sharedBridger(axi) = bridger
    this
  }
  def addPipelining(axi : Axi4ReadOnly)(bridger : (Axi4ReadOnly,Axi4ReadOnly) => Unit): this.type ={
    this.readOnlyBridger(axi) = bridger
    this
  }
  def addPipelining(axi : Axi4WriteOnly)(bridger : (Axi4WriteOnly,Axi4WriteOnly) => Unit): this.type ={
    this.writeOnlyBridger(axi) = bridger
    this
  }

  def addPipelining(axi : Axi4)(ro : (Axi4ReadOnly,Axi4ReadOnly) => Unit)(wo : (Axi4WriteOnly,Axi4WriteOnly) => Unit): this.type ={
    val b = axi4SlaveToReadWriteOnly(axi)
    val rAxi = b(0).asInstanceOf[Axi4ReadOnly]
    val wAxi = b(1).asInstanceOf[Axi4WriteOnly]
    addPipelining(rAxi)(ro)
    addPipelining(wAxi)(wo)
    this
  }

  def build(): Unit ={
    val masterToDecodedSlave = mutable.HashMap[Axi4Bus,Map[Axi4Bus,Axi4Bus]]()

    def applyName(bus : Bundle,name : String, onThat : Nameable) : Unit = {
      if(bus.component == Component.current)
        onThat.setCompositeName(bus,name)
      else if(bus.isNamed)
        onThat.setCompositeName(bus.component,bus.getName() + "_" + name)
    }

    val portMapMasters = mutable.HashMap[Axi4Bus, Axi4Bus]()
    val portMapSlaves = mutable.HashMap[Axi4Bus, Axi4Bus]()

    new Component {
      definitionName = "Axi4Crossbar"
      setName("Axi4Crossbar")

      val io = new Bundle {
        portMapMasters ++= (for ((master, id) <- masters.zipWithIndex) yield master match {
          case master: Axi4ReadOnly => {
            val port = slave(Axi4ReadOnly(master.config))
            port.setName("axi_m_ro_%d".format(id))
            (master, port)
          }
          case master: Axi4WriteOnly => {
            val port = slave(Axi4WriteOnly(master.config))
            port.setName("axi_m_wo_%d".format(id))
            (master, port)
          }
          case master: Axi4Shared => {
            val port = slave(Axi4Shared(master.config))
            port.setName("axi_m_%d".format(id))
            (master, port)
          }
        })

        portMapSlaves ++= (for (((slave, _), id) <- slavesConfigs.toSeq.sortBy(_._1.asInstanceOf[Bundle].getInstanceCounter).zipWithIndex) yield slave match {
          case slave: Axi4ReadOnly => {
            val port = master(Axi4ReadOnly(slave.config))
            port.setName("axi_s_ro_%d".format(id))
            (slave, port)
          }
          case slave: Axi4WriteOnly => {
            val port = master(Axi4WriteOnly(slave.config))
            port.setName("axi_s_wo_%d".format(id))
            (slave, port)
          }
          case slave: Axi4Shared => {
            val port = master(Axi4Shared(slave.config))
            port.setName("axi_s_%d".format(id))
            (slave, port)
          }
        })
      }

      val decoders = for (master <- masters) yield master match {
        case master: Axi4ReadOnly => new Area {
          val slaves = slavesConfigs.filter {
            case (slave, config) => config.connections.exists(connection => connection.master == master)
          }.toSeq

          val decoder = Axi4ReadOnlyDecoder(
            axiConfig = master.config,
            decodings = slaves.map(_._2.mapping)
          )
          val masterPort = portMapMasters(master).asInstanceOf[Axi4ReadOnly]
          applyName(masterPort, "decoder", decoder)
          masterToDecodedSlave(master) = (slaves.map(_._1), decoder.io.outputs.map(decoderToArbiterLink)).zipped.toMap
          readOnlyBridger.getOrElse[(Axi4ReadOnly, Axi4ReadOnly) => Unit](masterPort, _ >> _).apply(masterPort, decoder.io.input)
        }
        case master: Axi4WriteOnly => new Area {
          val slaves = slavesConfigs.filter {
            case (slave, config) => config.connections.exists(connection => connection.master == master)
          }.toSeq
          val decoder = Axi4WriteOnlyDecoder(
            axiConfig = master.config,
            decodings = slaves.map(_._2.mapping),
            lowLatency = lowLatency
          )
          val masterPort = portMapMasters(master).asInstanceOf[Axi4WriteOnly]
          applyName(masterPort, "decoder", decoder)
          masterToDecodedSlave(master) = (slaves.map(_._1), decoder.io.outputs.map(decoderToArbiterLink)).zipped.toMap
          writeOnlyBridger.getOrElse[(Axi4WriteOnly, Axi4WriteOnly) => Unit](masterPort, _ >> _).apply(masterPort, decoder.io.input)
        }
        case master: Axi4Shared => new Area {
          val slaves = slavesConfigs.filter {
            case (slave, config) => config.connections.exists(connection => connection.master == master)
          }.toSeq
          val readOnlySlaves = slaves.filter(_._1.isInstanceOf[Axi4ReadOnly])
          val writeOnlySlaves = slaves.filter(_._1.isInstanceOf[Axi4WriteOnly])
          val sharedSlaves = slaves.filter(_._1.isInstanceOf[Axi4Shared])
          val decoder = Axi4SharedDecoder(
            axiConfig = master.config,
            readDecodings = readOnlySlaves.map(_._2.mapping),
            writeDecodings = writeOnlySlaves.map(_._2.mapping),
            sharedDecodings = sharedSlaves.map(_._2.mapping),
            lowLatency = lowLatency
          )
          val masterPort = portMapMasters(master).asInstanceOf[Axi4Shared]
          applyName(masterPort, "decoder", decoder)
          masterToDecodedSlave(master) = (
            readOnlySlaves.map(_._1) ++ writeOnlySlaves.map(_._1) ++ sharedSlaves.map(_._1)
              -> List(decoder.io.readOutputs.map(decoderToArbiterLink).toSeq, decoder.io.writeOutputs.map(decoderToArbiterLink).toSeq, decoder.io.sharedOutputs.map(decoderToArbiterLink).toSeq).flatten
            ).zipped.toMap

          sharedBridger.getOrElse[(Axi4Shared, Axi4Shared) => Unit](masterPort, _ >> _).apply(masterPort, decoder.io.input)
        }
      }


      //    for((master,pair) <- masterToDecodedSlave){
      //      val newList = for((slave, interconnect) <- pair) yield{
      //        val pipeplined = cloneOf(interconnect)
      //        (slave -> interconnect)
      //      }
      //      masterToDecodedSlave(master) = newList.toMap
      //    }


      val arbiters = for ((slave, config) <- slavesConfigs.toSeq.sortBy(_._1.asInstanceOf[Bundle].getInstanceCounter)) yield slave match {
        case slave: Axi4ReadOnly => new Area {
          val readConnections = config.connections
          val slavePort = portMapSlaves(slave).asInstanceOf[Axi4ReadOnly]
          readConnections.size match {
            case 0 => PendingError(s"$slave has no master}")
            case 1 if readConnections.head.master.isInstanceOf[Axi4ReadOnly] => readConnections.head.master match {
              case m: Axi4ReadOnly => slavePort << masterToDecodedSlave(m)(slave).asInstanceOf[Axi4ReadOnly]
              //            case m : Axi4Shared => slave << m.toAxi4ReadOnly()
            }
            case _ => new Area {
              val arbiter = Axi4ReadOnlyArbiter(
                outputConfig = slave.config,
                inputsCount = readConnections.length
              )
              applyName(slavePort, "arbiter", arbiter)
              for ((input, master) <- (arbiter.io.inputs, readConnections).zipped) {
                if (!masterToDecodedSlave(master.master)(slave).isInstanceOf[Axi4ReadOnly])
                  println("???")
                input << masterToDecodedSlave(master.master)(slave).asInstanceOf[Axi4ReadOnly]
              }
              readOnlyBridger.getOrElse[(Axi4ReadOnly, Axi4ReadOnly) => Unit](slavePort, _ >> _).apply(arbiter.io.output, slavePort)
            }
          }
        }
        case slave: Axi4WriteOnly => {
          val writeConnections = config.connections
          val slavePort = portMapSlaves(slave).asInstanceOf[Axi4WriteOnly]
          config.connections.size match {
            case 0 => PendingError(s"$slave has no master}")
            case 1 if writeConnections.head.master.isInstanceOf[Axi4WriteOnly] => writeConnections.head.master match {
              case m: Axi4WriteOnly => slavePort << masterToDecodedSlave(m)(slave).asInstanceOf[Axi4WriteOnly]
              //            case m : Axi4Shared => slave << m.toAxi4WriteOnly()
            }
            case _ => new Area {
              val arbiter = Axi4WriteOnlyArbiter(
                outputConfig = slave.config,
                inputsCount = writeConnections.length,
                routeBufferSize = 4
              )
              applyName(slavePort, "arbiter", arbiter)
              for ((input, master) <- (arbiter.io.inputs, writeConnections).zipped) {
                input << masterToDecodedSlave(master.master)(slave).asInstanceOf[Axi4WriteOnly]
              }
              writeOnlyBridger.getOrElse[(Axi4WriteOnly, Axi4WriteOnly) => Unit](slavePort, _ >> _).apply(arbiter.io.output, slavePort)
            }
          }
        }
        case slave: Axi4Shared => {
          val connections = config.connections
          val readConnections = connections.filter(_.master.isInstanceOf[Axi4ReadOnly])
          val writeConnections = connections.filter(_.master.isInstanceOf[Axi4WriteOnly])
          val sharedConnections = connections.filter(_.master.isInstanceOf[Axi4Shared])
          val slavePort = portMapSlaves(slave).asInstanceOf[Axi4Shared]
          //        if(readConnections.size + sharedConnections.size == 0){
          //          PendingError(s"$slave has no master able to read it}")
          //          return
          //        }

          //        if(writeConnections.size + sharedConnections.size == 0){
          //          PendingError(s"$slave has no master able to write it}")
          //          return
          //        }

          if (readConnections.size == 0 && writeConnections.size == 0 && sharedConnections.size == 0) {
            slavePort << sharedConnections.head.master.asInstanceOf[Axi4Shared]
          } else {
            new Area {
              val arbiter = Axi4SharedArbiter(
                outputConfig = slave.config,
                readInputsCount = readConnections.size,
                writeInputsCount = writeConnections.size,
                sharedInputsCount = sharedConnections.size,
                routeBufferSize = 4
              )
              applyName(slavePort, "arbiter", arbiter)

              for ((input, master) <- (arbiter.io.readInputs, readConnections).zipped) {
                input << masterToDecodedSlave(master.master)(slave).asInstanceOf[Axi4ReadOnly]
              }
              for ((input, master) <- (arbiter.io.writeInputs, writeConnections).zipped) {
                input << masterToDecodedSlave(master.master)(slave).asInstanceOf[Axi4WriteOnly]
              }
              for ((input, master) <- (arbiter.io.sharedInputs, sharedConnections).zipped) {
                input << masterToDecodedSlave(master.master)(slave).asInstanceOf[Axi4Shared]
              }
              sharedBridger.getOrElse[(Axi4Shared, Axi4Shared) => Unit](slavePort, _ >> _).apply(arbiter.io.output, slavePort)
            }
          }
        }
      }
    }

    for ((master, port) <- portMapMasters) master match {
      case master: Axi4ReadOnly  => master >> port.asInstanceOf[Axi4ReadOnly]
      case master: Axi4WriteOnly => master >> port.asInstanceOf[Axi4WriteOnly]
      case master: Axi4Shared    => master >> port.asInstanceOf[Axi4Shared]
    }

    for ((slave, port) <- portMapSlaves) slave match {
      case slave: Axi4ReadOnly  => slave << port.asInstanceOf[Axi4ReadOnly]
      case slave: Axi4WriteOnly => slave << port.asInstanceOf[Axi4WriteOnly]
      case slave: Axi4Shared    => slave << port.asInstanceOf[Axi4Shared]
    }
  }
}

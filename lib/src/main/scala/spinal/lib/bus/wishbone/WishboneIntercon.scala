package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib.bus.misc._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object WishboneConnectors{
  def direct(m : Wishbone, s : Wishbone) : Unit = m >> s
}

case class WishboneInterconFactory(){
  case class MasterModel(var connector : (Wishbone,Wishbone) => Unit = WishboneConnectors.direct)
  case class SlaveModel(mapping : SizeMapping, var connector : (Wishbone,Wishbone) => Unit = WishboneConnectors.direct, var transactionLock : Boolean = true)
  case class ConnectionModel(m : Wishbone, s : Wishbone, var connector : (Wishbone,Wishbone) => Unit = WishboneConnectors.direct)


  val masters = mutable.LinkedHashMap[Wishbone, MasterModel]()
  val slaves = mutable.LinkedHashMap[Wishbone, SlaveModel]()
  val connections = ArrayBuffer[ConnectionModel]()

  /** Modify a connection
    * @param bus the bus
    */
  def setConnector(bus : Wishbone)( connector : (Wishbone,Wishbone) => Unit): Unit = (masters.get(bus), slaves.get(bus)) match {
    case (Some(m), _) =>    m.connector = connector
    case (None, Some(s)) => s.connector = connector
    case _ => ???
  }

  /** Modify a connection
    * @param m the master where the conenction start
    * @param s the slave that is connected to the master
    * @example{{{
    * interconnect.setConnector(dBus, slowBus){(m,s) =>
    *   m.cmd.halfPipe() >> s.cmd
    *   m.rsp            << s.rsp
    * }
    * }}}
    */
  def setConnector(m : Wishbone, s : Wishbone)(connector : (Wishbone,Wishbone) => Unit): Unit = connections.find(e => e.m == m && e.s == s) match {
    case Some(c) => c.connector = connector
    case _ => ???
  }

  /** add a slave to the intercon, and specify its address space
    * @param bus the slave device
    * @param mapping the address defined via [[spinal.lib.bus.misc.SizeMapping]]
    */
  def addSlave(bus: Wishbone,mapping: SizeMapping) : this.type = {
    slaves(bus) = SlaveModel(mapping)
    this
  }

  /** add multiple slave to the intercon, and specify their address space
    * @param orders
    * @example{{{
    * interconnect.addSlaves(
    *   ram.io.buses(0)     -> SizeMapping(0x00000,  64 KiB),
    *   ram.io.buses(1)     -> SizeMapping(0x00000,  64 KiB),
    *   peripherals.io.bus  -> SizeMapping(0x70000,  64 Byte),
    *   flashXip.io.bus     -> SizeMapping(0x80000, 512 KiB),
    *   slowBus             -> DefaultMapping
    * )
    * }}}
    */
  def addSlaves(orders : (Wishbone,SizeMapping)*) : this.type = {
    orders.foreach(order => addSlave(order._1,order._2))
    this
  }

  /** Queue a master to be connected
    * @param bus a master wishbone device
    * @param accesses a list of slaves device to connect the master with
    */
  def addMaster(bus : Wishbone, accesses : Seq[Wishbone]) : this.type = {
    masters(bus) = MasterModel()
    for(s <- accesses) connections += ConnectionModel(bus, s)
    this
  }

  /** Queue a master to be connected
    * @param specs a tuple of master wishbone device and a list of slaves device to connect the master with
    * @example{{{
    * interconnect.addMasters(
    *   dBus   -> List(ram.io.buses(0), slowBus),
    *   iBus   -> List(ram.io.buses(1), slowBus),
    *   slowBus-> List(peripherals.io.bus, flashXip.io.bus)
    * )
    * }}}
    */
  def addMasters(specs : (Wishbone,Seq[Wishbone])*) : this.type = {
    specs.foreach(spec => addMaster(spec._1,spec._2))
    this
  }

  def build(): Unit ={
    val connectionsInput  = mutable.HashMap[ConnectionModel,Wishbone]()
    val connectionsOutput = mutable.HashMap[ConnectionModel,Wishbone]()

    for((bus, model) <- masters){
      val busConnections = connections.filter(_.m == bus)
      val busSlaves = busConnections.map(c => slaves(c.s))
      val decoder = new WishboneDecoder(bus.config, busSlaves.map(_.mapping))
      decoder.setCompositeName(bus, "decoder")
      model.connector(bus, decoder.io.input)
      for((connection, decoderOutput) <- (busConnections, decoder.io.outputs).zipped) {
        connectionsInput(connection) = decoderOutput
      }
    }

    for((bus, model) <- slaves){
      val busConnections = connections.filter(_.s == bus)
      val busMasters = busConnections.map(c => masters(c.m))
      val arbiter = new WishboneArbiter(bus.config, busMasters.size)
      arbiter.setCompositeName(bus, "arbiter")
      model.connector(arbiter.io.output, bus)
      for((connection, arbiterInput) <- (busConnections, arbiter.io.inputs).zipped) {
        connectionsOutput(connection) = arbiterInput
      }
    }

    for(connection <- connections){
      val m = connectionsInput(connection)
      val s = connectionsOutput(connection)
      if(m.config == s.config) {
        connection.connector(m, s)
      }else{
        val tmp = cloneOf(s)
        m >> tmp //Adapte the bus kind.
        connection.connector(tmp,s)
      }
    }
  }
  //Will make SpinalHDL calling the build function at the end of the current component elaboration
  Component.current.addPrePopTask(build)
}

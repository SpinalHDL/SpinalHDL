//package spinal.lib.bus.bmb
//
//import spinal.core._
//import spinal.lib._
//import spinal.lib.bus.misc.AddressMapping
//
//import scala.collection.mutable
//import scala.collection.mutable.ArrayBuffer
//
//abstract class AbstractMemoryInterconnect[T](){
//  case class MasterModel(var connector : (T,T) => Unit = defaultConnector)
//  case class SlaveModel(mapping : AddressMapping, var connector : (T,T) => Unit = defaultConnector)
//  case class ConnectionModel(m : T, s : T, var connector : (T,T) => Unit = defaultConnector)
//
//  def defaultConnector(m : T, s : T) : Unit
//  val masters = mutable.LinkedHashMap[T, MasterModel]()
//  val slaves = mutable.LinkedHashMap[T, SlaveModel]()
//  val connections = ArrayBuffer[ConnectionModel]()
//
//  def setConnector(bus : T)( connector : (T,T) => Unit): Unit = (masters.get(bus), slaves.get(bus)) match {
//    case (Some(m), _) =>    m.connector = connector
//    case (None, Some(s)) => s.connector = connector
//    case _ => ???
//  }
//
//  def setConnector(m : T, s : T)(connector : (T,T) => Unit): Unit = connections.find(e => e.m == m && e.s == s) match {
//    case Some(c) => c.connector = connector
//    case _ => ???
//  }
//
//  def addSlave(bus: T,mapping: AddressMapping) : this.type = {
//    slaves(bus) = SlaveModel(mapping)
//    this
//  }
//
//  def addSlaves(orders : (T,AddressMapping)*) : this.type = {
//    orders.foreach(order => addSlave(order._1,order._2))
//    this
//  }
//
//
//
//  def addMaster(bus : T, accesses : Seq[T] = Nil) : this.type = {
//    masters(bus) = MasterModel()
//    for(s <- accesses) connections += ConnectionModel(bus, s)
//    this
//  }
//
//  def addMasters(specs : (T,Seq[T])*) : this.type = {
//    specs.foreach(spec => addMaster(spec._1,spec._2))
//    this
//  }
//
//  def addConnection(m : T, s : T) : this.type = {
//    connections += ConnectionModel(m, s)
//    this
//  }
//
//  def build(): Unit
//
//  //Will make SpinalHDL calling the build function at the end of the current component elaboration
//  Component.current.addPrePopTask(build)
//}
//
//
//class BmbInterconnect extends AbstractMemoryInterconnect [Bmb]{
//  var arbitrationPendingRspMaxDefault = 3
//
//  override def defaultConnector(m: Bmb, s: Bmb): Unit = m >> s
//
//  override def build(): Unit ={
//    val connectionsInput  = mutable.HashMap[ConnectionModel,Bmb]()
//    val connectionsOutput = mutable.HashMap[ConnectionModel,Bmb]()
//
//    //Generate decoders
//    for((bus, model) <- masters){
//      val busConnections = connections.filter(_.m == bus)
//      val busSlaves = busConnections.map(c => slaves(c.s))
//      val decoder = new BmbDecoder(bus.p, busSlaves.map(_.mapping))
//      decoder.setCompositeName(bus, "decoder")
//      model.connector(bus, decoder.io.input)
//      for((connection, decoderOutput) <- (busConnections, decoder.io.outputs).zipped) {
//        connectionsInput(connection) = decoderOutput
//      }
//    }
//
//    //Generate arbiters
//    for((bus, model) <- slaves){
//      val busConnections = connections.filter(_.s == bus)
//      val busMasters = busConnections.map(c => masters(c.m))
//      val arbiter = new BmbArbiter(bus.p, busMasters.size, arbitrationPendingRspMaxDefault, false)
//      arbiter.setCompositeName(bus, "arbiter")
//      model.connector(arbiter.io.output, bus)
//      for((connection, arbiterInput) <- (busConnections, arbiter.io.inputs).zipped) {
//        connectionsOutput(connection) = arbiterInput
//      }
//    }
//
//    //Apply connectors
//    for(connection <- connections){
//      val m = connectionsInput(connection)
//      val s = connectionsOutput(connection)
//      if(m.p == s.p) {
//        connection.connector(m, s)
//      }else{
//        val tmp = cloneOf(s)
//        m >> tmp //Adapte the bus kind.
//        connection.connector(tmp,s)
//      }
//    }
//  }
//}

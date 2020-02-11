package spinal.lib.generator

import spinal.core.{Area, dontName, log2Up}
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.bus.misc._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object BmbInterconnectGenerator{
  class ArbitrationKind
  val ROUND_ROBIN = new ArbitrationKind
  val STATIC_PRIORITY = new ArbitrationKind
}

case class BmbInterconnectGenerator() extends Generator{
  val lock = Lock()
  var defaultArbitration : BmbInterconnectGenerator.ArbitrationKind = BmbInterconnectGenerator.ROUND_ROBIN
  def setDefaultArbitration(kind : BmbInterconnectGenerator.ArbitrationKind): Unit ={
    defaultArbitration = kind
  }
  def setPriority(m : Handle[Bmb], priority : Int) = getMaster(m).priority = priority

  case class MasterModel(@dontName bus : Handle[Bmb], lock : Lock) extends Generator{
    val requirements = Handle[BmbParameter]
    var connector : (Bmb,Bmb) => Unit = defaultConnector
    var priority = 0

    this.setCompositeName(bus, "interconnect")

    dependencies += bus
    dependencies += lock
    val logic = add task new Area{
      val busConnections = connections.filter(_.m == bus)
      val busSlaves = busConnections.map(c => slaves(c.s))
      val decoder = new BmbDecoder(bus.p, busSlaves.map(_.mapping.get), busSlaves.map(_.capabilities.get))
      decoder.setCompositeName(bus, "decoder")
      connector(bus, decoder.io.input)
      for((connection, decoderOutput) <- (busConnections, decoder.io.outputs).zipped) {
        connection.decoder.load(decoderOutput)
      }
    }
  }

  case class SlaveModel(@dontName bus : Handle[Bmb], lock : Lock) extends Generator{
    val capabilities = Handle[BmbParameter]
    val requirements = Handle[BmbParameter]
    var arbiterRequirements = Handle[BmbParameter]
    val mapping = Handle[AddressMapping]
    var connector: (Bmb, Bmb) => Unit = defaultConnector
    var requireUnburstify, requireDownSizer, requireUpSizer = false

    this.setCompositeName(bus, "interconnect")

    dependencies ++= List(bus, mapping)
    dependencies += lock
    val logic = add task new Area{
      val busConnections = connections.filter(_.s == bus).sortBy(connection => getMaster(connection.m).priority).reverse
      val arbiter = new BmbArbiter(arbiterRequirements, busConnections.size, 3, lowerFirstPriority = defaultArbitration == BmbInterconnectGenerator.STATIC_PRIORITY)
      arbiter.setCompositeName(bus, "arbiter")
      val requireBurstSpliting = arbiterRequirements.lengthWidth != requirements.lengthWidth
      @dontName var busPtr = arbiter.io.output

      val downSizer = if(requireDownSizer){
        val c = BmbDownSizerBridge(
          inputParameter = busPtr.p,
          outputParameter = BmbDownSizerBridge.outputParameterFrom(
            inputParameter = busPtr.p,
            outputDataWidth = requirements.dataWidth
          )
        ).setCompositeName(bus, "downSizer")
        c.io.input << busPtr
        busPtr = c.io.output
        c
      }
      
      val upSizer = if(requireUpSizer){
        val c = BmbUpSizerBridge(
          inputParameter = busPtr.p,
          outputParameter = BmbUpSizerBridge.outputParameterFrom(
            inputParameter = busPtr.p,
            outputDataWidth = requirements.dataWidth
          )
        ).setCompositeName(bus, "upSizer")
        c.io.input << busPtr
        busPtr = c.io.output
        c
      }
      
      val burstSpliter = if(requireUnburstify){
        val c = BmbUnburstify(busPtr.p).setCompositeName(bus, "burstUnburstifier")
        c.io.input << busPtr
        busPtr = c.io.output
        c
      }

      connector(busPtr, bus)
      for((connection, arbiterInput) <- (busConnections, arbiter.io.inputs).zipped) {
        connection.arbiter.load(arbiterInput)
      }
    }

    val requirementsGenerator = this add new Generator{
      dependencies += capabilities
      dependencies += lock
      products += requirements

      add task {
        val busConnections = connections.filter(_.s == bus)
        val busMasters = busConnections.map(c => masters(c.m))
        assert(busMasters.nonEmpty, s"$bus has no master")
        val routerBitCount = log2Up(busConnections.size)
        val inputSourceWidth = busMasters.map(_.requirements.sourceWidth).max 
        val inputContextWidth = busMasters.map(_.requirements.contextWidth).max
        val inputLengthWidth = busMasters.map(_.requirements.lengthWidth).max
        var inputAlignement : BmbParameter.BurstAlignement.Kind = BmbParameter.BurstAlignement.LENGTH
        val inputDataWidth = busMasters.map(_.requirements.dataWidth).max
        if(busMasters.exists(_.requirements.alignment.allowWord)) inputAlignement = BmbParameter.BurstAlignement.WORD
        if(busMasters.exists(_.requirements.alignment.allowByte)) inputAlignement = BmbParameter.BurstAlignement.BYTE




        arbiterRequirements.load(capabilities.copy(
          dataWidth = inputDataWidth,
          sourceWidth = inputSourceWidth + routerBitCount,
          lengthWidth = inputLengthWidth,
          contextWidth = inputContextWidth,
          alignment = inputAlignement
        ))

        requirements.load(arbiterRequirements.get)

        //require down
        requireDownSizer = requirements.dataWidth > capabilities.dataWidth
        requireUpSizer = requirements.dataWidth < capabilities.dataWidth
        if(requireDownSizer){
          requirements.load(BmbDownSizerBridge.outputParameterFrom(
            inputParameter   = requirements,
            outputDataWidth  = capabilities.dataWidth
          ))
        }
        if(requireUpSizer){
          requirements.load(BmbUpSizerBridge.outputParameterFrom(
            inputParameter   = requirements,
            outputDataWidth  = capabilities.dataWidth
          ))
        }

        requireUnburstify = capabilities.lengthWidth < requirements.lengthWidth
        if(requireUnburstify){  //TODO manage allowXXXburst flags
          assert(capabilities.lengthWidth == log2Up(capabilities.byteCount) && !capabilities.alignment.allowByte)
          requirements.load(BmbUnburstify.outputParameter(
            inputParameter = requirements
          ))
        }

        assert(requirements.sourceWidth <= capabilities.sourceWidth)
      }
    }
  }

  class ConnectionModel(@dontName val m : Handle[Bmb],@dontName val s : Handle[Bmb]) extends Generator{
    var connector : (Bmb,Bmb) => Unit = defaultConnector
    @dontName val decoder, arbiter = Handle[Bmb]()

    Dependable(s){
      val address = getSlave(s).mapping.get match {
        case `DefaultMapping` => BigInt(0)
        case m => m.lowerBound
      }
      tags += new MemoryConnection(m, s, address)
    }

    dependencies ++= List(decoder, arbiter)
    val logic = add task new Area{
      connector(decoder, arbiter)
    }
  }

  def defaultConnector(m : Bmb, s : Bmb) : Unit = s << m

  @dontName val masters = mutable.LinkedHashMap[Handle[Bmb], MasterModel]()
  @dontName val slaves = mutable.LinkedHashMap[Handle[Bmb], SlaveModel]()
  @dontName val connections = ArrayBuffer[ConnectionModel]()

  def getMaster(key : Handle[Bmb]) = masters.getOrElseUpdate(key, new MasterModel(key, lock))
  def getSlave(key : Handle[Bmb]) = slaves.getOrElseUpdate(key, new SlaveModel(key, lock))

  def setConnector(bus : Handle[Bmb])( connector : (Bmb,Bmb) => Unit): Unit = (masters.get(bus), slaves.get(bus)) match {
    case (Some(m), _) =>    m.connector = connector
    case (None, Some(s)) => s.connector = connector
    case _ => ???
  }

  def setConnector(m : Handle[Bmb], s : Handle[Bmb])(connector : (Bmb,Bmb) => Unit): Unit = connections.find(e => e.m == m && e.s == s) match {
    case Some(c) => c.connector = connector
    case _ => ???
  }

  def addSlave(capabilities : Handle[BmbParameter],
               requirements : Handle[BmbParameter],
               bus : Handle[Bmb],
               mapping: Handle[AddressMapping]) : Unit = {
    val model = getSlave(bus)
    model.capabilities.merge(capabilities)
    model.requirements.merge(requirements)
    model.mapping.merge(mapping)
  }

  def addSlaveAt(capabilities : Handle[BmbParameter],
               requirements : Handle[BmbParameter],
               bus : Handle[Bmb],
               address: Handle[BigInt]) : Unit = {
    val model = getSlave(bus)
    model.capabilities.merge(capabilities)
    model.requirements.merge(requirements)
    Dependable(capabilities, address){
      model.mapping.load(SizeMapping(address, BigInt(1) << capabilities.addressWidth))
    }
  }


  def addMaster(requirements : Handle[BmbParameter], bus : Handle[Bmb], priority : Int = 0) : Unit = {
    val model = getMaster(bus)
    model.requirements.merge(requirements)
    model.priority = priority
  }

  def addConnection(m : Handle[Bmb], s : Handle[Bmb]) : this.type = {
    connections += new ConnectionModel(m, s)
    getMaster(m).dependencies += getSlave(s).mapping
    getMaster(m).dependencies += getSlave(s).capabilities
    getSlave(s).requirementsGenerator.dependencies += getMaster(m).requirements
    this
  }

  def addConnection(m : Handle[Bmb], s : Seq[Handle[Bmb]]) : this.type = {
    for(e <- s) addConnection(m, e)
    this
  }
  def addConnection(l : (Handle[Bmb], Seq[Handle[Bmb]])*) : this.type = {
    for((m, s) <- l) addConnection(m, s)
    this
  }
}


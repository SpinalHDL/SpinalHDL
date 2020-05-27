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
      val arbiter = new BmbArbiter(arbiterRequirements, busConnections.size, lowerFirstPriority = defaultArbitration == BmbInterconnectGenerator.STATIC_PRIORITY)
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






case class BmbSmpInterconnectGenerator() extends Generator{

  case class MasterModel(@dontName bus : Handle[Bmb], lock : Lock){
    val connections = ArrayBuffer[ConnectionModel]()
    val accessRequirements = Handle[BmbAccessParameter]
    val invalidationSource = Handle[BmbInvalidationParameter]
    val invalidationCapabilities = Handle[BmbInvalidationParameter]
    val invalidationRequirements = Handle[BmbInvalidationParameter]
    var priority = 0

    dependencies += lock

    add task assert(connections.nonEmpty, s"$bus has no slaves")

    val decoderGen = add task new Generator {
      dependencies += accessRequirements
      dependencies ++= connections.map(_.mapping)
      dependencies ++= connections.map(_.s.accessCapabilities)
      add task {
        slaves match {
          case _ if connections.size == 1 && connections.head.mapping.get == DefaultMapping && accessRequirements.canRead == connections.head.s.accessCapabilities.canRead  && accessRequirements.canWrite == connections.head.s.accessCapabilities.canWrite => {
            connections.head.decoder.merge(bus)
            connections.head.decoderAccessRequirements.merge(accessRequirements)
          }
          case _ => {
            for(c <- connections){
              c.decoderAccessRequirements.merge(accessRequirements.produce(accessRequirements.copy(
                addressWidth = c.s.accessCapabilities.addressWidth,
                canRead = c.s.accessCapabilities.canRead,
                canWrite = c.s.accessCapabilities.canWrite
              )))
            }
            new Generator{
              dependencies += bus
              dependencies += invalidationRequirements
              dependencies ++= connections.map(_.decoderAccessRequirements)

              add task new Area {
                val decoder = BmbDecoder(bus.p, connections.map(_.mapping.get), connections.map(c => BmbParameter(c.decoderAccessRequirements.get, invalidationRequirements.get)))
                decoder.setCompositeName(bus, "decoder")
                decoder.io.input << bus
                for((connection, decoderOutput) <- (connections, decoder.io.outputs).zipped) {
                  connection.decoder.load(decoderOutput)
                }
              }
            }
          }
        }
      }
    }

    val invalidationRequirementsGen = add task new Generator{
      dependencies ++= connections.map(_.s.invalidationRequirements)
      val gen = add task new Area{
        val sInvalidationRequirements = connections.map(_.s.invalidationRequirements)
        var invalidationAlignement : BmbParameter.BurstAlignement.Kind = BmbParameter.BurstAlignement.LENGTH
        if(sInvalidationRequirements.exists(_.invalidateAlignment.allowWord)) invalidationAlignement = BmbParameter.BurstAlignement.WORD
        if(sInvalidationRequirements.exists(_.invalidateAlignment.allowByte)) invalidationAlignement = BmbParameter.BurstAlignement.BYTE
        val aggregated = BmbInvalidationParameter(
          canInvalidate       = sInvalidationRequirements.exists(_.canInvalidate),
          canSync             = sInvalidationRequirements.exists(_.canInvalidate),
          invalidateLength    = sInvalidationRequirements.map(_.invalidateLength).max,
          invalidateAlignment = invalidationAlignement
        )

        invalidationSource.load(aggregated)
      }
    }

    val invalidationRequirementsGen2 = add task new Generator{
      dependencies ++= connections.map(_.arbiterInvalidationRequirements)
      val gen = add task new Area{
        val sInvalidationRequirements = connections.map(_.arbiterInvalidationRequirements)
        var invalidationAlignement : BmbParameter.BurstAlignement.Kind = BmbParameter.BurstAlignement.LENGTH
        if(sInvalidationRequirements.exists(_.invalidateAlignment.allowWord)) invalidationAlignement = BmbParameter.BurstAlignement.WORD
        if(sInvalidationRequirements.exists(_.invalidateAlignment.allowByte)) invalidationAlignement = BmbParameter.BurstAlignement.BYTE
        val aggregated = BmbInvalidationParameter(
          canInvalidate       = sInvalidationRequirements.exists(_.canInvalidate),
          canSync             = sInvalidationRequirements.exists(_.canInvalidate),
          invalidateLength    = sInvalidationRequirements.map(_.invalidateLength).max,
          invalidateAlignment = invalidationAlignement
        )

        invalidationRequirements.load(aggregated)
      }
    }


  }


  case class SlaveModel(@dontName bus : Handle[Bmb], lock : Lock) extends Generator{
    val connections = ArrayBuffer[ConnectionModel]()
    val accessSource = Handle[BmbAccessParameter]()
    val accessCapabilities = Handle[BmbAccessParameter]()
    val accessRequirements = Handle[BmbAccessParameter]()
    val invalidationRequirements = Handle[BmbInvalidationParameter]()
    val mapping = Handle[AddressMapping]

    dependencies += lock

    add task assert(connections.nonEmpty, s"$bus has no master")

    val arbiterGen = add task new Generator {
      dependencies += accessSource
      dependencies ++= connections.map(_.s.accessCapabilities)
      add task {
        slaves match {
          case _ if connections.size == 1 => {
            connections.head.arbiterInvalidationRequirements.merge(invalidationRequirements)
            connections.head.arbiter.merge(bus)
          }
          case _ => {
            connections.foreach(_.arbiterInvalidationRequirements.merge(invalidationRequirements))
            new Generator{
              dependencies += bus

              add task new Area {
                val connectionsSorted = connections.sortBy(connection => connection.m.priority).reverse
                val arbiter = new BmbArbiter(
                  p = bus.p,
                  connections.size,
                  lowerFirstPriority = defaultArbitration == BmbInterconnectGenerator.STATIC_PRIORITY,
                  inputsWithInv = connections.map(_.arbiterInvalidationRequirements.canInvalidate),
                  inputsWithSync = connections.map(_.arbiterInvalidationRequirements.canSync),
                  pendingInvMax = 16 //TODO
                )
                arbiter.setCompositeName(bus, "arbiter")
                for((connection, arbiterInput) <- (connectionsSorted, arbiter.io.inputs).zipped) {
                  connection.arbiter.load(arbiterInput)
                }
                arbiter.io.output >> bus
              }
            }
          }
        }
      }
    }

    val gen = add task new Generator{
      dependencies ++= connections.map(_.m.accessRequirements)
      val gen = add task new Area{
        val mAccessRequirements = connections.map(_.m.accessRequirements)
        var inputAlignement : BmbParameter.BurstAlignement.Kind = BmbParameter.BurstAlignement.LENGTH
        if(mAccessRequirements.exists(_.alignment.allowWord)) inputAlignement = BmbParameter.BurstAlignement.WORD
        if(mAccessRequirements.exists(_.alignment.allowByte)) inputAlignement = BmbParameter.BurstAlignement.BYTE
        val aggregated = BmbAccessParameter(
          addressWidth = mAccessRequirements.map(_.addressWidth).max,
          dataWidth    = mAccessRequirements.map(_.dataWidth).max,
          lengthWidth  = mAccessRequirements.map(_.lengthWidth).max,
          sourceWidth  = mAccessRequirements.map(_.sourceWidth).max,
          contextWidth = mAccessRequirements.map(_.contextWidth).max,
          alignment    = inputAlignement,
          alignmentMin = mAccessRequirements.map(_.alignmentMin).min,
          canRead      = mAccessRequirements.exists(_.canRead),
          canWrite     = mAccessRequirements.exists(_.canWrite),
          canExclusive = mAccessRequirements.exists(_.canExclusive)
        )
        accessSource.load(aggregated)
      }
    }

    val accessRequirementsGen = add task new Generator{
      dependencies ++= connections.map(_.arbiterAccessRequirements)

      val gen = add task new Area{
        val mAccessRequirements = connections.map(_.arbiterAccessRequirements)
        var inputAlignement : BmbParameter.BurstAlignement.Kind = BmbParameter.BurstAlignement.LENGTH
        if(mAccessRequirements.exists(_.alignment.allowWord)) inputAlignement = BmbParameter.BurstAlignement.WORD
        if(mAccessRequirements.exists(_.alignment.allowByte)) inputAlignement = BmbParameter.BurstAlignement.BYTE
        val aggregated = BmbAccessParameter(
          addressWidth = mAccessRequirements.map(_.addressWidth).max,
          dataWidth    = mAccessRequirements.map(_.dataWidth).max,
          lengthWidth  = mAccessRequirements.map(_.lengthWidth).max,
          sourceWidth  = mAccessRequirements.map(_.sourceWidth).max + log2Up(connections.size),
          contextWidth = mAccessRequirements.map(_.contextWidth).max,
          alignment    = inputAlignement,
          alignmentMin = mAccessRequirements.map(_.alignmentMin).min,
          canRead      = mAccessRequirements.exists(_.canRead),
          canWrite     = mAccessRequirements.exists(_.canWrite),
          canExclusive = mAccessRequirements.exists(_.canExclusive)
        )
        accessRequirements.load(aggregated)
      }
    }
  }


  def defaultConnector(m : Bmb, s : Bmb) : Unit = s << m

  case class ConnectionModel(m : MasterModel, s : SlaveModel, mapping : Handle[AddressMapping]) extends Generator{
    var connector : (Bmb,Bmb) => Unit = defaultConnector

    val decoderAccessRequirements = Handle[BmbAccessParameter]()
    val arbiterInvalidationRequirements = Handle[BmbInvalidationParameter]()
    val arbiter, decoder = Handle[Bmb]
    val arbiterAccessRequirements = Handle[BmbAccessParameter]
    val decoderInvalidationRequirements = Handle[BmbInvalidationParameter]

    abstract class Bridge{
      def logic(mSide : Bmb) : Bmb
      def accessParameter(mSide : BmbAccessParameter) : BmbAccessParameter
    }
    val accessBridges = ArrayBuffer[Bridge]()
    val adapterGen = add task List(decoderAccessRequirements, s.accessCapabilities).produce{
      val dummySlaveParameter = BmbInvalidationParameter()

      if(m.accessRequirements.dataWidth < s.accessCapabilities.dataWidth) accessBridges += new Bridge {
        override def accessParameter(mSide: BmbAccessParameter): BmbAccessParameter = BmbUpSizerBridge.outputParameterFrom(
          inputParameter = BmbParameter(mSide, dummySlaveParameter),
          outputDataWidth = s.accessCapabilities.dataWidth
        ).toAccessRequirements

        override def logic(mSide: Bmb): Bmb = {
          val c = BmbUpSizerBridge(
            inputParameter = mSide.p,
            outputParameter = BmbUpSizerBridge.outputParameterFrom(mSide.p, s.accessCapabilities.dataWidth)
          )
          c.setCompositeName(m.bus, "upSizer", true)
          c.io.input << mSide
          c.io.output
        }
      }

      if(m.accessRequirements.dataWidth > s.accessCapabilities.dataWidth) accessBridges += new Bridge {
        override def accessParameter(mSide: BmbAccessParameter): BmbAccessParameter = BmbDownSizerBridge.outputParameterFrom(
          inputParameter = BmbParameter(mSide, dummySlaveParameter),
          outputDataWidth = s.accessCapabilities.dataWidth
        ).toAccessRequirements

        override def logic(mSide: Bmb): Bmb = {
          val c = BmbDownSizerBridge(
            inputParameter = mSide.p,
            outputParameter = BmbDownSizerBridge.outputParameterFrom(mSide.p, s.accessCapabilities.dataWidth)
          )
          c.setCompositeName(m.bus, "downSizer", true)
          c.io.input << mSide
          c.io.output
        }
      }

      if(m.accessRequirements.lengthWidth > s.accessCapabilities.lengthWidth) {
        if(s.accessCapabilities.lengthWidth == log2Up(s.accessCapabilities.dataWidth/8)) {
          accessBridges += new Bridge {
            override def accessParameter(mSide: BmbAccessParameter): BmbAccessParameter = BmbUnburstify.outputParameter(
              inputParameter = BmbParameter(mSide, dummySlaveParameter)
            ).toAccessRequirements

            override def logic(mSide: Bmb): Bmb = {
              val c = BmbUnburstify(
                inputParameter = mSide.p
              )
              c.setCompositeName(m.bus, "unburstify", true)
              c.io.input << mSide
              c.io.output
            }
          }
        } else{
          ???
        }
      }

      var accessParameter = decoderAccessRequirements.get
      for(bridge <- accessBridges){
        accessParameter = bridge.accessParameter(accessParameter)
      }
      arbiterAccessRequirements.load(accessParameter)
    }

    val adapterGen2 = new Generator {
      dependencies += arbiterInvalidationRequirements
      dependencies += m.invalidationCapabilities

      add task {
        val dummySlaveParameter = BmbInvalidationParameter()

        val invalidationParameter = arbiterInvalidationRequirements.get

        decoderInvalidationRequirements.load(invalidationParameter)
      }
    }

    List(arbiter, decoder).produce{
      var bmb : Bmb = decoder
      for(bridge <- accessBridges){
        bmb = bridge.logic(bmb)
      }
      connector(bmb, arbiter)
    }
  }

  val masters = mutable.LinkedHashMap[Handle[Bmb], MasterModel]()
  val slaves = mutable.LinkedHashMap[Handle[Bmb], SlaveModel]()
  val lock = Lock()
  var defaultArbitration : BmbInterconnectGenerator.ArbitrationKind = BmbInterconnectGenerator.ROUND_ROBIN


  def getMaster(key : Handle[Bmb]) = masters.getOrElseUpdate(key, MasterModel(key, lock))
  def getSlave(key : Handle[Bmb]) = slaves.getOrElseUpdate(key, SlaveModel(key, lock))


//  def setConnector(bus : Handle[Bmb])( connector : (Bmb,Bmb) => Unit): Unit = (masters.get(bus), slaves.get(bus)) match {
//    case (Some(m), _) =>    m.connector = connector
//    case (None, Some(s)) => s.connector = connector
//    case _ => ???
//  }
//
//  def setConnector(m : Handle[Bmb], s : Handle[Bmb])(connector : (Bmb,Bmb) => Unit): Unit = connections.find(e => e.m == m && e.s == s) match {
//    case Some(c) => c.connector = connector
//    case _ => ???
//  }

  def addMaster(accessRequirements : Handle[BmbAccessParameter],
                invalidationSource : Handle[BmbInvalidationParameter] = Handle[BmbInvalidationParameter],
                invalidationCapabilities : Handle[BmbInvalidationParameter] = BmbInvalidationParameter(),
                invalidationRequirements : Handle[BmbInvalidationParameter] = Handle[BmbInvalidationParameter],
                bus : Handle[Bmb]): Unit ={
    val model = getMaster(bus)
    model.accessRequirements.merge(accessRequirements)
    model.invalidationSource.merge(invalidationSource)
    model.invalidationCapabilities.merge(invalidationCapabilities)
    model.invalidationRequirements.merge(invalidationRequirements)
  }

//  def addMaster(accessRequirements : Handle[BmbMasterRequirements],
//                bus : Handle[Bmb]): Unit = addMaster(
//    accessRequirements = accessRequirements,
//    invalidationSource = Handle[BmbSlaveRequirements],
//    invalidationCapabilities = BmbSlaveRequirements(),
//    invalidationRequirements = Handle[BmbSlaveRequirements],
//    bus
//  )

  def addSlave(accessSource : Handle[BmbAccessParameter],
               accessCapabilities : Handle[BmbAccessParameter],
               accessRequirements : Handle[BmbAccessParameter],
               invalidationRequirements : Handle[BmbInvalidationParameter],
               bus : Handle[Bmb],
               mapping : Handle[AddressMapping]): Unit ={
    val model = getSlave(bus)
    model.accessSource.merge(accessSource)
    model.accessRequirements.merge(accessRequirements)
    model.accessCapabilities.merge(accessCapabilities)
    model.invalidationRequirements.merge(invalidationRequirements)
    model.mapping.merge(mapping)
  }
  def addSlaveAt(accessSource : Handle[BmbAccessParameter] = Handle[BmbAccessParameter],
                 accessCapabilities : Handle[BmbAccessParameter],
                 accessRequirements : Handle[BmbAccessParameter],
                 invalidationRequirements : Handle[BmbInvalidationParameter] = BmbInvalidationParameter(),
                 bus : Handle[Bmb],
                 address: Handle[BigInt]) : Unit = {
    addSlave(
      accessSource = accessSource,
      accessCapabilities = accessCapabilities,
      accessRequirements = accessRequirements,
      invalidationRequirements = invalidationRequirements,
      bus = bus,
      mapping = List(accessCapabilities, address).produce(SizeMapping(address, BigInt(1) << accessCapabilities.addressWidth))
    )
  }

  def addConnection(m : Handle[Bmb], s : Handle[Bmb]) : this.type = {
    val c = ConnectionModel(getMaster(m), getSlave(s), getSlave(s).mapping) //TODO .setCompositeName(m, "connector", true)
    getMaster(m).connections += c
    getSlave(s).connections += c
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






object BmbInterconnect2GeneratorGen extends App{
  import spinal.core._
  SpinalVerilog(gen = new Component {
    val mainGenerator = new Generator {
      val interconnect = new BmbSmpInterconnectGenerator()

      val mA = new Area {
        val accessRequirements = BmbAccessParameter(
          addressWidth = 32,
          dataWidth = 32,
          lengthWidth = 8,
          sourceWidth = 2,
          contextWidth = 5
        )
        val invalidationSource = Handle[BmbInvalidationParameter]
        val invalidationCapabilities = invalidationSource
        val invalidationRequirements = Handle[BmbInvalidationParameter]
        val config = invalidationRequirements.derivate(BmbParameter(accessRequirements, _))
        val bus = config.produce(slave(Bmb(config)))

        interconnect.addMaster(
          accessRequirements = accessRequirements,
          invalidationSource = invalidationSource,
          invalidationCapabilities = invalidationCapabilities,
          invalidationRequirements = invalidationRequirements,
          bus = bus
        )
      }

      val mB = new Area {
        val accessRequirements = BmbAccessParameter(
          addressWidth = 32,
          dataWidth = 32,
          lengthWidth = 8,
          sourceWidth = 2,
          contextWidth = 5
        )
        val invalidationSource = Handle[BmbInvalidationParameter]
        val invalidationCapabilities = invalidationSource
        val invalidationRequirements = Handle[BmbInvalidationParameter]
        val config = invalidationRequirements.derivate(BmbParameter(accessRequirements, _))
        val bus = config.produce(slave(Bmb(config)))

        interconnect.addMaster(
          accessRequirements = accessRequirements,
          invalidationSource = invalidationSource,
          invalidationCapabilities = invalidationCapabilities,
          invalidationRequirements = invalidationRequirements,
          bus = bus
        )
      }

      val sA = new Area {
        val accessSource = Handle[BmbAccessParameter]
        val accessCapabilities = BmbAccessParameter(
          addressWidth = 32,
          dataWidth = 16,
          lengthWidth = 1,
          sourceWidth = 2,
          contextWidth = 5
        )
        val accessRequirements = Handle[BmbAccessParameter]
        val invalidationRequirements = accessRequirements produce BmbInvalidationParameter(
          canInvalidate = false,
          canSync = false,
          invalidateLength = 0
        )
        val config = List(accessRequirements, invalidationRequirements).produce(BmbParameter(accessRequirements, invalidationRequirements))
        val bus = config.produce(master(Bmb(config)))

        interconnect.addSlave(
          accessSource = accessSource,
          accessCapabilities = accessCapabilities,
          accessRequirements = accessRequirements,
          invalidationRequirements = invalidationRequirements,
          bus = bus,
          mapping = DefaultMapping
        )
      }

      val sB = new Area{
        val accessSource = Handle[BmbAccessParameter]
        val accessCapabilities = BmbAccessParameter(
          addressWidth = 32,
          dataWidth = 128,
          lengthWidth = 8,
          sourceWidth = 2,
          contextWidth = 5
        )
        val accessRequirements = Handle[BmbAccessParameter]
        val invalidationRequirements = accessRequirements produce BmbInvalidationParameter(
          canInvalidate    = false,
          canSync          = false,
          invalidateLength = 0
        )
        val config = List(accessRequirements, invalidationRequirements).produce(BmbParameter(accessRequirements, invalidationRequirements))
        val bus = config.produce(master(Bmb(config)))

        interconnect.addSlave(
          accessSource = accessSource,
          accessCapabilities = accessCapabilities,
          accessRequirements = accessRequirements,
          invalidationRequirements = invalidationRequirements,
          bus = bus,
          mapping = SizeMapping(0x400, 0x100)
        )
      }

      interconnect.addConnection(mA.bus, sA.bus)
      interconnect.addConnection(mA.bus, sB.bus)
      interconnect.addConnection(mB.bus, sA.bus)
      interconnect.addConnection(mB.bus, sB.bus)
    }

    println(mainGenerator.sA.bus.getName())
    GeneratorCompiler(mainGenerator)
  })
}





//object BmbInterconnectGeneratorGen extends App{
//  import spinal.core._
//  SpinalVerilog(new Component{
//    val mainGenerator = new Generator{
//      //Define some Handle which will be later loaded with real values
//      val a,b = Handle[Int]
//
//      //Print a + b
//      val calculator = new Generator{
//        //Specify that this generator need a and b before executing his tasks
//        dependencies += a
//        dependencies += b
//
//        //Create a new task that will run when all the dependencies are loaded
//        add task{
//          val sum = a.get + b.get
//          println(s"a + b = $sum") //Will print a + b = 7
//        }
//      }
//
//      //load a and b with values, which will then unlock the calculator generator
//      a.load(3)
//      b.load(4)
//    }
//
//    GeneratorCompiler(mainGenerator)
//    println(mainGenerator.a.getName())
//  })
//}

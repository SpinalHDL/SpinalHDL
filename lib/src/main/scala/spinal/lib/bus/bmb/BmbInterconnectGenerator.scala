package spinal.lib.bus.bmb

import spinal.core._
import spinal.core.fiber._
import spinal.lib.`sexport`
import spinal.lib.bus.misc._
import spinal.lib.generator._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.Seq

object BmbInterconnectGenerator{
  class ArbitrationKind
  val ROUND_ROBIN = new ArbitrationKind
  val STATIC_PRIORITY = new ArbitrationKind

  def apply() = new BmbInterconnectGenerator
}

case class BmbImplicitPeripheralDecoder(bus : Handle[Bmb])
case class BmbImplicitDebugDecoder(bus : Handle[Bmb])

class BmbInterconnectGenerator() extends Area{

  def setDefaultArbitration(kind : BmbInterconnectGenerator.ArbitrationKind): Unit ={
    defaultArbitration = kind
  }
  def setPriority(m : Handle[Bmb], priority : Int) = getMaster(m).priority = priority

  case class MasterModel(@dontName bus : Handle[Bmb], lock : Lock) extends Area{
    val generatorClockDomain = ClockDomain.currentHandle
    val connections = ArrayBuffer[ConnectionModel]()
    val accessRequirements = Handle[BmbAccessParameter]
    val invalidationSource = Handle[BmbInvalidationParameter]
    val invalidationCapabilities = Handle[BmbInvalidationParameter]
    val invalidationRequirements = Handle[BmbInvalidationParameter]
    var connector: (Bmb, Bmb) => Unit = defaultConnector

    var priority = 0

    val DECODER_SMALL = 0
    val DECODER_OUT_OF_ORDER = 1
    val DECODER_SMALL_PER_SOURCE = 2
    val DECODER_PERIPHERALS = 3
    var decoderKind = DECODER_SMALL

    def withOutOfOrderDecoder(): Unit ={
      decoderKind = DECODER_OUT_OF_ORDER
    }
    def withPerSourceDecoder(): Unit ={
      decoderKind = DECODER_SMALL_PER_SOURCE
    }
    def withPeripheralDecoder() : Unit = {
      decoderKind = DECODER_PERIPHERALS
    }
    def addConnection(c : ConnectionModel): Unit ={
      connections += c
      decoderGen.soon(c.decoderAccessRequirements)
      decoderGen.soon(c.decoder)
    }


    lock.map(_ => assert(connections.nonEmpty, s"BMB bus named $bus has no slave. Located at :\n${bus.getScalaLocationLong}"))

    val decoderGen = Handle{
      lock.get
      slaves match {
        case _ if connections.size == 1 && connections.head.mapping.get == DefaultMapping && accessRequirements.canRead == connections.head.s.accessCapabilities.canRead  && accessRequirements.canWrite == connections.head.s.accessCapabilities.canWrite => {
          connections.head.decoder.derivatedFrom(bus){_ =>
            val decoder = Bmb(bus.p)
            connector(bus, decoder)
            decoder
          }
          connections.head.decoderAccessRequirements.load(accessRequirements)
        }
        case _ => {
          for(c <- connections){
            c.decoderAccessRequirements.load(accessRequirements.produce(accessRequirements.copy(
              addressWidth = c.s.accessCapabilities.addressWidth
            ).sourcesTransform(source => source.copy(
              canRead = c.s.accessCapabilities.canRead,
              canWrite = c.s.accessCapabilities.canWrite,
              contextWidth = decoderKind match {
                case DECODER_SMALL | DECODER_PERIPHERALS => source.contextWidth
                case DECODER_SMALL_PER_SOURCE => source.contextWidth
                case DECODER_OUT_OF_ORDER => 0
              }
            ))))
          }

          Handle{
            connections.foreach(c => soon(c.decoder))
            lock.await()
            decoderKind match {
              case DECODER_SMALL | DECODER_PERIPHERALS => new Area {
                val perif = decoderKind == DECODER_PERIPHERALS
                val decoder = BmbDecoder(
                  bus.p,
                  connections.map(_.mapping.get),
                  connections.map(c => BmbParameter(c.decoderAccessRequirements.get, invalidationRequirements.get)),
                  pendingMax = if(perif) 8 else 64,
                  pipelinedDecoder  = perif,
                  pipelinedHalfPipe  = perif
                )

                decoder.setCompositeName(bus, "decoder")
                connector(bus, decoder.io.input)
                for ((connection, decoderOutput) <- (connections, decoder.io.outputs).zipped) {
                  connection.decoder.load(decoderOutput)
                }
              }
              case DECODER_SMALL_PER_SOURCE => new Area {
                val decoder = BmbDecoderPerSource(bus.p, connections.map(_.mapping.get), connections.map(c => BmbParameter(c.decoderAccessRequirements.get, invalidationRequirements.get)))
                decoder.setCompositeName(bus, "decoder")
                connector(bus, decoder.io.input)
                for ((connection, decoderOutput) <- (connections, decoder.io.outputs).zipped) {
                  connection.decoder.load(decoderOutput)
                }
              }
              case DECODER_OUT_OF_ORDER => new Area {
                val decoder = BmbDecoderOutOfOrder(
                  bus.p,
                  connections.map(_.mapping.get),
                  connections.map(c => BmbParameter(c.decoderAccessRequirements.get, invalidationRequirements.get)),
                  pendingRspTransactionMax = 32
                )
                decoder.setCompositeName(bus, "decoder")
                connector(bus, decoder.io.input)
                for ((connection, decoderOutput) <- (connections, decoder.io.outputs).zipped) {
                  connection.decoder.load(decoderOutput)
                }
              }
            }
          }
        }

      }
    }

    invalidationSource.loadAsync{
      lock.await()

      val sInvalidationRequirements = connections.map(_.s.invalidationRequirements)
      var invalidationAlignement : BmbParameter.BurstAlignement.Kind = BmbParameter.BurstAlignement.LENGTH
      if(sInvalidationRequirements.exists(_.invalidateAlignment.allowWord)) invalidationAlignement = BmbParameter.BurstAlignement.WORD
      if(sInvalidationRequirements.exists(_.invalidateAlignment.allowByte)) invalidationAlignement = BmbParameter.BurstAlignement.BYTE
      val aggregated = BmbInvalidationParameter(
        invalidateLength    = sInvalidationRequirements.map(_.invalidateLength).max,
        invalidateAlignment = invalidationAlignement
      )
      aggregated
    }

    invalidationRequirements.loadAsync{
      lock.await()

      val sInvalidationRequirements = connections.map(_.decoderInvalidationRequirements)
      var invalidationAlignement : BmbParameter.BurstAlignement.Kind = BmbParameter.BurstAlignement.LENGTH
      if(sInvalidationRequirements.exists(_.invalidateAlignment.allowWord)) invalidationAlignement = BmbParameter.BurstAlignement.WORD
      if(sInvalidationRequirements.exists(_.invalidateAlignment.allowByte)) invalidationAlignement = BmbParameter.BurstAlignement.BYTE

      val aggregated = BmbInvalidationParameter(
        invalidateLength    = sInvalidationRequirements.map(_.invalidateLength).max,
        invalidateAlignment = invalidationAlignement
      )

      aggregated
    }
  }


  case class SlaveModel(@dontName bus : Handle[Bmb], lock : Lock) extends Area{
    val generatorClockDomain = ClockDomain.currentHandle
    val connections = ArrayBuffer[ConnectionModel]()
    val accessSource = Handle[BmbAccessCapabilities]()
    val accessCapabilities = Handle[BmbAccessCapabilities]()
    val accessRequirements = Handle[BmbAccessParameter]()
    val invalidationRequirements = Handle[BmbInvalidationParameter]()
    val mapping = Handle[AddressMapping]
    var connector: (Bmb, Bmb) => Unit = defaultConnector

    val accessSourceModifiers = ArrayBuffer[BmbAccessCapabilities => BmbAccessCapabilities]()
    def forceAccessSourceDataWidth(dataWidth : Int) = accessSourceModifiers += (c => c.copy(dataWidth = dataWidth))

    def addConnection(c : ConnectionModel): Unit ={
      connections += c
      arbiterGen.soon(c.arbiter)
      invalidationGen.soon(c.arbiterInvalidationRequirements)
    }

    lock.map(_ => assert(connections.nonEmpty, s"BMB bus named $bus has no master. Located at :\n${bus.getScalaLocationLong}"))

    def connectionsSorted = connections.sortBy(connection => connection.m.priority).reverse



    val arbiterGen = Handle(new Area{
      lock.await()
      val oneToOne = (connections.size == 1) generate new Area{
        val arbiter = Bmb(bus.p)
        connector(arbiter, bus)
        connections.head.arbiter.load(arbiter)
      }
      val logic = (connections.size > 1) generate new Area {
        val sorted = connectionsSorted
        val arbiter = new BmbArbiter(
          inputsParameter = sorted.map(c => BmbParameter(c.arbiterAccessRequirements, c.arbiterInvalidationRequirements)),
          outputParameter = bus.p,
          lowerFirstPriority = defaultArbitration == BmbInterconnectGenerator.STATIC_PRIORITY,
          pendingInvMax = 15 //TODO
        )
        arbiter.setCompositeName(bus, "arbiter")
        for((connection, arbiterInput) <- (sorted, arbiter.io.inputs).zipped) {
          connection.arbiter.load(arbiterInput)
        }
        connector(arbiter.io.output, bus)
      }
    })

    val invalidationGen = Handle {
      for(c <- connections){
        c.arbiterInvalidationRequirements.load(invalidationRequirements.copy(
//          canInvalidate = invalidationRequirements.canInvalidate && c.arbiterAccessRequirements.aggregated.withCachedRead
        ))
      }
    }

    accessSource.loadAsync {
      lock.get
      val mAccessRequirements = connectionsSorted.map(_.m.accessRequirements)
      val addressWidths = for(c <- connections) yield c.mapping.get match {
        case m : SizeMapping => log2Up(m.size)
        case _ => c.m.accessRequirements.addressWidth
      }
      val sourcesRemaped = mutable.LinkedHashMap[Int, BmbSourceParameter]()
      val sourceShift = log2Up(mAccessRequirements.size)
      for((master, masterId) <- mAccessRequirements.zipWithIndex; (sourceId, source) <- master.sources){
        sourcesRemaped((sourceId << sourceShift) | masterId) = source
      }

      val aggregated = BmbAccessParameter(
        addressWidth = addressWidths.max,
        dataWidth    = mAccessRequirements.map(_.dataWidth).max,
        sources      = sourcesRemaped
      )

      val modified = accessSourceModifiers.foldLeft(aggregated.toAccessCapabilities)((c, f) => f(c))
      modified.copy(contextWidthMax = Int.MaxValue, sourceWidthMax = Int.MaxValue)
    }

    accessRequirements.loadAsync{
      lock.get
      val mAccessRequirements = connectionsSorted.map(_.arbiterAccessRequirements)

      val sourcesRemaped = mutable.LinkedHashMap[Int, BmbSourceParameter]()
      val sourceShift = log2Up(mAccessRequirements.size)
      for((master, masterId) <- mAccessRequirements.zipWithIndex; (sourceId, source) <- master.sources){
        sourcesRemaped((sourceId << sourceShift) | masterId) = source
      }

      val aggregated = BmbAccessParameter(
        addressWidth = mAccessRequirements.map(_.addressWidth).max,
        dataWidth    = mAccessRequirements.map(_.dataWidth).max,
        sources      = sourcesRemaped
      )
      aggregated
    }
  }


  def defaultConnector(m : Bmb, s : Bmb) : Unit = s << m

  case class ConnectionModel(m : MasterModel, s : SlaveModel, mapping : Handle[AddressMapping]) extends Area{
    var connector : (Bmb,Bmb) => Unit = defaultConnector

    val decoderAccessRequirements = Handle[BmbAccessParameter]()
    val arbiterInvalidationRequirements = Handle[BmbInvalidationParameter]()
    val arbiter, decoder = Handle[Bmb]
    val arbiterAccessRequirements = Handle[BmbAccessParameter]
    val decoderInvalidationRequirements = Handle[BmbInvalidationParameter]

    val CC_FIFO = 0
    val CC_TOGGLE = 1
    var ccKind = CC_FIFO
    def ccByToggle(): Unit ={
      ccKind = CC_TOGGLE
    }

    hardFork{
      val address = mapping.get match {
        case `DefaultMapping` => BigInt(0)
        case m => m.lowerBound
      }
      sexport(new MemoryConnection(m.bus, s.bus, address, mapping))
    }


    abstract class AccessBridge{
      def logic(mSide : Bmb) : Bmb
      def accessParameter(mSide : BmbAccessParameter) : BmbAccessParameter
    }
    val accessBridges = ArrayBuffer[AccessBridge]()

    arbiterAccessRequirements.loadAsync{
      lock.await()

      if(m.accessRequirements.canWrite && m.accessRequirements.canMask && !s.accessCapabilities.canMask) accessBridges += new AccessBridge {
        override def accessParameter(mSide: BmbAccessParameter): BmbAccessParameter = mSide.sourcesTransform(_.copy(canMask = false))

        override def logic(mSide: Bmb): Bmb = m.generatorClockDomain.get{
          val p = mSide.p.copy(access = mSide.p.access.sourcesTransform(_.copy(canMask = false)))
          val c = Bmb(p)
          c.setCompositeName(m.bus, "withoutMask", true)
          c << mSide
          c
        }
      }

      if(m.accessRequirements.dataWidth < s.accessCapabilities.dataWidth) accessBridges += new AccessBridge {
        override def accessParameter(mSide: BmbAccessParameter): BmbAccessParameter = BmbUpSizerBridge.outputParameterFrom(
          inputParameter = mSide,
          outputDataWidth = s.accessCapabilities.dataWidth
        )

        override def logic(mSide: Bmb): Bmb = m.generatorClockDomain.get{
          val c = BmbUpSizerBridge(
            inputParameter = mSide.p,
            outputParameter = BmbUpSizerBridge.outputParameterFrom(mSide.p.access, s.accessCapabilities.dataWidth).toBmbParameter()
          )
          c.setCompositeName(m.bus, "upSizer", true)
          c.io.input << mSide
          c.io.output
        }
      }

      if(m.accessRequirements.dataWidth > s.accessCapabilities.dataWidth) accessBridges += new AccessBridge {
        override def accessParameter(mSide: BmbAccessParameter): BmbAccessParameter = BmbDownSizerBridge.outputParameterFrom(
          inputAccessParameter = mSide,
          outputDataWidth = s.accessCapabilities.dataWidth
        )

        override def logic(mSide: Bmb): Bmb = m.generatorClockDomain.get{
          val c = BmbDownSizerBridge(
            inputParameter = mSide.p,
            outputParameter = BmbDownSizerBridge.outputParameterFrom(mSide.p.access, s.accessCapabilities.dataWidth).toBmbParameter()
          )
          c.setCompositeName(m.bus, "downSizer", true)
          c.io.input << mSide
          c.io.output
        }
      }

      if(m.accessRequirements.lengthWidth > s.accessCapabilities.lengthWidthMax) {
        if(s.accessCapabilities.lengthWidthMax == log2Up(s.accessCapabilities.dataWidth/8)) {
          accessBridges += new AccessBridge {
            override def accessParameter(mSide: BmbAccessParameter): BmbAccessParameter = BmbUnburstify.outputAccessParameter(
              inputParameter = mSide
            )

            override def logic(mSide: Bmb): Bmb = m.generatorClockDomain.get{
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

      if(m.generatorClockDomain.clock != s.generatorClockDomain.clock) { //TODO better sync check
        ccKind match {
          case CC_FIFO =>
            accessBridges += new AccessBridge {
              override def accessParameter(mSide: BmbAccessParameter): BmbAccessParameter = mSide

              override def logic(mSide: Bmb): Bmb = m.generatorClockDomain.get{
                val c = BmbCcFifo(
                  p = mSide.p,
                  cmdDepth = 16,
                  rspDepth = 16,
                  inputCd = m.generatorClockDomain,
                  outputCd = s.generatorClockDomain
                )
                c.setCompositeName(m.bus, "crossClock", true)
                c.io.input << mSide
                c.io.output
              }
            }
          case CC_TOGGLE =>
            accessBridges += new AccessBridge {
              override def accessParameter(mSide: BmbAccessParameter): BmbAccessParameter = mSide

              override def logic(mSide: Bmb): Bmb = m.generatorClockDomain.get{
                val c = BmbCcToggle(
                  p = mSide.p,
                  inputCd = m.generatorClockDomain,
                  outputCd = s.generatorClockDomain
                )
                c.setCompositeName(m.bus, "crossClock", true)
                c.io.input << mSide
                c.io.output
              }
            }
        }
      }

      if(!m.accessRequirements.canSync && s.accessCapabilities.canSync) {
        accessBridges += new AccessBridge {
          override def logic(mSide: Bmb) = {
            val c = BmbSyncRemover(
              p = mSide.p
            )
            c.setCompositeName(s.bus, "syncRemover", true)
            c.io.input << mSide
            c.io.output
          }

          override def accessParameter(mSide: BmbAccessParameter) = BmbSyncRemover.outputConfig(mSide)
        }
      }

      var accessParameter = decoderAccessRequirements.get
      for(bridge <- accessBridges){
        accessParameter = bridge.accessParameter(accessParameter)
      }
      accessParameter
    }



    abstract class InvalidationBridge{
      def logic(sSide : Bmb) : Bmb
      def invalidationParameter(sSide : BmbInvalidationParameter) : BmbInvalidationParameter
    }
    val invalidationBridges = ArrayBuffer[InvalidationBridge]()

    decoderInvalidationRequirements.loadAsync{
      lock.get
      var invalidationParameter = arbiterInvalidationRequirements.get

      for(bridge <- invalidationBridges){
        invalidationParameter = bridge.invalidationParameter(invalidationParameter)
      }
      invalidationParameter
    }

    List(arbiter, decoder).produce{
      var mBus : Bmb = decoder
      for(bridge <- accessBridges){
        mBus = bridge.logic(mBus)
      }

      var sBus = arbiter
      for(bridge <- invalidationBridges){
        sBus = bridge.logic(sBus)
      }

      connector(mBus, sBus)
    }
  }

  val masters = mutable.LinkedHashMap[Handle[Bmb], MasterModel]()
  val slaves = mutable.LinkedHashMap[Handle[Bmb], SlaveModel]()
  val lock = Lock()
//  lock.retain() //TODO ????
//  add task lock.release()

  var defaultArbitration : BmbInterconnectGenerator.ArbitrationKind = BmbInterconnectGenerator.ROUND_ROBIN


  def getMaster(key : Handle[Bmb]) = masters.getOrElseUpdate(key, MasterModel(key, lock).setCompositeName(key, "masterModel"))
  def getSlave(key : Handle[Bmb]) = slaves.getOrElseUpdate(key, SlaveModel(key, lock).setCompositeName(key, "slaveModel"))


  def setConnector(bus : Handle[Bmb])( connector : (Bmb,Bmb) => Unit): Unit = (masters.get(bus), slaves.get(bus)) match {
    case (Some(m), _) =>    m.connector = connector
    case (None, Some(s)) => s.connector = connector
    case _ => ???
  }

  def setConnector(m : Handle[Bmb], s : Handle[Bmb])(connector : (Bmb,Bmb) => Unit): Unit = getMaster(m).connections.find(_.s.bus == s) match {
    case Some(c) => c.connector = connector
    case _ => ???
  }

  val scalaWorkAround = Handle[Bmb]
  def setPipelining(m : Handle[Bmb], s : Handle[Bmb] = scalaWorkAround)(cmdValid : Boolean = false,
                                       cmdReady : Boolean = false,
                                       cmdHalfRate : Boolean = false,
                                       rspValid : Boolean = false,
                                       rspReady : Boolean = false,
                                       rspHalfRate : Boolean = false,
                                       invValid : Boolean = false,
                                       invReady : Boolean = false,
                                       invHalfRate : Boolean = false,
                                       ackValid : Boolean = false,
                                       ackReady : Boolean = false,
                                       ackHalfRate : Boolean = false,
                                       syncValid : Boolean = false,
                                       syncReady : Boolean = false,
                                       syncHalfRate : Boolean = false): Unit = {
    if (s == scalaWorkAround) {
      setConnector(m)(_.pipelined(
        cmdValid     = cmdValid,
        cmdReady     = cmdReady,
        cmdHalfRate  = cmdHalfRate,
        rspValid     = rspValid,
        rspReady     = rspReady,
        rspHalfRate  = rspHalfRate,
        invValid     = invValid,
        invReady     = invReady,
        invHalfRate  = invHalfRate,
        ackValid     = ackValid,
        ackReady     = ackReady,
        ackHalfRate  = ackHalfRate,
        syncValid    = syncValid,
        syncReady    = syncReady,
        syncHalfRate = syncHalfRate) >> _)
    } else {
      setConnector(m, s)(_.pipelined(
        cmdValid = cmdValid,
        cmdReady = cmdReady,
        cmdHalfRate = cmdHalfRate,
        rspValid = rspValid,
        rspReady = rspReady,
        rspHalfRate = rspHalfRate,
        invValid = invValid,
        invReady = invReady,
        invHalfRate = invHalfRate,
        ackValid = ackValid,
        ackReady = ackReady,
        ackHalfRate = ackHalfRate,
        syncValid = syncValid,
        syncReady = syncReady,
        syncHalfRate = syncHalfRate) >> _)
    }
  }


  def addSlave(accessSource       : Handle[BmbAccessCapabilities] = Handle[BmbAccessCapabilities],
               accessCapabilities : Handle[BmbAccessCapabilities],
               accessRequirements : Handle[BmbAccessParameter],
               invalidationRequirements : Handle[BmbInvalidationParameter] = BmbInvalidationParameter(),
               bus : Handle[Bmb],
               mapping : Handle[AddressMapping]) ={
    val model = getSlave(bus)
    accessSource.loadAsync(model.accessSource)
    model.accessCapabilities.loadAsync(accessCapabilities)
    accessRequirements.loadAsync(model.accessRequirements)
    model.invalidationRequirements.loadAsync(invalidationRequirements)
    model.mapping.loadAsync(mapping)
    model
  }


  def addMaster(accessRequirements : Handle[BmbAccessParameter],
                invalidationSource : Handle[BmbInvalidationParameter] = Handle[BmbInvalidationParameter],
                invalidationCapabilities : Handle[BmbInvalidationParameter] = BmbInvalidationParameter(),
                invalidationRequirements : Handle[BmbInvalidationParameter] = Handle[BmbInvalidationParameter],
                bus : Handle[Bmb]): Unit ={
    val model = getMaster(bus)
    model.accessRequirements.loadAsync(accessRequirements)
    invalidationSource.loadAsync(model.invalidationSource)
    model.invalidationCapabilities.loadAsync(invalidationCapabilities)
    invalidationRequirements.loadAsync(model.invalidationRequirements)
  }

  def addConnection(m : Handle[Bmb], s : Handle[Bmb]) : ConnectionModel = {
    val c = ConnectionModel(getMaster(m), getSlave(s), getSlave(s).mapping).setCompositeName(m, "connector", true)
    getMaster(m).addConnection(c)
    getSlave(s).addConnection(c)
    c
  }
  def addConnection(m : Handle[Bmb], s : Seq[Handle[Bmb]]) : this.type = {
    for(e <- s) addConnection(m, e)
    this
  }
  def addConnection(l : (Handle[Bmb], Seq[Handle[Bmb]])*) : this.type = {
    for((m, s) <- l) addConnection(m, s)
    this
  }
  def getConnection(m : Handle[Bmb], s : Handle[Bmb]) = getMaster(m).connections.find(_.s.bus == s).get
}






//object BmbInterconnect2GeneratorGen extends App{
//  import spinal.core._
//  SpinalVerilog(gen = new Component {
//    val mainGenerator = new Generator {
//      val interconnect = new BmbSmpInterconnectGenerator()
//
//      val mA = new Area {
//        val accessRequirements = BmbAccessParameter(
//          addressWidth = 32,
//          dataWidth = 32,
//          lengthWidth = 8,
//          sourceWidth = 2,
//          contextWidth = 5
//        )
//        val invalidationSource = Handle[BmbInvalidationParameter]
//        val invalidationCapabilities = invalidationSource
//        val invalidationRequirements = Handle[BmbInvalidationParameter]
//        val config = invalidationRequirements.derivate(BmbParameter(accessRequirements, _))
//        val bus = config.produce(slave(Bmb(config)))
//
//        interconnect.addMaster(
//          accessRequirements = accessRequirements,
//          invalidationSource = invalidationSource,
//          invalidationCapabilities = invalidationCapabilities,
//          invalidationRequirements = invalidationRequirements,
//          bus = bus
//        )
//      }
//
//      val mB = new Area {
//        val accessRequirements = BmbAccessParameter(
//          addressWidth = 32,
//          dataWidth = 32,
//          lengthWidth = 8,
//          sourceWidth = 2,
//          contextWidth = 5
//        )
//        val invalidationSource = Handle[BmbInvalidationParameter]
//        val invalidationCapabilities = invalidationSource
//        val invalidationRequirements = Handle[BmbInvalidationParameter]
//        val config = invalidationRequirements.derivate(BmbParameter(accessRequirements, _))
//        val bus = config.produce(slave(Bmb(config)))
//
//        interconnect.addMaster(
//          accessRequirements = accessRequirements,
//          invalidationSource = invalidationSource,
//          invalidationCapabilities = invalidationCapabilities,
//          invalidationRequirements = invalidationRequirements,
//          bus = bus
//        )
//      }
//
//      val sA = new Area {
//        val accessSource = Handle[BmbAccessParameter]
//        val accessCapabilities = BmbAccessParameter(
//          addressWidth = 32,
//          dataWidth = 16,
//          lengthWidth = 1,
//          sourceWidth = 2,
//          contextWidth = 5
//        )
//        val accessRequirements = Handle[BmbAccessParameter]
//        val invalidationRequirements = accessRequirements produce BmbInvalidationParameter(
//          canInvalidate = false,
//          canSync = false,
//          invalidateLength = 0
//        )
//        val config = List(accessRequirements, invalidationRequirements).produce(BmbParameter(accessRequirements, invalidationRequirements))
//        val bus = config.produce(master(Bmb(config)))
//
//        interconnect.addSlave(
//          accessSource = accessSource,
//          accessCapabilities = accessCapabilities,
//          accessRequirements = accessRequirements,
//          invalidationRequirements = invalidationRequirements,
//          bus = bus,
//          mapping = DefaultMapping
//        )
//      }
//
//      val sB = new Area{
//        val accessSource = Handle[BmbAccessParameter]
//        val accessCapabilities = BmbAccessParameter(
//          addressWidth = 32,
//          dataWidth = 128,
//          lengthWidth = 8,
//          sourceWidth = 2,
//          contextWidth = 5
//        )
//        val accessRequirements = Handle[BmbAccessParameter]
//        val invalidationRequirements = accessRequirements produce BmbInvalidationParameter(
//          canInvalidate    = false,
//          canSync          = false,
//          invalidateLength = 0
//        )
//        val config = List(accessRequirements, invalidationRequirements).produce(BmbParameter(accessRequirements, invalidationRequirements))
//        val bus = config.produce(master(Bmb(config)))
//
//        interconnect.addSlave(
//          accessSource = accessSource,
//          accessCapabilities = accessCapabilities,
//          accessRequirements = accessRequirements,
//          invalidationRequirements = invalidationRequirements,
//          bus = bus,
//          mapping = SizeMapping(0x400, 0x100)
//        )
//      }
//
//      interconnect.addConnection(mA.bus, sA.bus)
//      interconnect.addConnection(mA.bus, sB.bus)
//      interconnect.addConnection(mB.bus, sA.bus)
//      interconnect.addConnection(mB.bus, sB.bus)
//    }
//
//    println(mainGenerator.sA.bus.getName())
//    GeneratorCompiler(mainGenerator)
//  })
//}





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

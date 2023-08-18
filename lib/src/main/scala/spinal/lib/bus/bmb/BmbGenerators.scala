package spinal.lib.bus.bmb

import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib.generator._
import spinal.lib._
import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.amba3.apb.Apb3Config
import spinal.lib.misc.{BmbClint, Clint}
import spinal.lib.misc.plic.{PlicGateway, PlicGatewayActiveHigh, PlicMapper, PlicMapping, PlicTarget}

import scala.collection.mutable.ArrayBuffer
import scala.collection.Seq



case class BmbExclusiveMonitorGenerator()
                                       (implicit interconnect: BmbInterconnectGenerator) extends Area {
  val input = Handle(logic.io.input)
  val output = Handle(logic.io.output)


  val inputAccessSource = Handle[BmbAccessCapabilities]
  val inputAccessRequirements = Handle[BmbAccessParameter]
  val outputInvalidationSource = Handle[BmbInvalidationParameter]
  val invalidationRequirements = Handle[BmbInvalidationParameter]

  interconnect.addSlave(
    accessSource = inputAccessSource,
    accessCapabilities = inputAccessSource,
    accessRequirements = inputAccessRequirements,
    invalidationRequirements = invalidationRequirements,
    bus = input,
    mapping = DefaultMapping
  )

  interconnect.addMaster(
    accessRequirements = inputAccessRequirements.produce(BmbExclusiveMonitor.outputParameter(inputAccessRequirements)),
    invalidationSource = outputInvalidationSource,
    invalidationCapabilities = outputInvalidationSource,
    invalidationRequirements = invalidationRequirements,
    bus = output
  )

  val logic = Handle(BmbExclusiveMonitor(
    inputParameter = BmbParameter(inputAccessRequirements, invalidationRequirements),
    pendingWriteMax = 64
  ))

  sexport(new MemoryConnection(input, output, 0, null))
}

case class BmbInvalidateMonitorGenerator(withExternalInvalidation : Boolean = false)
                                        (implicit interconnect: BmbInterconnectGenerator) extends Area {
  val input = Handle(logic.input)
  val output = Handle(logic.monitor.io.output)

  val inputAccessSource = Handle[BmbAccessCapabilities]
  val inputAccessRequirements = Handle[BmbAccessParameter]
  val inputInvalidationRequirements = Handle(BmbInvalidationParameter(
    invalidateLength = inputAccessRequirements.lengthWidth,
    invalidateAlignment = inputAccessRequirements.alignment
  ))

  interconnect.addSlave(
    accessSource = inputAccessSource,
    accessCapabilities = inputAccessSource,
    accessRequirements = inputAccessRequirements,
    invalidationRequirements = inputInvalidationRequirements,
    bus = input,
    mapping = DefaultMapping
  )

  interconnect.addMaster(
    accessRequirements = inputAccessRequirements.produce(BmbInvalidateMonitor.outputAccessParameter(inputAccessRequirements)),
    bus = output
  )

  val logic = Handle(new Area{
    val monitor = BmbInvalidateMonitor(
      inputParameter = BmbParameter(inputAccessRequirements, inputInvalidationRequirements),
      pendingInvMax = 16
    )

    val input = cloneOf(monitor.io.input)
    if(!withExternalInvalidation){
      input >> monitor.io.input
    }
    val withExt = withExternalInvalidation generate new Area{
      input.cmd >> monitor.io.input.cmd
      input.rsp << monitor.io.input.rsp
      input.sync << monitor.io.input.sync

      val inv = Stream(BmbInv(input.p))
      val ack = Stream(BmbAck(input.p))
      val queue = StreamFifo(Bool(), 16)
      val arbiter = StreamArbiterFactory().transactionLock.roundRobin.build(BmbInv(input.p), 2)
      arbiter.io.inputs(0) << monitor.io.input.inv
      arbiter.io.inputs(1) << inv
      arbiter.io.output.haltWhen(!queue.io.push.ready) >> input.inv

      queue.io.push.valid := input.inv.fire
      queue.io.push.payload := arbiter.io.chosen.asBool

      monitor.io.input.ack.valid := input.ack.valid && !queue.io.pop.payload
      ack.valid                  := input.ack.valid && queue.io.pop.payload
      monitor.io.input.ack.payload := input.ack.payload
      ack.payload                  := input.ack.payload
      queue.io.pop.ready := input.ack.fire
      input.ack.ready := (queue.io.pop.payload ? ack.ready | monitor.io.input.ack.ready)
    }
  })

  sexport(new MemoryConnection(input, output, 0, null))
}

case class BmbClintGenerator(apbOffset : Handle[BigInt] = Unset)
                            (implicit interconnect: BmbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Area {
  val ctrl = Handle(logic.io.bus)
  val stop = Handle(logic.io.stop)
  val cpuCount = Handle[Int]

  val accessSource = Handle[BmbAccessCapabilities]
  val accessRequirements = Handle[BmbAccessParameter]
  val logic = Handle(BmbClint(accessRequirements.toBmbParameter(), cpuCount))
  def timerInterrupt(id : Int) = logic.derivate(_.io.timerInterrupt(id))
  def softwareInterrupt(id : Int) = logic.derivate(_.io.softwareInterrupt(id))

  if(interconnect != null) interconnect.addSlave(
    accessSource = accessSource,
    accessCapabilities = accessSource.derivate(Clint.getBmbCapabilities),
    accessRequirements = accessRequirements,
    bus = ctrl,
    mapping = apbOffset.derivate(SizeMapping(_, 1 << Clint.addressWidth))
  )

  val hz = sexport(Handle(ClockDomain.current.frequency))
  if(decoder != null) interconnect.addConnection(decoder.bus, ctrl)
}



case class BmbPlicGenerator(apbOffset : Handle[BigInt] = Unset) (implicit interconnect: BmbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Area with InterruptCtrlGeneratorI{
  @dontName val gateways = ArrayBuffer[Handle[PlicGateway]]()
  val ctrl = Handle(logic.bmb)

  val accessSource = Handle[BmbAccessCapabilities]
  val accessRequirements = Handle[BmbAccessParameter]

  val priorityWidth = Handle[Int]
  val mapping = Handle[PlicMapping]

  val lock = Lock()

  case class TargetModel(target : Handle[Bool], clockDomain : Handle[ClockDomain])
  val targetsModel = ArrayBuffer[TargetModel]()
  def addTarget(target : Handle[Bool]) = {
    val id = targetsModel.size
    targetsModel += TargetModel(target, ClockDomain.currentHandle)

    //TODO remove the need of delaying stuff for name capture
    Handle(Component.current.addTag(new Export(BmbPlicGenerator.this.getName() + "_" + target.getName, id)))
  }

  override def addInterrupt(source : => Handle[Bool], id : Int) = {
    lock.retain()
    Handle{
      val src = source
      soon(lock)
      gateways += PlicGatewayActiveHigh(
        source = src,
        id = id,
        priorityWidth = priorityWidth
      ).setCompositeName(src, "plic_gateway")

      Component.current.addTag (new Export(BmbPlicGenerator.this.getName() + "_" + src.getName, id))
      lock.release()
    }
  }

  override def getBus(): Handle[Nameable] = ctrl

  val logic = Handle(new Area{
    lock.await()
    val bmb = Bmb(accessRequirements.toBmbParameter())
    val bus = BmbSlaveFactory(bmb)
    val targets = (targetsModel.zipWithIndex).map { case (flag, id) =>
      PlicTarget(
        id = id,
        gateways = gateways.map(_.get),
        priorityWidth = priorityWidth
      ).setCompositeName(flag.target, "plic_target")
    }

    //    gateways.foreach(_.priority := 1)
    //    targets.foreach(_.threshold := 0)
    //    targets.foreach(_.ie.foreach(_ := True))

    val bridge = PlicMapper(bus, mapping)(
      gateways = gateways.map(_.get),
      targets = targets
    )

    for(targetId <- 0 until targetsModel.length){
      val plicCd = ClockDomain.currentHandle
      def bufferize[T <: Data](that : T) : T = if(targetsModel(targetId).clockDomain != ClockDomain.currentHandle) targetsModel(targetId).clockDomain on BufferCC[T](plicCd on RegNext(that), init = null.asInstanceOf[T]) else RegNext[T](that)
      targetsModel(targetId).target := bufferize(targets(targetId).iep)
    }
  })


  if(interconnect != null) interconnect.addSlave(
    accessSource = accessSource,
    accessCapabilities = Handle(BmbSlaveFactory.getBmbCapabilities(
      accessSource,
      addressWidth = 22,
      dataWidth = 32
    )),
    accessRequirements = accessRequirements,
    bus = ctrl,
    mapping = Handle(SizeMapping(apbOffset, 1 << 22))
  )

  if(decoder != null) interconnect.addConnection(decoder.bus, ctrl)
}



object BmbBridgeGenerator{
  def apply(mapping : Handle[AddressMapping] = DefaultMapping)(implicit interconnect: BmbInterconnectGenerator) : BmbBridgeGenerator = new BmbBridgeGenerator(mapping = mapping)
}



class BmbBridgeGenerator(val mapping : Handle[AddressMapping] = DefaultMapping, bypass : Boolean = true)
                        (implicit interconnect: BmbInterconnectGenerator) extends Area {
  val accessSource = Handle[BmbAccessCapabilities]
  val invalidationSource = Handle[BmbInvalidationParameter]

  val accessCapabilities = Handle[BmbAccessCapabilities]
  val invalidationCapabilities = Handle[BmbInvalidationParameter]

  val accessRequirements = Handle[BmbAccessParameter]
  val invalidationRequirements = Handle[BmbInvalidationParameter]
  val bmb = Handle(Bmb(accessRequirements, invalidationRequirements))

  val accessTranform = ArrayBuffer[BmbAccessCapabilities => BmbAccessCapabilities]()

  def dataWidth(w : Int): this.type = {
    accessTranform += { a => a.copy(
      dataWidth = w
    )}
    this
  }
  def unburstify(): this.type = {
    accessTranform += { a => a.copy(
      alignment =  BmbParameter.BurstAlignement.WORD,
      lengthWidthMax = log2Up(a.dataWidth/8)
    )}
    this
  }
  def withoutMask(): this.type = {
    accessTranform += { a => a.copy(
      canMask =  false
    )}
    this
  }
  def peripheral(dataWidth : Int): this.type = {
    this.dataWidth(dataWidth)
    this.unburstify()
  }
  def asPeripheralDecoder(dataWidth : Int) : BmbImplicitPeripheralDecoder = {
    peripheral(dataWidth)
    asPeripheralDecoder()
  }

  def asPeripheralDecoder() : BmbImplicitPeripheralDecoder = {
    interconnect.masters(bmb).withPeripheralDecoder()
    BmbImplicitPeripheralDecoder(bmb)
  }

  if(bypass){
    accessCapabilities.loadAsync{
      accessTranform.foldLeft(accessSource)((v, f) => f(v))
    }
    invalidationCapabilities.load(invalidationSource)
  }

  interconnect.addSlave(
    accessSource = accessSource,
    accessCapabilities = accessCapabilities,
    accessRequirements = accessRequirements,
    invalidationRequirements = invalidationRequirements,
    bus = bmb,
    mapping = mapping
  )

  interconnect.addMaster(
    accessRequirements = accessRequirements,
    invalidationSource = invalidationSource,
    invalidationCapabilities = invalidationCapabilities,
    invalidationRequirements = invalidationRequirements,
    bus = bmb
  )
}


case class BmbToApb3Generator(mapping : Handle[AddressMapping] = Unset)
                             (implicit interconnect: BmbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Area {
  val input = Handle(logic.io.input)
  val output = Handle(logic.io.output)

  val apb3Config = Handle[Apb3Config]
  val accessSource = Handle[BmbAccessCapabilities]
  val accessRequirements = Handle[BmbAccessParameter]
  val logic = Handle(BmbToApb3Bridge(
    apb3Config = apb3Config,
    bmbParameter = accessRequirements.toBmbParameter(),
    pipelineBridge = false
  ))

  interconnect.addSlave(
    accessSource = accessSource,
    accessCapabilities = accessSource.derivate(
      _.copy(
        addressWidth = apb3Config.addressWidth,
        dataWidth = apb3Config.dataWidth,
        lengthWidthMax = log2Up(apb3Config.dataWidth/8),
        alignment = BmbParameter.BurstAlignement.LENGTH,
        canMask = false
      )
    ),
    accessRequirements = accessRequirements,
    bus = input,
    mapping = mapping
  )

  if(decoder != null) interconnect.addConnection(decoder.bus, input)
}
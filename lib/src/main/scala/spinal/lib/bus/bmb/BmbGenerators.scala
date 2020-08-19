package spinal.lib.bus.bmb

import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib.generator._
import spinal.lib._
import spinal.core._
import spinal.lib.bus.amba3.apb.Apb3Config
import spinal.lib.misc.{BmbClint, Clint}
import spinal.lib.misc.plic.{PlicGateway, PlicGatewayActiveHigh, PlicMapper, PlicMapping, PlicTarget}

import scala.collection.mutable.ArrayBuffer




case class BmbExclusiveMonitorGenerator()
                                       (implicit interconnect: BmbInterconnectGenerator) extends Generator {
  val input = produce(logic.io.input)
  val output = produce(logic.io.output)


  val inputAccessSource = Handle[BmbAccessCapabilities]
  val inputAccessRequirements = createDependency[BmbAccessParameter]
  val outputInvalidationSource = Handle[BmbInvalidationParameter]
  val invalidationRequirements = createDependency[BmbInvalidationParameter]

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

  val logic = add task BmbExclusiveMonitor(
    inputParameter = BmbParameter(inputAccessRequirements, invalidationRequirements),
    pendingWriteMax = 64
  )

  tags += new MemoryConnection(input, output, 0)
}

case class BmbInvalidateMonitorGenerator()
                                        (implicit interconnect: BmbInterconnectGenerator) extends Generator {
  val input = produce(logic.io.input)
  val output = produce(logic.io.output)

  val inputAccessSource = Handle[BmbAccessCapabilities]
  val inputAccessRequirements = createDependency[BmbAccessParameter]
  val inputInvalidationRequirements = createDependency[BmbInvalidationParameter]

  inputInvalidationRequirements.derivatedFrom(inputAccessRequirements)(r => BmbInvalidationParameter(
    canInvalidate = true,
    canSync = true,
    invalidateLength = r.lengthWidth,
    invalidateAlignment = r.alignment
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

  val logic = add task BmbInvalidateMonitor(
    inputParameter = BmbParameter(inputAccessRequirements, inputInvalidationRequirements),
    pendingInvMax = 16
  )

  tags += new MemoryConnection(input, output, 0)
}

case class BmbClintGenerator(apbOffset : Handle[BigInt] = Unset)
                            (implicit interconnect: BmbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Generator {
  val ctrl = produce(logic.io.bus)
  val cpuCount = createDependency[Int]

  val accessSource = Handle[BmbAccessCapabilities]
  val accessRequirements = createDependency[BmbAccessParameter]
  val logic = add task BmbClint(accessRequirements.toBmbParameter(), cpuCount)
  def timerInterrupt(id : Int) = logic.derivate(_.io.timerInterrupt(id))
  def softwareInterrupt(id : Int) = logic.derivate(_.io.softwareInterrupt(id))

  if(interconnect != null) interconnect.addSlave(
    accessSource = accessSource,
    accessCapabilities = accessSource.derivate(Clint.getBmbCapabilities),
    accessRequirements = accessRequirements,
    bus = ctrl,
    mapping = apbOffset.derivate(SizeMapping(_, 1 << Clint.addressWidth))
  )

  val hz = export(produce(ClockDomain.current.frequency))
  if(decoder != null) interconnect.addConnection(decoder.bus, ctrl)
}



case class BmbPlicGenerator(apbOffset : Handle[BigInt] = Unset) (implicit interconnect: BmbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Generator with InterruptCtrlGeneratorI{
  @dontName val gateways = ArrayBuffer[Handle[PlicGateway]]()
  val ctrl = produce(logic.bmb)

  val accessSource = Handle[BmbAccessCapabilities]
  val accessRequirements = createDependency[BmbAccessParameter]

  val priorityWidth = createDependency[Int]
  val mapping = createDependency[PlicMapping]

  val targetsModel = ArrayBuffer[Handle[Bool]]()
  def addTarget(target : Handle[Bool]) = {
    val id = targetsModel.size

    targetsModel += target
    dependencies += target

    //TODO remove the need of delaying stuff for name capture
    add task(tags += new Export(BmbPlicGenerator.this.getName() + "_" + target.getName, id))
  }

  override def addInterrupt(source : Handle[Bool], id : Int) = {
    this.dependencies += new Generator {
      dependencies += source
      add task new Area {
        gateways += PlicGatewayActiveHigh(
          source = source,
          id = id,
          priorityWidth = priorityWidth
        ).setCompositeName(source, "plic_gateway")

        tags += new Export(BmbPlicGenerator.this.getName() + "_" + source.getName, id)
      }
    }
  }

  override def getBus(): Handle[Nameable] = ctrl

  val logic = add task new Area{
    val bmb = Bmb(accessRequirements.toBmbParameter())
    val bus = BmbSlaveFactory(bmb)

    val targets = targetsModel.map(flag =>
      PlicTarget(
        gateways = gateways.map(_.get),
        priorityWidth = priorityWidth
      ).setCompositeName(flag, "plic_target")
    )

    //    gateways.foreach(_.priority := 1)
    //    targets.foreach(_.threshold := 0)
    //    targets.foreach(_.ie.foreach(_ := True))

    val bridge = PlicMapper(bus, mapping)(
      gateways = gateways.map(_.get),
      targets = targets
    )

    for(targetId <- 0 until targetsModel.length){
      targetsModel(targetId) := targets(targetId).iep
    }
  }


  if(interconnect != null) interconnect.addSlave(
    accessSource = accessSource,
    accessCapabilities = accessSource.derivate(BmbSlaveFactory.getBmbCapabilities(
      _,
      addressWidth = 22,
      dataWidth = 32
    )),
    accessRequirements = accessRequirements,
    bus = ctrl,
    mapping = apbOffset.derivate(SizeMapping(_, 1 << 22))
  )

  if(decoder != null) interconnect.addConnection(decoder.bus, ctrl)
}



object BmbBridgeGenerator{
  def apply(mapping : Handle[AddressMapping] = DefaultMapping)(implicit interconnect: BmbInterconnectGenerator) : BmbBridgeGenerator = new BmbBridgeGenerator(mapping = mapping)
}



class BmbBridgeGenerator(mapping : Handle[AddressMapping] = DefaultMapping, bypass : Boolean = true)
                        (implicit interconnect: BmbInterconnectGenerator) extends Generator {
  val accessSource = Handle[BmbAccessCapabilities]
  val invalidationSource = Handle[BmbInvalidationParameter]

  val accessCapabilities = Handle[BmbAccessCapabilities]
  val invalidationCapabilities = Handle[BmbInvalidationParameter]

  val accessRequirements = createDependency[BmbAccessParameter]
  val invalidationRequirements = createDependency[BmbInvalidationParameter]
  val bmb = add task Bmb(accessRequirements, invalidationRequirements)

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
  def peripheral(dataWidth : Int): this.type = {
    this.dataWidth(dataWidth)
    this.unburstify()
  }
  def asPeripheralDecoder(dataWidth : Int) = {
    peripheral(dataWidth)
    BmbImplicitPeripheralDecoder(bmb)
  }
  def asPeripheralDecoder() = {
    BmbImplicitPeripheralDecoder(bmb)
  }

  if(bypass){
    accessCapabilities.derivatedFrom(accessSource){
      accessTranform.foldLeft(_)((v, f) => f(v))
    }
    invalidationCapabilities.merge(invalidationSource)
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
                             (implicit interconnect: BmbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Generator {
  val input = produce(logic.io.input)
  val output = produce(logic.io.output)

  val apb3Config = createDependency[Apb3Config]
  val accessSource = createDependency[BmbAccessCapabilities]
  val accessRequirements = createDependency[BmbAccessParameter]
  val logic = add task BmbToApb3Bridge(
    apb3Config = apb3Config,
    bmbParameter = accessRequirements.toBmbParameter(),
    pipelineBridge = false
  )

  interconnect.addSlave(
    accessSource = accessSource,
    accessCapabilities = accessSource.derivate(Clint.getBmbCapabilities),
    accessRequirements = accessRequirements,
    bus = input,
    mapping = mapping
  )

  if(decoder != null) interconnect.addConnection(decoder.bus, input)
}
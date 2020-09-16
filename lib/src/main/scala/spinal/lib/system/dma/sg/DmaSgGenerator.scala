package spinal.lib.system.dma.sg

import spinal.core._
import spinal.lib.bus.bmb._
import spinal.lib.bus.bsb.{Bsb, BsbInterconnectGenerator, BsbParameter}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.generator._
import spinal.lib.system.dma.sg.DmaSg.Channel

import scala.collection.mutable.ArrayBuffer

class DmaSgGenerator(ctrlOffset : Handle[BigInt] = Unset)
                         (implicit interconnect: BmbInterconnectGenerator, bsbInterconnect : BsbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Generator{
  val ctrl = produce(logic.io.ctrl)
  val write = produce(logic.io.write)
  val read = produce(logic.io.read)
  val writeSg = produce(logic.io.sgWrite)
  val readSg = produce(logic.io.sgRead)
  val interrupts = produce(logic.io.interrupts)
  val interrupt = produce(logic.io.interrupts.orR)

  val parameter = new Generator{
    interconnect.dependencies += this

    val readAddressWidth = createDependency[Int]
    val readDataWidth = createDependency[Int]
    val readLengthWidth = createDependency[Int]
    val writeAddressWidth = createDependency[Int]
    val writeDataWidth = createDependency[Int]
    val writeLengthWidth = createDependency[Int]
    val pendingWritePerChannel = createDependency[Int]
    val pendingReadPerChannel = createDependency[Int]
    val bytePerTransferWidth = createDependency[Int]
    val sgAddressWidth = createDependency[Int]
    val sgReadDataWidth = createDependency[Int]
    val sgWriteDataWidth = createDependency[Int]
    val layout = createDependency[DmaMemoryLayout]

    val p = add task DmaSg.Parameter(
      readAddressWidth = readAddressWidth,
      readDataWidth = readDataWidth,
      readLengthWidth = readLengthWidth,
      writeAddressWidth = writeAddressWidth,
      writeDataWidth = writeDataWidth,
      writeLengthWidth = writeLengthWidth,
      sgAddressWidth = sgAddressWidth,
      sgReadDataWidth = sgReadDataWidth,
      sgWriteDataWidth = sgWriteDataWidth,
      memory = layout,
      outputs = outputs.map(o => BsbParameter(
        byteCount = o.im.byteCount,
        sourceWidth = o.im.sourceWidth,
        sinkWidth = o.im.sinkWidth,
        withMask = o.im.withMask
      )),
      inputs =  Nil,//inputs.map(_.input.p),
      channels = channels.map(c => Channel(
        memoryToMemory          = c.memoryToMemory,
        inputsPorts             = Nil,
        outputsPorts            = c.outputsPorts.map(_.id),
        linkedListCapable       = c.linkedListCapable,
        directCtrlCapable       = c.directCtrlCapable,
        selfRestartCapable      = c.selfRestartCapable,
        progressProbes          = c.progressProbes,
        halfCompletionInterrupt = c.halfCompletionInterrupt,
        bytePerBurst            = c.bytePerBurst,
        fifoMapping             = c.fifoMapping
      )),
      bytePerTransferWidth = bytePerTransferWidth,
      pendingWritePerChannel = pendingWritePerChannel,
      pendingReadPerChannel = pendingReadPerChannel
    )

    add task {
      if(p.canWrite) interconnect.addMaster(
        accessRequirements = DmaSg.getWriteRequirements(p),
        bus = write
      )
      if(p.canRead) interconnect.addMaster(
        accessRequirements = DmaSg.getReadRequirements(p),
        bus = read
      )
      if(p.canSgWrite) interconnect.addMaster(
        accessRequirements = DmaSg.getSgWriteRequirements(p),
        bus = writeSg
      )
      if(p.canSgRead) interconnect.addMaster(
        accessRequirements = DmaSg.getSgReadRequirements(p),
        bus = readSg
      )
    }
  }

  dependencies += parameter.p



  def setBmbParameter(addressWidth : Int, dataWidth : Int, lengthWidth : Int): Unit ={
    parameter.readAddressWidth.load(addressWidth)
    parameter.readDataWidth.load(dataWidth)
    parameter.readLengthWidth.load(lengthWidth)
    parameter.writeAddressWidth.load(addressWidth)
    parameter.writeDataWidth.load(dataWidth)
    parameter.writeLengthWidth.load(lengthWidth)
    parameter.sgAddressWidth.load(addressWidth)
    parameter.sgReadDataWidth.load(dataWidth)
    parameter.sgWriteDataWidth.load(dataWidth)
    parameter.pendingWritePerChannel.load(8)
    parameter.pendingReadPerChannel.load(8)
    parameter.bytePerTransferWidth.load(26)
  }

  def createInput(): Handle[Bsb] ={
    val model = InputModel()
    model.input
  }

  def createOutput(byteCount : Int): OutputModel ={
    val model = OutputModel(bsbInterconnect)
    model.im.byteCount.load(byteCount)
    model
  }

  def createChannel() : ChannelModel = {
    new ChannelModel
  }

  def connectInterrupts(ctrl : InterruptCtrlGeneratorI, offsetId : Int): Unit = {
    ctrl.addInterrupt(interrupt, offsetId)
  }

  val inputs = ArrayBuffer[InputModel]()
  case class InputModel() extends Area{
    val id = inputs.size
    val byteCount = createDependency[Int]
    val sourceWidth = createDependency[Int]
    val sinkWidth = createDependency[Int]
    val withMask = createDependency[Boolean]
    val input = produce(logic.io.inputs(id))
//    val inputModel = bsbInterconnect.addSlave(
//      bsb = input,
//      byteCount = byteCount,
//      sourceWidth = sourceWidth,
//      sinkWidth = sinkWidth,
//      withMask = withMask
//    )
    inputs += this
  }


  val outputs = ArrayBuffer[OutputModel]()
  case class OutputModel(bsbInterconnect : BsbInterconnectGenerator) extends Generator{
    val id = outputs.size
    val output = DmaSgGenerator.this.produce(logic.io.outputs(id))
    val im = bsbInterconnect.addMaster(bsb = output)

    im.sourceWidth.load(0)
    im.withMask.load(true)

    parameter.dependencies += this
    parameter.dependencies += im.sinkWidth
    parameter.dependencies += im.byteCount
    outputs += this
  }


  val channels = ArrayBuffer[ChannelModel]()
  case class ChannelModel() extends Generator{
    val id = channels.size
    channels += this
    parameter.dependencies += this

    val interrupt = DmaSgGenerator.this.interrupts.derivate(_(id))
    def connectInterrupt(ctrl : InterruptCtrlGeneratorI, offsetId : Int): Unit = {
      ctrl.addInterrupt(interrupt, offsetId)
    }

    val memoryToMemory = createDependency[Boolean]
    val linkedListCapable = createDependency[Boolean]
    val directCtrlCapable = createDependency[Boolean]
    val selfRestartCapable = createDependency[Boolean]
    val progressProbes = createDependency[Boolean]
    val halfCompletionInterrupt = createDependency[Boolean]
    val bytePerBurst = createDependency[Option[Int]]
    val fifoMapping = createDependency[Option[(Int, Int)]]
    //    val inputsPorts = createDependency[Seq[Int]]
    val outputsPorts = ArrayBuffer[OutputModel]()

    memoryToMemory.load(false)
    linkedListCapable.load(false)
    directCtrlCapable.load(false)
    selfRestartCapable.load(false)
    progressProbes.load(false)
    halfCompletionInterrupt.load(false)

    def withCircularMode(): Unit ={
      directCtrlCapable load true
      selfRestartCapable load true
      halfCompletionInterrupt load false
    }

    def withScatterGatter(): Unit ={
      linkedListCapable load true
    }

    def fixedBurst(bytePerBurst : Int): Unit ={
      this.bytePerBurst load Some(bytePerBurst)
    }
  }


//   outputs : Seq[BsbParameter]
//    inputs : Seq[BsbParameter],
//    channels : Seq[Channel],

  val logic : Handle[DmaSg.Core[Bmb]] = add task DmaSg.Core[Bmb](
    p = parameter.p,
    ctrlType = HardType(Bmb(accessRequirements.toBmbParameter())),
    slaveFactory = BmbSlaveFactory(_)
  )


  val accessSource = Handle[BmbAccessCapabilities]
  val accessRequirements = createDependency[BmbAccessParameter]
  interconnect.addSlave(
    accessSource = accessSource,
    accessCapabilities = accessSource.derivate(DmaSg.getCtrlCapabilities),
    accessRequirements = accessRequirements,
    bus = ctrl,
    mapping = ctrlOffset.derivate(SizeMapping(_, 1 << DmaSg.ctrlAddressWidth))
  )
  if(decoder != null) interconnect.addConnection(decoder.bus, ctrl)
}

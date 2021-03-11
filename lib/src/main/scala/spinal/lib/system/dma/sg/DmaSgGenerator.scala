package spinal.lib.system.dma.sg

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.bmb._
import spinal.lib.bus.bsb.{Bsb, BsbInterconnectGenerator, BsbParameter}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.generator._
import spinal.lib.system.dma.sg.DmaSg.Channel

import scala.collection.mutable.ArrayBuffer

class DmaSgGenerator(ctrlOffset : Handle[BigInt] = Unset)
                         (implicit interconnect: BmbInterconnectGenerator, bsbInterconnect : BsbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Area{
  val ctrl       = Handle(logic.io.ctrl)
  val write      = Handle(logic.io.write)
  val read       = Handle(logic.io.read)
  val writeSg    = Handle(logic.io.sgWrite)
  val readSg     = Handle(logic.io.sgRead)
  val interrupts = Handle(logic.io.interrupts)
  val interrupt  = Handle(logic.io.interrupts.orR)

  val parameter = new Area{
    interconnect.lock.retain()

    val readAddressWidth        = Handle[Int]
    val readDataWidth           = Handle[Int]
    val readLengthWidth         = Handle[Int]
    val writeAddressWidth       = Handle[Int]
    val writeDataWidth          = Handle[Int]
    val writeLengthWidth        = Handle[Int]
    val pendingWritePerChannel  = Handle[Int]
    val pendingReadPerChannel   = Handle[Int]
    val bytePerTransferWidth    = Handle[Int]
    val sgAddressWidth          = Handle[Int]
    val sgReadDataWidth         = Handle[Int]
    val sgWriteDataWidth        = Handle[Int]
    val layout                  = Handle[DmaMemoryLayout]

    val p = Handle(DmaSg.Parameter(
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
      pendingReadPerChannel = pendingReadPerChannel,
      weightWidth = 2
    ))

    Handle {
      soon(interconnect.lock)
      if(p.canWrite) interconnect.addMaster(
        accessRequirements = Handle(DmaSg.getWriteRequirements(p)),
        bus = write
      )
      if(p.canRead) interconnect.addMaster(
        accessRequirements = Handle(DmaSg.getReadRequirements(p)),
        bus = read
      )
      if(p.canSgWrite) interconnect.addMaster(
        accessRequirements = Handle(DmaSg.getSgWriteRequirements(p)),
        bus = writeSg
      )
      if(p.canSgRead) interconnect.addMaster(
        accessRequirements = Handle(DmaSg.getSgReadRequirements(p)),
        bus = readSg
      )
      interconnect.lock.release()
    }
  }


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
    val byteCount = Handle[Int]
    val sourceWidth = Handle[Int]
    val sinkWidth = Handle[Int]
    val withMask = Handle[Boolean]
    val input = Handle(logic.io.inputs(id))
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
  case class OutputModel(bsbInterconnect : BsbInterconnectGenerator) extends Area{
    val id = outputs.size
    val output = Handle(logic.io.outputs(id))
    val im = bsbInterconnect.addMaster(bsb = output)

    im.sourceWidth.load(0)
    im.withMask.load(true)

    outputs += this
  }


  val channels = ArrayBuffer[ChannelModel]()
  case class ChannelModel() extends Area{
    val id = channels.size
    channels += this

    val interrupt = DmaSgGenerator.this.interrupts.derivate(_(id))
    def connectInterrupt(ctrl : InterruptCtrlGeneratorI, offsetId : Int): Unit = {
      ctrl.addInterrupt(interrupt, offsetId)
    }

    val memoryToMemory = Handle[Boolean]
    val linkedListCapable = Handle[Boolean]
    val directCtrlCapable = Handle[Boolean]
    val selfRestartCapable = Handle[Boolean]
    val progressProbes = Handle[Boolean]
    val halfCompletionInterrupt = Handle[Boolean]
    val bytePerBurst = Handle[Option[Int]]
    val fifoMapping = Handle[Option[(Int, Int)]]
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

  val logic : Handle[DmaSg.Core[Bmb]] = Handle(DmaSg.Core[Bmb](
    p = parameter.p,
    ctrlType = HardType(Bmb(accessRequirements.toBmbParameter())),
    slaveFactory = BmbSlaveFactory(_)
  ))


  val accessSource = Handle[BmbAccessCapabilities]
  val accessRequirements = Handle[BmbAccessParameter]
  interconnect.addSlave(
    accessSource = accessSource,
    accessCapabilities = accessSource.derivate(DmaSg.getCtrlCapabilities),
    accessRequirements = accessRequirements,
    bus = ctrl,
    mapping = Handle(SizeMapping(ctrlOffset, 1 << DmaSg.ctrlAddressWidth))
  )
  if(decoder != null) interconnect.addConnection(decoder.bus, ctrl)
}

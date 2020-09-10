package spinal.lib.misc.analog

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAccessCapabilities, BmbAccessParameter, BmbImplicitPeripheralDecoder, BmbInterconnectGenerator, BmbParameter, BmbSlaveFactory}
import spinal.lib.bus.bsb.{Bsb, BsbInterconnectGenerator, BsbParameter}
import spinal.lib.bus.misc.{BusSlaveFactory, SizeMapping}
import spinal.lib.generator._

case class UIntToSigmaDelta(inputWidth : Int)  extends Component{
  val io = new Bundle{
    val input = in UInt(inputWidth bits)
    val output = out Bool()
  }

  val accumulator = Reg(UInt(inputWidth bits)) randBoot()
  val adder = accumulator +^ io.input
  accumulator := adder.resized
  io.output := RegNext(adder.msb)
}

case class BsbToDeltaSigmaParameter(channels : Int,
                                    channelWidth : Int,
                                    rateWidth : Int)

case class BsbToDeltaSigma(p : BsbToDeltaSigmaParameter, inputParameter : BsbParameter) extends Component{
  val io = new Bundle{
    val run = in Bool()
    val rate = in UInt(p.rateWidth bits)
    val input = slave(Bsb(inputParameter))
    val outputs = out Bits(p.channels bits)

    def driveFrom(ctrl : BusSlaveFactory): Unit ={
      ctrl.drive(run,  0x10, 0)
      ctrl.drive(rate, 0x14)
    }
  }

  val decoder = new Area{
    val input = io.input.toStream(omitMask = true) //TODO
    val output = Stream(Vec(UInt(p.channelWidth bits), p.channels))
    StreamWidthAdapter(input, output)
  }

  val sampler = new Area{
    val counter = Reg(UInt(p.rateWidth bits)) randBoot()
    val fire = counter === io.rate || !io.run
    counter := counter + 1
    when(fire){
      counter := 0
    }

    val state = decoder.output.stage.continueWhen(fire).toFlow.toReg
  }

  val channels = for(channelId <- 0 until p.channels) yield new Area{
    val toSigmaDelta = UIntToSigmaDelta(p.channelWidth)
    toSigmaDelta.io.input <> sampler.state(channelId)
    toSigmaDelta.io.output <> io.outputs(channelId)
  }
}

object BmbBsbToDeltaSigma{
  def getBmbCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = addressWidth,
    dataWidth = 32
  )
  def addressWidth = 12
}

case class BmbBsbToDeltaSigma(p : BsbToDeltaSigmaParameter,
                              inputParameter : BsbParameter,
                              bmbParameter : BmbParameter) extends Component{
  val io = new Bundle {
    val ctrl = slave(Bmb(bmbParameter))
    val input = slave(Bsb(inputParameter))
    val outputs = out Bits(p.channels bits)
  }
  val ctrl = BmbSlaveFactory(io.ctrl)

  val core = BsbToDeltaSigma(p, inputParameter)
  core.io.driveFrom(ctrl)
  core.io.input <> io.input
  core.io.outputs <> io.outputs
}



case class BmbBsbToDeltaSigmaGenerator(ctrlOffset : Handle[BigInt] = Unset)
                                      (implicit val interconnect: BmbInterconnectGenerator, val bsbInterconnect : BsbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Generator{
  val ctrl = produce(logic.io.ctrl)
  val input = produce(logic.io.input)
  val outputs = produceIo(logic.io.outputs)
  val parameter = createDependency[BsbToDeltaSigmaParameter]

  val logic : Handle[BmbBsbToDeltaSigma] = add task BmbBsbToDeltaSigma(
    p              = parameter,
    bmbParameter  = accessRequirements.toBmbParameter(),
    inputParameter = BsbParameter(
      byteCount = is.byteCount,
      sourceWidth = is.sourceWidth,
      sinkWidth = is.sinkWidth,
      withMask = is.withMask
    )
  )


  val accessSource = Handle[BmbAccessCapabilities]
  val accessRequirements = createDependency[BmbAccessParameter]
  interconnect.addSlave(
    accessSource = accessSource,
    accessCapabilities = accessSource.derivate(BmbBsbToDeltaSigma.getBmbCapabilities),
    accessRequirements = accessRequirements,
    bus = ctrl,
    mapping = ctrlOffset.derivate(SizeMapping(_, 1 << BmbBsbToDeltaSigma.addressWidth))
  )
  if(decoder != null) interconnect.addConnection(decoder.bus, ctrl)
  val is = bsbInterconnect.addSlave(input)
  dependencies += List(is.byteCount, is.sourceWidth, is.withMask)
  is.sinkWidth.load(0)
}
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
  io.output := adder.msb
}

case class BsbToDeltaSigmaParameter(channels : Int,
                                    channelWidth : Int,
                                    rateWidth : Int)

case class BsbToDeltaSigma(p : BsbToDeltaSigmaParameter, inputParameter : BsbParameter) extends Component{
  val io = new Bundle{
    val channelCount = in UInt(log2Up(p.channels + 1) bits)
    val rate = in UInt(p.rateWidth bits)
    val input = slave(Bsb(inputParameter))
    val outputs = out Bits(p.channels bits)

    def driveFrom(ctrl : BusSlaveFactory): Unit ={
      ctrl.drive(channelCount,  0x10, 0)
      ctrl.drive(rate, 0x14)
    }
  }

  val decoder = new Area{
    val input = io.input.toStream(omitMask = true) //TODO
    val output, adapter = Stream(Vec(UInt(p.channelWidth bits), p.channels))
    StreamWidthAdapter(input, adapter)

    assert(p.channels >= 1 && p.channels <= 2)
    if(p.channels == 1) output << adapter
    val gearbox = if(p.channels == 2) new Area{
      val counter = Reg(UInt(1 bits))
      when(output.fire){
        counter := counter + 1
      }
      when(io.channelCount === 0){
        counter := 0
      }

      output.valid := adapter.valid
      output.payload.assignDontCare()
      adapter.ready := output.ready
      switch(io.channelCount){
        is(1){
          val sel = adapter.payload(counter)
          output.payload(0) := sel
          output.payload(1) := sel
          adapter.ready clearWhen(counter =/= 1)
        }
        is(2){
          output.payload(0) := adapter.payload(0)
          output.payload(1) := adapter.payload(1)
        }
      }
    }
  }

  val sampler = new Area{
    val counter = Reg(UInt(p.rateWidth bits)) randBoot()
    val fire = counter === io.rate || io.channelCount === 0
    counter := counter + 1
    when(fire){
      counter := 0
    }

    val state = decoder.output.stage.continueWhen(fire).toFlow.toReg
  }

  val channels = for(channelId <- 0 until p.channels) yield new Area{
    val toSigmaDelta = UIntToSigmaDelta(p.channelWidth)
    toSigmaDelta.io.input <> (sampler.state(channelId) ^ (BigInt(1) << p.channelWidth-1))

    val buffer = Reg(Bool)
    buffer := toSigmaDelta.io.output && io.channelCount =/= 0

    io.outputs(channelId) := buffer
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
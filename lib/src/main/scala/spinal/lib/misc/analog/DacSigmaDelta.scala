package spinal.lib.misc.analog

import spinal.core._
import spinal.lib._
import spinal.core.fiber._
import spinal.lib.bus.bmb._
import spinal.lib.bus.bsb.{Bsb, BsbDownSizerAlignedMultiWidth, BsbInterconnectGenerator, BsbParameter}
import spinal.lib.bus.misc.{BusSlaveFactory, SizeMapping}
import spinal.lib.generator._

case class UIntToSigmaDeltaFirstOrder(inputWidth : Int)  extends Component{
  val io = new Bundle{
    val input = in UInt(inputWidth bits)
    val output = out Bool()
  }

  val accumulator = Reg(UInt(inputWidth + 1 bits)) randBoot()
  val counter = CounterFreeRun(3)
  when(counter === 2) {
    accumulator := accumulator.resize(inputWidth) +^ io.input
  }

  val symbol = accumulator.msb ? B"110" | B"100"
  io.output := symbol(counter)
}

case class SIntToSigmaDeltaSecondOrder(inputWidth : Int)  extends Component{
  val io = new Bundle{
    val input = in SInt(inputWidth bits)
    val output = out Bool()
  }
  val bitstream = Bool()
  val inputScaled =  bitstream ? S(-(1 << inputWidth-1)) | S(1 << inputWidth-1)
  val acc1, acc2 = Reg(SInt(inputWidth+16 bits)) randBoot()

  val acc1Next = acc1 + io.input + inputScaled
  val acc2Next = acc2 + acc1Next + inputScaled


  val counter = CounterFreeRun(3)
  when(counter === 2) {
    acc1 := acc1Next
    acc2 := acc2Next
  }

  bitstream := !acc2.msb

  val symbol = bitstream ? B"110" | B"100"
  io.output := symbol(counter)
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
    val downSizerMods = p.channels match {
      case 1 => List(p.channelWidth/8)
      case 2 => List(p.channelWidth/8, 2*p.channelWidth/8)
    }

    val downSizer = BsbDownSizerAlignedMultiWidth(io.input.p, downSizerMods)
    downSizer.io.sel := (io.channelCount-1).resized
    downSizer.io.input << io.input

    val toStream = downSizer.io.output.throwWhen(!downSizer.io.output.mask.lsb).toStream(omitMask = true)
    val output, adapter = Stream(Vec(UInt(p.channelWidth bits), p.channels))

    adapter.arbitrationFrom(toStream)
    adapter.payload.assignFromBits(toStream.payload)


    assert(p.channels >= 1 && p.channels <= 2)

    val mono = if(p.channels == 1) new Area{
      output << adapter
    }

    val stereo = if(p.channels == 2) new Area{
      output.arbitrationFrom(adapter)
      output.payload.assignDontCare()
      switch(io.channelCount){
        is(1){
          output.payload(0) := adapter.payload(0)
          output.payload(1) := adapter.payload(0)
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
    val toSigmaDelta = UIntToSigmaDeltaFirstOrder(p.channelWidth)
    toSigmaDelta.io.input <> (sampler.state(channelId) ^ (BigInt(1) << p.channelWidth-1))

//    val toSigmaDelta = UIntToSigmaDeltaSecondOrder(p.channelWidth)
//      toSigmaDelta.io.input <> S(sampler.state(channelId))

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
                                      (implicit val interconnect: BmbInterconnectGenerator, val bsbInterconnect : BsbInterconnectGenerator, decoder : BmbImplicitPeripheralDecoder = null) extends Area{
  val ctrl = Handle(logic.io.ctrl)
  val input = Handle(logic.io.input)
  val outputs = Handle(logic.io.outputs.toIo)
  val parameter = Handle[BsbToDeltaSigmaParameter]

  val logic : Handle[BmbBsbToDeltaSigma] = Handle(BmbBsbToDeltaSigma(
    p              = parameter,
    bmbParameter  = accessRequirements.toBmbParameter(),
    inputParameter = BsbParameter(
      byteCount = is.byteCount,
      sourceWidth = is.sourceWidth,
      sinkWidth = is.sinkWidth,
      withMask = is.withMask
    )
  ))


  val accessSource = Handle[BmbAccessCapabilities]
  val accessRequirements = Handle[BmbAccessParameter]
  interconnect.addSlave(
    accessSource = accessSource,
    accessCapabilities = accessSource.derivate(BmbBsbToDeltaSigma.getBmbCapabilities),
    accessRequirements = accessRequirements,
    bus = ctrl,
    mapping = ctrlOffset.derivate(SizeMapping(_, 1 << BmbBsbToDeltaSigma.addressWidth))
  )
  if(decoder != null) interconnect.addConnection(decoder.bus, ctrl)
  val is = bsbInterconnect.addSlave(input)
  is.sinkWidth.load(0)
}
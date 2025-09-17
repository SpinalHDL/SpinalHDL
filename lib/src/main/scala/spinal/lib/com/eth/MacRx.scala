package spinal.lib.com.eth

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.fsm.{State, StateMachine}


case class MacRxPreamble(dataWidth : Int) extends Component{
  val io = new Bundle {
    val input = slave(Stream(Fragment(PhyRx(dataWidth))))
    val output = master(Stream(Fragment(PhyRx(dataWidth))))
  }

  val startDelimiter = 0xD5
  val startDelimiterWidth = 8
  val history = History(io.input, 0 until startDelimiterWidth/dataWidth, when = io.input.fire)
  val historyDataCat = B(Cat(history.map(_.data).reverse))
  val hit = history.map(_.valid).andR && historyDataCat === startDelimiter
  val inFrame = RegInit(False)

  io.output.valid := False
  io.output.payload  := io.input.payload
  io.input.ready := !inFrame || io.output.ready

  when(!inFrame){
    when(hit){
      inFrame := True
    }
  } otherwise {
    when(io.input.valid) {
      io.output.valid := True
      when(io.output.ready && io.input.last){
        inFrame := False
      }
    }
  }
}


case class CrcKind(polynomial      : BigInt,
                   polynomialWidth : Int,
                   initValue       : BigInt,
                   inputReflected  : Boolean,
                   outputReflected : Boolean,
                   finalXor        : BigInt)

object CrcKind{
  val Crc32 =  new CrcKind(
    polynomial = BigInt("04C11DB7", 16),
    polynomialWidth = 32,
    initValue = BigInt("FFFFFFFF", 16),
    inputReflected = true,
    outputReflected = true,
    finalXor = BigInt("FFFFFFFF", 16)
  )
  val usb = new {
    val crc5 = new CrcKind(
      polynomial = BigInt("5", 16),
      polynomialWidth = 5,
      initValue = BigInt("1F", 16),
      inputReflected = true,
      outputReflected = true,
      finalXor = BigInt("1F", 16)
    )
    val crc16 = new CrcKind(
      polynomial = BigInt("8005", 16),
      polynomialWidth = 16,
      initValue = BigInt("FFFF", 16),
      inputReflected = true,
      outputReflected = true,
      finalXor = BigInt("FFFF", 16)
    )
    val crc5Check = new CrcKind(
      polynomial = BigInt("5", 16),
      polynomialWidth = 5,
      initValue = BigInt("1F", 16),
      inputReflected = true,
      outputReflected = false,
      finalXor = BigInt("00", 16)
    )
    val crc16Check = new CrcKind(
      polynomial = BigInt("8005", 16),
      polynomialWidth = 16,
      initValue = BigInt("FFFF", 16),
      inputReflected = true,
      outputReflected = false,
      finalXor = BigInt("0000", 16)
    )
  }
}

case class Crc(kind : CrcKind, dataWidth : Int) extends Component{
  val io = new Bundle {
    val flush = in Bool()
    val input = slave Flow(Bits(dataWidth bits))
    val result = out Bits(kind.polynomialWidth bits)
    val resultNext = out Bits(kind.polynomialWidth bits)
  }

  val state = Reg(Bits(kind.polynomialWidth bits)) init(kind.initValue)

  var acc = CombInit(state)
  for(i <- if(kind.inputReflected) 0 to dataWidth-1 else dataWidth-1 downto 0){
    acc \= ((acc |<< 1) ^ ((io.input.payload(i) ^ acc.msb) ? B(kind.polynomial, kind.polynomialWidth bits) | B(0, kind.polynomialWidth bits)))
  }

  when(io.input.fire){
    state := acc
  }
  when(io.flush){
    state := kind.initValue
  }

  val stateXor = state ^ kind.finalXor
  val accXor = acc ^ kind.finalXor
  io.result := (if(kind.outputReflected) stateXor.asBools.reverse.asBits() else stateXor)
  io.resultNext := (if(kind.outputReflected) accXor.asBools.reverse.asBits() else accXor)
}

case class MacRxChecker(dataWidth : Int) extends Component {
  val io = new Bundle {
    val input = slave(Stream(Fragment(PhyRx(dataWidth))))
    val output = master(Stream(Fragment(PhyRx(dataWidth))))
  }

  val crc = Crc(CrcKind.Crc32, dataWidth)
  crc.io.input.valid := io.input.valid
  crc.io.input.payload := io.input.data
  crc.io.flush := io.output.lastFire

  val crcHit = crc.io.resultNext === 0x2144DF1C

  io.output.valid := io.input.valid
  io.output.last := io.input.last
  io.output.data := io.input.data
  io.output.error := io.input.error | io.input.last && !crcHit
  io.input.ready := io.output.ready
}


case class MacRxAligner(dataWidth : Int) extends Component{
  val io = new Bundle{
    val enable = in Bool()
    val input = slave(Stream(Fragment(PhyRx(dataWidth))))
    val output = master(Stream(Fragment(PhyRx(dataWidth))))
  }
  val alignWidth = 16
  assert(dataWidth <= alignWidth)

  val stateCount = alignWidth/dataWidth
  val state = Reg(UInt(log2Up(stateCount + 1) bits)) init(0)

  io.output << io.input
  when(io.enable && state =/= stateCount){
    io.output.valid := True
    io.output.last := False
    io.output.error := False
    io.input.ready := False
    when(io.output.ready){
      state := state + 1
    }
  }

  when(io.input.lastFire){
    state := 0
  }
}

case class MacRxDropper(dataWidth : Int) extends Component {
  val io = new Bundle {
    val input = slave(Stream(Fragment(PhyRx(dataWidth))))
    val output = master(Stream(Fragment(PhyRx(dataWidth))))
  }

  io.output.valid := io.input.valid && io.output.first === io.input.first
  io.output.last := io.input.last
  io.output.data := io.input.data
  io.output.error := io.input.error
  io.input.ready := True

  val drop = RegInit(False) setWhen(io.output.isStall) clearWhen(io.output.fire && io.output.last)
  when(drop){
    io.output.valid := True
    io.output.last := True
    io.output.error := True
  }
}


case class MacRxBuffer(pushCd : ClockDomain,
                       popCd : ClockDomain,
                       pushWidth : Int,
                       popWidth : Int,
                       byteSize : Int) extends Component {
  assert(isPow2(byteSize))
  assert(isPow2(popWidth/pushWidth))
  assert(popWidth >= pushWidth)
  assert(popWidth >= 16)

  val io = new Bundle {
    val push = new Bundle{
      val stream = slave(Stream(Fragment(PhyRx(pushWidth))))
      val drop = out Bool()
      val commit = out Bool()
      val error = out Bool()
    }

    val pop = new Bundle{
      val stream = master(Stream(Bits(popWidth bits)))
      val stats = new Bundle {
        val clear = in Bool()
        val drops, errors = out UInt (8 bits)
      }
    }
  }

  val ram = Mem(Bits(popWidth bits), byteSize*8/popWidth)
  val ptrWidth = ram.addressWidth + 1

  val popToPush = new StreamCCByToggle(UInt(ptrWidth bits), popCd, pushCd, withOutputBuffer = false)
  val pushToPop = new StreamCCByToggle(UInt(ptrWidth bits), pushCd, popCd, withOutputBuffer = false)

  popToPush.io.input.valid := True
  popToPush rework { popToPush.pushArea.data init(0) }

  pushToPop.io.input.valid := True
  pushToPop rework { pushToPop.pushArea.data init(0) }

  def isFull(a: UInt, b: UInt) = a.msb =/= b.msb && a(ptrWidth - 2 downto 0) === b(ptrWidth - 2 downto 0)
  def isEmpty(a: UInt, b: UInt) = a === b

  val lengthWidth = log2Up(byteSize*8)

  val push = pushCd on new Area{
    val currentPtr, oldPtr = Reg(UInt(ptrWidth bits)) init(0)
    val currentPtrPlusOne = currentPtr + 1
    val popPtr  = popToPush.io.output.toFlow.toReg(0).addTag(crossClockDomain)
    pushToPop.io.input.payload := oldPtr

    val ratio = popWidth/pushWidth
    val buffer = Reg(Bits(popWidth-pushWidth bits))
    val state = Reg(UInt(log2Up(ratio) bits)) init(0)
    val length = Reg(UInt(lengthWidth bits)) init(0)


    val port = ram.writePort
    port.valid := False
    port.payload.assignDontCare()

    val error = RegInit(False)
    val drop = RegInit(False)
    when(io.push.stream.fire && io.push.stream.error){
      error := True
    }

    val doWrite = False
    when(io.push.stream.fire){
      state := state + 1
      length := length + pushWidth
      for(i <- 0 to ratio-2) when(state === i){
        buffer(i*pushWidth, pushWidth bits) := io.push.stream.data
      }
      when(state === ratio-1){
        doWrite := True
      }
    }


    val full = isFull(currentPtrPlusOne, popPtr)
    when(doWrite){
      when(full){
        drop := True
      } otherwise {
        port.valid := True
        port.address := currentPtrPlusOne.resized
        port.data := io.push.stream.data ## buffer
        currentPtr := currentPtrPlusOne
      }
    }

    val cleanup =  RegNext(io.push.stream.lastFire) init(False)
    val commit = RegNext(cleanup) init(False)
    io.push.stream.ready := !cleanup && !commit

    when(cleanup && state =/= 0){
      doWrite := True
    }

    when(commit){
      when(error || drop || full) {
        currentPtr := oldPtr
      } otherwise {
        oldPtr := currentPtrPlusOne
        currentPtr := currentPtrPlusOne
        port.valid := True
        port.address := oldPtr.resized
        port.data := B(length).resized
      }
      error := False
      drop := False
      state := 0
      length := 0
    }

    io.push.drop := drop || commit && full
    io.push.commit := commit
    io.push.error := error
  }

  val pop = popCd on new Area{
    val currentPtr = Reg(UInt(ptrWidth bits)) init(0)
    val pushPtr = pushToPop.io.output.toFlow.toReg(0).addTag(crossClockDomain)
    popToPush.io.input.payload := currentPtr

    val cmd = Stream(ram.addressType())
    cmd.valid := !isEmpty(currentPtr, pushPtr)
    cmd.payload := currentPtr.resized

    io.pop.stream << ram.streamReadSync(cmd, crossClock = true)

    when(cmd.fire){
      currentPtr := currentPtr + 1
    }

    val stats = new Area{
      val drop = PulseCCByToggle(
        input = push.commit && push.drop,
        clockIn = pushCd,
        clockOut = popCd
      )
      val error = PulseCCByToggle(
        input = push.commit && push.error,
        clockIn = pushCd,
        clockOut = popCd
      )

      val drops, errors = Reg(UInt(8 bits)) init(0)
      drops := drops + U(drop && drops =/= drops.maxValue)
      errors := errors + U(error && errors =/= errors.maxValue)
      when(io.pop.stats.clear){
        drops := 0
        errors := 0
      }

      io.pop.stats.drops := drops
      io.pop.stats.errors := errors
    }
  }
}


object MacRxCheckSumChecker extends App{
  SpinalConfig(privateNamespace = true).generateVerilog(new MacRxCheckSumChecker())
}

case class MacRxCheckSumChecker() extends Component {
  val io = new Bundle {
    val input = slave(Stream(Fragment(PhyRx(8))))
    val output = master(Stream(Fragment(PhyRx(8))))
  }

  val frontend = new StateMachine{
    val ETH, IPV4, TRANSPORT, COMPLETION, UNKNOWN = new State()
    setEntry(ETH)

    val lastFired = RegInit(False) setWhen(io.input.fire && io.input.last)
    val history = History(io.input.data, 0 to 1, when = io.input.fire)
    val counter = Reg(UInt(16 bits))
    when(io.input.fire){
      counter := counter + 1
    }

    io.input.ready := io.output.ready
    io.output.valid := io.input.fire
    io.output.data := io.input.data
    io.output.error := io.input.error
    io.output.last := False

    case class CheckSum() extends Area {
      val clear = False
      val accumulator = Reg(UInt(16 bits))
      val push = False
      val inputLsb = CombInit(counter.lsb)
      val inputData = CombInit(io.input.data)
      val input = inputLsb.mux(B"x00" ## inputData, inputData ## B"x00").asUInt
      val s0 = accumulator +^ input
      val s1 = s0(15 downto 0) + s0.msb.asUInt
      when(push){
        accumulator := s1
      }
      when(clear){
        accumulator := 0
      }
      val result = ~accumulator.asBits
    }

    val csIp, csTransport = new CheckSum()

    ETH.onEntry{
      counter := 0
      lastFired := False
    }
    ETH.whenIsActive{
      csIp.clear := True
      csTransport.clear := True
      when(io.input.fire){
        when(counter === 13){
          goto(UNKNOWN)
          when(history(0) === 0 && history(1) === 0x08){
            counter := 0
            goto(IPV4)
          }
        }
        when(io.input.last){
          goto(UNKNOWN)
        }
      }
    }

    val IHL = Reg(UInt(4 bits))
    val protocol = Reg(Bits(8 bits))
    val totalLength = Reg(UInt(16 bits))
    val inIp4 = counter < totalLength
    val udpZeroChecksum = Reg(Bool())
    IPV4.whenIsActive{
      when(io.input.fire) {
        csTransport.push := counter >= 12 && counter <= 19
        csIp.push := True
        when(counter === 9){
          protocol := io.input.data
          csTransport.push := True
        }
        when(counter === 3){
          csTransport.push := True
          csTransport.input := (history(1) ## history(0)).asUInt - (IHL << 2)
          totalLength := (history(1) ## history(0)).asUInt
        }
        when(counter === 0) {
          IHL := io.input.data(3 downto 0).asUInt
          when(io.input.data(7 downto 4) =/= 4 || io.input.data(3 downto 0).asUInt < 5) {
            goto(UNKNOWN)
          }
        } elsewhen (counter === (IHL << 2) - 1) {
          goto(UNKNOWN)
          udpZeroChecksum := True
          switch(protocol){
            is(0x01) { goto(TRANSPORT); csTransport.clear := True } //ICMP
            is(0x06) { goto(TRANSPORT) } //TCP
            is(0x11) { goto(TRANSPORT) } //UDP
          }
        }
        when(io.input.last){
          goto(UNKNOWN)
        }
      }
    }

    TRANSPORT.whenIsActive{
      when(io.input.fire) {
        udpZeroChecksum clearWhen(io.input.data =/= 0 && counter >> 1 === ((IHL << 2) +^ 6) >> 1)
        csTransport.push.setWhen(inIp4)
        when(io.input.last){
          goto(COMPLETION)
        }
      }
    }

    UNKNOWN.whenIsActive{
      when(lastFired){
        io.input.ready := False
        io.output.valid := True
        io.output.data := 0
        io.output.last := True
        io.output.error := False
        when(io.output.ready) {
          goto(ETH)
        }
      }
    }

    val checksumOk = csIp.result === 0 && (csTransport.result === 0 || protocol === 0x11 && udpZeroChecksum)
    COMPLETION.whenIsActive{
      when(lastFired){
        io.input.ready := False
        io.output.valid := True
        io.output.data := 0
        io.output.data(0) := checksumOk
        io.output.last := True
        io.output.error := False
        when(io.output.ready) {
          goto(ETH)
        }
      }
    }
  }
}

package spinal.lib.com.eth

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{State, StateMachine}



case class MacTxManagedStreamFifoCc[T <: Data](payloadType : HardType[T],
                                          depth : Int,
                                          pushCd : ClockDomain,
                                          popCd : ClockDomain) extends Component {
  assert(isPow2(depth))


  val ram = Mem(payloadType, depth)
  val ptrWidth = ram.addressWidth + 1

  val io = new Bundle {
    val push = new Bundle {
      val stream = slave(Stream(payloadType))
      val commit = in Bool()
      val availability = out UInt (ptrWidth bits)
    }

    val pop = new Bundle {
      val stream = master(Stream(payloadType))
      val redo = in Bool()
      val commit = in Bool()
    }
  }

  val popToPush = new StreamCCByToggle(UInt(ptrWidth bits), popCd, pushCd, withOutputBuffer = false)
  val pushToPop = new StreamCCByToggle(UInt(ptrWidth bits), pushCd, popCd, withOutputBuffer = false)

  popToPush.io.input.valid := True
  popToPush rework { popToPush.pushArea.data init(0) }

  pushToPop.io.input.valid := True
  pushToPop rework { pushToPop.pushArea.data init(0) }

  def isFull(a: UInt, b: UInt) = a.msb =/= b.msb && a(ptrWidth - 2 downto 0) === b(ptrWidth - 2 downto 0)
  def isEmpty(a: UInt, b: UInt) = a === b

  val push = pushCd on new Area {
    val currentPtr, oldPtr = Reg(UInt(ptrWidth bits)) init(0)
    val popPtr = popToPush.io.output.toFlow.toReg(0).addTag(crossClockDomain)
    pushToPop.io.input.payload := oldPtr

    io.push.stream.ready := !isFull(currentPtr, popPtr)
    when(io.push.stream.fire) {
      ram(currentPtr.resized) := io.push.stream.payload
      currentPtr := currentPtr + 1
    }

    io.push.availability := depth - (currentPtr - popPtr)

    when(io.push.commit) {
      oldPtr := currentPtr
    }
  }

  val pop = popCd on new Area {
    val currentPtr, oldPtr = Reg(UInt(ptrWidth bits)) init(0)
    val pushPtr = pushToPop.io.output.toFlow.toReg(0).addTag(crossClockDomain)
    popToPush.io.input.payload := oldPtr

    val cmd = Stream(ram.addressType())
    cmd.valid := !isEmpty(currentPtr, pushPtr) && !io.pop.redo
    cmd.payload := currentPtr.resized

    val commitPtr = RegNextWhen(currentPtr, io.pop.stream.fire)

    io.pop.stream << ram.streamReadSync(cmd, crossClock = true).throwWhen(io.pop.redo)

    when(cmd.fire){
      currentPtr := currentPtr + 1
    }
    when(io.pop.redo){
      currentPtr := oldPtr
    }
    when(io.pop.commit){
      oldPtr := commitPtr
    }
  }
}

case class MacTxBuffer(pushCd : ClockDomain,
                       popCd : ClockDomain,
                       pushWidth : Int,
                       popWidth : Int,
                       byteSize : Int) extends Component {
  assert(isPow2(byteSize))
  assert(isPow2(pushWidth / popWidth))
  assert(popWidth <= pushWidth)

  val ptrWidth = log2Up(byteSize * 8 / pushWidth) + 1

  val io = new Bundle {
    val push = new Bundle {
      val stream = slave(Stream(Bits(pushWidth bits)))
      val availability = out UInt (ptrWidth bits)
    }

    val pop = new Bundle {
      val stream = master(Stream(Fragment(PhyTx(popWidth))))
      val redo = in Bool()
      val commit = in Bool()
    }
  }


  val fifo = MacTxManagedStreamFifoCc(
    payloadType = Bits(pushWidth bits),
    depth = byteSize * 8 / pushWidth,
    pushCd = pushCd,
    popCd = popCd
  )

  val wordCountWidth = log2Up(fifo.depth)
  val lengthWidth = log2Up(fifo.depth*pushWidth)

  io.push.availability := fifo.io.push.availability

  val push = pushCd on new Area {
    fifo.io.push.stream << io.push.stream

    val commit = RegNext(False)
    fifo.io.push.commit := commit


    val State = new SpinalEnum{
      val LENGTH, DATA = newElement()
    }
    val state = RegInit(State.LENGTH)
    val length = Reg(UInt(lengthWidth bits))
    val wordCountMinusOne = length-1 >> log2Up(pushWidth)
    val wordCounter = Reg(UInt(wordCountWidth bits))
    switch(state){
      is(State.LENGTH){
        wordCounter := 0
        when(fifo.io.push.stream.fire){
          state := State.DATA
          length := io.push.stream.payload.asUInt.resized
        }
      }
      is(State.DATA){
        when(io.push.stream.fire){
          wordCounter := wordCounter + 1
          when(wordCounter === wordCountMinusOne){
            commit := True
            state := State.LENGTH
          }
        }
      }
    }
  }

  val pop = popCd on new Area {
    val State = new SpinalEnum{
      val LENGTH, DATA, WAIT = newElement()
    }
    val state = RegInit(State.LENGTH())
    val length = Reg(UInt(lengthWidth bits))
    val lengthMinusOne = length-1
    val wordCounter = Reg(UInt(wordCountWidth bits))
    val wordCountEndAt = lengthMinusOne >> log2Up(pushWidth)
    val spliterEndAt = lengthMinusOne(log2Up(pushWidth)-1 downto log2Up(popWidth))
    val ratio = pushWidth/popWidth
    val spliter = Reg(UInt(log2Up(ratio) bits))


    fifo.io.pop.commit := io.pop.commit
    fifo.io.pop.redo := io.pop.redo
    fifo.io.pop.stream.ready := False

    io.pop.stream.valid := False
    io.pop.stream.last := False
    io.pop.stream.data := fifo.io.pop.stream.payload.subdivideIn(popWidth bits).read(spliter)

    switch(state){
      is(State.LENGTH){
        wordCounter := 0
        spliter := 0
        when(fifo.io.pop.stream.valid){
          state := State.DATA
          length := fifo.io.pop.stream.payload.asUInt.resized
          fifo.io.pop.stream.ready := True
        }
      }
      is(State.DATA) {
        io.pop.stream.valid := True
        when(io.pop.stream.ready) {
          spliter := spliter + 1
          when(wordCounter === wordCountEndAt && spliter === spliterEndAt) {
            state := State.WAIT
            fifo.io.pop.stream.ready := True
            io.pop.stream.last := True
          }
          when(spliter === spliter.maxValue) {
            wordCounter := wordCounter + 1
            fifo.io.pop.stream.ready := True
          }
        }
      }
      is(State.WAIT){
        when(io.pop.commit){
          state := State.LENGTH
        }
      }
    }

    when(io.pop.redo){
      state := State.LENGTH
    }
  }
}

case class MacTxCrc(dataWidth : Int) extends Component{
  val io = new Bundle{
    val input = slave(Stream(Fragment(PhyTx(dataWidth))))
    val output = master(Stream(Fragment(PhyTx(dataWidth))))
  }

  val emitCrc = RegInit(False) setWhen(io.input.lastFire) clearWhen(io.output.lastFire)
  val counter = Reg(UInt(log2Up(32/dataWidth) bits)) init(0)
  val crc = Crc(CrcKind.Crc32, dataWidth)
  crc.io.input << io.input.toFlowFire.translateWith(io.input.data)
  crc.io.flush := io.output.lastFire



  io.output.last := False
  when(!emitCrc) {
    io.output.valid := io.input.valid
    io.output.fragment := io.input.fragment
    io.input.ready := io.output.ready
  } otherwise {
    io.input.ready := False
    io.output.valid := True
    io.output.data := crc.io.result.subdivideIn(dataWidth bits).read(counter)
    when(counter === counter.maxValue) {
      io.output.last := True
      when(io.output.ready) {
        emitCrc := False
      }
    }
    when(io.output.ready) {
      counter := counter + 1
    }
  }
}

case class MacTxAligner(dataWidth : Int) extends Component{
  val io = new Bundle{
    val enable = in Bool()
    val input = slave(Stream(Fragment(PhyTx(dataWidth))))
    val output = master(Stream(Fragment(PhyTx(dataWidth))))
  }

  val alignWidth = 16
  assert(dataWidth <= alignWidth)

  val stateCount = alignWidth/dataWidth
  val state = Reg(UInt(log2Up(stateCount + 1) bits)) init(0)

  io.output << io.input
  when(io.enable && state =/= stateCount){
    io.output.valid := False
    io.input.ready := True
    when(io.input.valid){
      state := state + 1
    }
  }

  when(io.input.lastFire){
    state := 0
  }
}


case class MacTxHeader(dataWidth : Int, withError : Boolean = false) extends Component{
  val io = new Bundle{
    val input = slave(Stream(Fragment(PhyTx(dataWidth, withError))))
    val output = master(Stream(Fragment(PhyTx(dataWidth, withError))))
  }

//  val header = B"x555555555555555D"
  val headerWords = 64/dataWidth
  val state = Reg(UInt(log2Up(headerWords + 1) bits)) init(0)
  io.output.valid := io.input.valid
  io.output.last := False
  if(withError) io.output.error := io.input.error
  io.input.ready := False
  when(state === headerWords){
    io.input.ready := io.output.ready
    io.output.payload := io.input.payload
  } otherwise {
//    io.output.data := header.subdivideIn(dataWidth bits).reverse.read(state.resized)
    io.output.data := 0
    for(i <- 0 until dataWidth by 2) io.output.data(i) := True
    when(state === headerWords-1) {io.output.data.msb := True}
    when(io.output.fire) {
      state := state + 1
    }
  }
  when(io.input.lastFire){
    state := 0
  }
}

case class MacTxPadder(dataWidth : Int, carrierExtension : Boolean = false) extends Component{
  val ce = carrierExtension
  val io = new Bundle{
    val input = slave(Stream(Fragment(PhyTx(dataWidth))))
    val output = master(Stream(Fragment(PhyTx(dataWidth, carrierExtension))))
  }

  val byteCount = ce.mux(512, 64-4)
  val cycles = (byteCount*8 + dataWidth-1)/dataWidth
  val counter = Reg(UInt(log2Up(cycles) bits)) init(0)
  val ok = counter === cycles-1
  val fill = counter =/= 0 && io.input.first

  when(!ok && io.output.fire){
    counter := counter + 1
  }
  when(io.output.lastFire){
    counter := 0
  }
  val halted = io.input.haltWhen(fill)
  io.output.arbitrationFrom(halted)
  io.output.data := io.input.data
  io.output.last := io.input.last
  if(ce) io.output.error := False
  when(!ok){
    io.output.last := False
  }
  when(fill){
    io.output.valid := True
    if(!ce) {
      io.output.data := 0
    } else {
      io.output.data := 0x0F
      io.output.error := True
    }
    io.output.last := ok
  }
}

case class MacTxInterFrame(dataWidth : Int, withError : Boolean = false) extends Component{
  val io = new Bundle{
    val input = slave(Stream(Fragment(PhyTx(dataWidth, withError))))
    val output = master(Flow(Fragment(PhyTx(dataWidth, withError))))
  }

  val byteCount = 12
  val cycles = (byteCount*8 + dataWidth-1)/dataWidth
  val counter = Counter(cycles + 1)
  when(counter =/= 0 || io.input.lastFire){
    counter.increment()
  }
  io.output << io.input.haltWhen(counter =/= 0).toFlow
}



case class MacTxLso(bufferBytes : Int, packetsMax : Int = 15) extends Component{

  val io = new Bundle{
    val input = slave(Stream(Fragment(PhyTx(8))))
    val output = master(Stream(Fragment(PhyTx(8))))
  }

  val buffer = new Area {
    assert(isPow2(bufferBytes))
    val ram = Mem.fill(bufferBytes)(Fragment(PhyTx(8)))
    val pushAt, popAt = Reg(UInt(log2Up(bufferBytes) + 1 bits)) init (0)
    val write = ram.writePort()
    val readCmd = Stream(ram.addressType)
    val readRsp = ram.streamReadSync(readCmd)
    val packets = CounterUpDown(packetsMax + 1)
    val full = (pushAt ^ popAt ^ bufferBytes) === 0 || packets === packetsMax
    val empty = pushAt === popAt
  }

  val backend = new Area{
    buffer.readCmd.valid := !buffer.empty
    buffer.readCmd.payload := buffer.popAt.resized
    when(buffer.readCmd.fire){
      buffer.popAt := buffer.popAt + 1
    }
    when(buffer.readRsp.fire && buffer.readRsp.last){
      buffer.packets.decrement()
    }
    io.output <-< buffer.readRsp.haltWhen(buffer.packets.value === 0)
  }

  val frontend = new StateMachine{
    val ETH, DONE, IPV4, TCP, UDP, CRC_WRITE_0, CRC_WRITE_1 = new State()
    setEntry(ETH)

    val lastFired = RegInit(False) setWhen(io.input.fire && io.input.last)
    val history = History(io.input.data, 0 to 1, when = io.input.fire)
    val counter = Reg(UInt(16 bits))
    when(io.input.fire){
      counter := counter + 1
    }

    io.input.ready := !buffer.full && !lastFired
    buffer.write.valid := io.input.fire
    buffer.write.address := buffer.pushAt.resized
    buffer.write.data := io.input.payload
    when(io.input.fire){
      buffer.pushAt := buffer.pushAt + 1
    }

    val checksum = new Area{
      val clear = False
      val accumulator = Reg(UInt(16 bits))
//      val counter = Reg(UInt(1 bits))
      val push = False
      val input = counter.lsb.mux(B"x00" ## io.input.data, io.input.data ## B"x00").asUInt
      val s0 = accumulator +^ input
      val s1 = s0(15 downto 0) + s0.msb.asUInt

      when(push){
        accumulator := s1
      }
      when(clear){
        accumulator := 0
      }

      val result = ~accumulator
    }

    ETH.onEntry{
      lastFired := False
      counter := 0
    }
    ETH.whenIsActive{
      checksum.clear := True
      when(io.input.fire){
        when(counter === 13){
          goto(DONE)
          when(history(0) === 0 && history(1) === 0x08){
            counter := 0
            goto(IPV4)
          }
        }
        when(io.input.last){
          goto(DONE)
        }
      }
    }

    DONE.whenIsActive{
      when(lastFired){
        buffer.packets.increment()
        goto(ETH)
      }
    }



    val IHL = Reg(UInt(4 bits))
    val protocol = Reg(Bits(8 bits))
    val crcAt = Reg(buffer.ram.addressType)
    IPV4.whenIsActive{
      when(io.input.fire) {
        checksum.push := counter >= 12 && counter <= 19
        when(counter === 9){
          protocol := io.input.data
          checksum.push := True
        }
        when(counter === 3){
          checksum.push := True
          checksum.input := (history(1) ## history(0)).asUInt - (IHL << 2)
        }
        when(counter === 0) {
          IHL := io.input.data(3 downto 0).asUInt
          when(io.input.data(7 downto 4) =/= 4 || io.input.data(3 downto 0).asUInt < 5) {
            goto(DONE)
          }
        } elsewhen (counter === (IHL << 2) - 1) {
          goto(DONE)
          counter := 0
          switch(protocol){
            is(0x06) { goto(TCP) }
            is(0x11) { goto(UDP) }
          }
        }
        when(io.input.last){
          goto(DONE)
        }
      }
    }

    TCP.whenIsActive{
      when(io.input.fire) {
        checksum.push := counter =/= 16 && counter =/= 17
        when(counter === 16){
          crcAt := buffer.pushAt.resized
        }
        when(io.input.last){
          goto(CRC_WRITE_0)
          when(counter < 18 ){
            goto(DONE)
          }
        }
      }
    }

    UDP.whenIsActive{
      when(io.input.fire) {
        checksum.push := counter =/= 6 && counter =/= 7
        when(counter === 6){
          crcAt := buffer.pushAt.resized
        }
        when(io.input.last){
          goto(CRC_WRITE_0)
          when(counter < 8 ){
            goto(DONE)
          }
        }
      }
    }


    CRC_WRITE_0 whenIsActive{
      buffer.write.valid := True
      buffer.write.address := crcAt
      buffer.write.data.fragment.data := checksum.result(15 downto 8).asBits
      buffer.write.data.last := False
      goto(CRC_WRITE_1)
    }
    CRC_WRITE_1 whenIsActive{
      buffer.write.valid := True
      buffer.write.address := crcAt+1
      buffer.write.data.fragment.data := checksum.result(7 downto 0).asBits
      buffer.write.data.last := crcAt+2 === buffer.pushAt
      goto(DONE)
    }
  }
}
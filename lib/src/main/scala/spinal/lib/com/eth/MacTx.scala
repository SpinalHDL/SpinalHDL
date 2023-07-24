package spinal.lib.com.eth

import spinal.core._
import spinal.lib._



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


case class MacTxHeader(dataWidth : Int) extends Component{
  val io = new Bundle{
    val input = slave(Stream(Fragment(PhyTx(dataWidth))))
    val output = master(Stream(Fragment(PhyTx(dataWidth))))
  }

//  val header = B"x555555555555555D"
  val headerWords = 64/dataWidth
  val state = Reg(UInt(log2Up(headerWords + 1) bits)) init(0)
  io.output.valid := io.input.valid
  io.output.last := False
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

case class MacTxPadder(dataWidth : Int) extends Component{
  val io = new Bundle{
    val input = slave(Stream(Fragment(PhyTx(dataWidth))))
    val output = master(Stream(Fragment(PhyTx(dataWidth))))
  }

  val byteCount = 64-4
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
  io.output << io.input.haltWhen(fill)
  when(!ok){
    io.output.last := False
  }
  when(fill){
    io.output.valid := True
    io.output.data := 0
    io.output.last := ok
  }
}

case class MacTxInterFrame(dataWidth : Int) extends Component{
  val io = new Bundle{
    val input = slave(Stream(Fragment(PhyTx(dataWidth))))
    val output = master(Flow(Fragment(PhyTx(dataWidth))))
  }

  val byteCount = 12
  val cycles = (byteCount*8 + dataWidth-1)/dataWidth
  val counter = Counter(cycles + 1)
  when(counter =/= 0 || io.input.lastFire){
    counter.increment()
  }
  io.output << io.input.haltWhen(counter =/= 0).toFlow
}
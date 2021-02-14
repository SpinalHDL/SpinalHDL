package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

case class BmbL2CacheParameter(lineLength : Int,
                               waySize : Int,
                               wayCount : Int)

case class BmbL2Cache(p : BmbL2CacheParameter, ip : BmbParameter, op : BmbParameter) extends Component{

  // Constant definitions
  val lineCount = p.waySize / p.lineLength // Number of cache lines in a way
  val wordCount = p.waySize * 8 / op.access.dataWidth // Number of words in the data ram
  val loadBeatsCount = p.lineLength * 8 / op.access.dataWidth // Number of out_address words to transfer to load a line

  val indexBits = log2Up(lineCount) // Bits in the index
  val wordAlignBits = log2Up(op.access.dataWidth / 8) // Line access index
  val wordAddrBits = log2Up(wordCount) // Bits to address inside a line
  val wordLoadAddrBit = log2Up(loadBeatsCount)
  val indexBitsOffset = log2Up(p.lineLength)
  val wayBits = log2Up(p.wayCount)

  val tagIndexRange = (indexBits + indexBitsOffset - 1) downto indexBitsOffset // bits in the address to access the tag
  val tagRange = ip.access.addressWidth - 1 downto (wordAddrBits + wordAlignBits) // bits in the address to check the tag

  val lineIndexRange = (wordAddrBits + wordAlignBits - 1 downto wordAlignBits) // part of the address in the line indexing

  val io = new Bundle{
    val input = slave(Bmb(ip))
    val output = master(Bmb(op))
  }

  case class LineTag() extends Bundle{
    val valid = Bool
    val error = Bool
    val dirty = Bool
    val tagAddr = UInt(ip.access.addressWidth - wordAddrBits - wordAlignBits bits)
  }

  case class MissSlot() extends Bundle{
    val needsWriteback = Bool
    val writebackAddr = UInt(ip.access.addressWidth - (wordAddrBits + wordAlignBits) bits)
    val loadAddr = UInt(ip.access.addressWidth bits)
    val missId = UInt(8 bits)
    val way = UInt(wayBits bits)
  }

  case class BypassSlot() extends Bundle {
    val valid = Bool
    val write = Bool
  }

  case class DDRContext() extends Bundle {
    val write = Bool
    val index = UInt(indexBits bits)
    val way = UInt(wayBits bits)
  }

  case class LockItem() extends Bundle {
    val valid = Bool
    val index = UInt(indexBits bits)
    val way = UInt(wayBits bits)
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////
  // Tags Storage
  val tagMem = new Area() {
    val initCache = Bool()
    val rdEn = Bool
    val rdAddrIndex = UInt()

    val wrInit = Bool()
    val wrTagInit = LineTag()
    val wrAddressInit = UInt(indexBits bits)

    val wrTag = LineTag()
    val wrAddress = UInt(indexBits bits)
    val wr = Bits(p.wayCount bits)

    val tagAreas = for(i<- 0 until p.wayCount) yield new Area() {
      val tags = Mem(LineTag(), lineCount)
      val rdData = tags.readSync(rdAddrIndex, rdEn)

      tags.write(Mux(initCache, wrAddressInit, wrAddress),
        Mux(initCache, wrTagInit, wrTag),
        Mux(initCache, wrInit, wr(i)))
    }
  }

  // pLRU tag storage
  val pLRU = new Area() {
    val initCache = Bool()

    val rdAddr = UInt(indexBits bits)
    val rdData = Bits(p.wayCount bits)
    val rdEn = Bool

    val wrAddr = UInt(indexBits bits)
    val wrData = Bits(p.wayCount bits)
    val wrEn = Bool()

    val wrInit = Bool()
    val wrAddressInit = UInt(indexBits bits)

    // With 1 way always replace the same one :D
    if (p.wayCount == 1) {
      rdData := 0
    } else {
      val storage = Mem(Bits(p.wayCount bits), lineCount)
      rdData := storage.readSync(rdAddr, rdEn)
      storage.write(Mux(initCache, wrAddressInit, wrAddr),
        Mux(initCache, 0, wrData),
        Mux(initCache, wrInit, wrEn))
    }
  }

  // Data storage
  val lineMem = for(i <- 0 until p.wayCount) yield new Area() {
    val lines = Mem(Bits(op.access.dataWidth bits), wordCount)
    val hitAddr = UInt(wordAddrBits bits)
    val hitWrData = Bits(op.access.dataWidth bits)
    val hitEn = Bool()
    val hitWr = Bool()
    val hitWrMask = Bits(4 bits)
    val hitRdData = lines.readWriteSync(hitAddr, hitWrData, hitEn, hitWr, hitWrMask)

    val missWrData = Bits(op.access.dataWidth bits)
    val missEn = Bool()
    val missWr = Bool()
    val missAddr = UInt(wordAddrBits bits)
    val missRdData = lines.readWriteSync(missAddr, missWrData, missEn, missWr)
  }

  // Tag Broadcast Area
  val tagBroadcast = new Area() {
    val addr = UInt(ip.access.addressWidth bits)
    val valid = Bool
    val write = Bool
  }

  // Locking Table
  val lockTable = new Area() {
    val lockTableSize = 32
    val lockTableAddrBits = log2Up(lockTableSize)
    val lockTableRange = (lockTableAddrBits - 2 downto 0)
    val lockItems = Vec(Reg(LockItem()), lockTableSize)
    val clear = Bool

    val matchAddr = UInt(indexBits bits)
    val matchWay = UInt(wayBits bits)
    val matchOut = Bits(lockTableSize bits)

    when (clear) {
      for(i <- 0 until lockTableSize) {
        lockItems(i).valid := False
      }
    }

    for(i <- 0 until lockTableSize) {
      matchOut(i) := (matchAddr === lockItems(i).index) && lockItems(i).valid && matchWay === lockItems(i).way
    }

    val wrAddr = UInt(indexBits bits)
    val wrWay = UInt(wayBits bits)
    val wrEn = Bool
    val wrBurstFinished = Bool
    val wrCounter = Reg(UInt(lockTableAddrBits bits)) init 0
    val wrStall = Bool
    wrStall := lockItems(wrCounter + 1).valid

    when (wrEn) {
      lockItems(wrCounter).valid := True
      lockItems(wrCounter).index := wrAddr
      lockItems(wrCounter).way := wrWay
    }

    when (wrBurstFinished) {
      wrCounter := wrCounter + 1
    }

    val unlockAddr = UInt(lockTableAddrBits bits)
    val unlockEn = Bool
    when(unlockEn) {
      lockItems(unlockAddr).valid := False
    }

  }

  /////////////////////////////////////////////////////////////////////////////////////////////////

  val stallTagReading_s1 = Bool()
  val stallTagReading_s2 = Bool()

  // Tag Fetch
  val s0 = new Area() {
    val input = io.input.cmd.haltWhen(tagMem.initCache || stallTagReading_s1 || stallTagReading_s2 || lockTable.wrStall)
    tagMem.rdAddrIndex := input.payload.fragment.address(tagIndexRange)
    tagMem.rdEn := input.fire
    pLRU.rdAddr := input.payload.fragment.address(tagIndexRange)
    pLRU.rdEn := input.fire

    case class S0Output() extends Bundle {
      val cmd = Fragment(BmbCmd(ip))
      //val tagBypass = BypassSlot()
    }
    val S0out = S0Output()

    /*when (tagBroadcast.addr(ip.access.addressWidth-1 downto indexBitsOffset) === input.fragment.address(ip.access.addressWidth-1 downto indexBitsOffset)) {
      S0out.tagBypass.valid := tagBroadcast.valid
      S0out.tagBypass.write := tagBroadcast.write
    } otherwise {
      S0out.tagBypass.valid := False
      S0out.tagBypass.write := False
    }*/

    S0out.cmd := input.payload
    val output = input.translateWith(S0out)
  }

  // Tag Check
  val s1 = new Area() {
    val input = s0.output.m2sPipe()

    case class S1Output() extends Bundle {
      val cmd = Fragment(BmbCmd(ip))
      val hitId = Bits(p.wayCount bits)
      val valid = Bits(p.wayCount bits)
      val tagLines = Vec(LineTag(), p.wayCount)
      //val tagBypass = BypassSlot()
      val pLRU = Bits(p.wayCount bits)
    }

    stallTagReading_s1 := (io.input.cmd.payload.fragment.address(tagIndexRange) === input.cmd.address(tagIndexRange)) && input.valid

    val S1out = S1Output()

    S1out.cmd := input.payload.cmd
    S1out.pLRU := pLRU.rdData
    val compareAddr = input.cmd.address(tagRange)

    for (i <- 0 until p.wayCount) {
      S1out.hitId(i) := tagMem.tagAreas(i).rdData.valid && (tagMem.tagAreas(i).rdData.tagAddr === compareAddr)
      S1out.valid(i) := tagMem.tagAreas(i).rdData.valid
    }
    S1out.tagLines := Vec(tagMem.tagAreas.map(_.rdData))

    /*when (tagBroadcast.addr(ip.access.addressWidth-1 downto indexBitsOffset) === input.cmd.address(ip.access.addressWidth-1 downto indexBitsOffset)) {
      S1out.tagBypass.valid := tagBroadcast.valid
      S1out.tagBypass.write := tagBroadcast.write
    } otherwise {
      S1out.tagBypass := input.tagBypass
    }*/

    val output = input.translateWith(S1out)
  }

  // Dispatch
  val s2 = new Area() {
    val input = s1.output.m2sPipe()

    case class S2Output() extends Bundle {
      val cmd = Fragment(BmbCmd(ip))
      val wayId = Bits(p.wayCount bits)
      val hit = Bool
      val missId = UInt(8 bits)
      val lockId = UInt(lockTable.lockTableAddrBits bits)
    }
    val reqMissId = Reg(UInt(8 bits)) init 0x1

    stallTagReading_s2 := (io.input.cmd.payload.fragment.address(tagIndexRange) === input.cmd.address(tagIndexRange)) && input.valid

    val S2out = S2Output()
    S2out.cmd := input.payload.cmd

    val (pipeStream, missStream) = StreamFork2(input, true)

    val replaceWay = OHMasking.first(~input.pLRU)
    val replaceWayId = OHToUInt(replaceWay)
    val tagHit = input.hitId.orR
    S2out.wayId := Mux(tagHit, input.payload.hitId, replaceWay)

    val hitWayId = OHToUInt(input.hitId)

    tagMem.wr.clearAll()
    tagMem.wrAddress.assignDontCare()
    val tagSelect = Mux(tagHit, hitWayId, replaceWayId)
    tagMem.wrTag := input.payload.tagLines(tagSelect)

    // Stream for Misses
    val S2miss = MissSlot()
    val missTagLine = input.payload.tagLines(replaceWayId)
    S2miss.loadAddr := input.payload.cmd.address
    S2miss.needsWriteback := (missTagLine.dirty && missTagLine.valid)
    S2miss.writebackAddr := missTagLine.tagAddr
    S2miss.missId := reqMissId
    S2miss.way := replaceWayId

    val newMiss = RegNextWhen(missStream.cmd.last, missStream.fire, True)
    val missTransaction = missStream.translateWith(S2miss)
    val missTransactionFiltered = missTransaction.throwWhen(tagHit || !newMiss)
    val missTransactionLocked = missTransactionFiltered.haltWhen(lockTable.matchOut.orR)
    val missQueue = missTransactionLocked.queue(16)
    when (missTransactionFiltered.fire) {
      reqMissId := reqMissId + 1
    }

    // Lock refill when collision happens
    lockTable.matchAddr := S2miss.loadAddr(tagIndexRange)
    lockTable.matchWay := S2miss.way

    // Tag Updating
    tagBroadcast.valid := False
    tagBroadcast.write := pipeStream.cmd.isWrite
    tagBroadcast.addr := input.payload.cmd.address

    // Locktable update
    lockTable.wrEn := False
    lockTable.wrAddr := pipeStream.cmd.address(tagIndexRange)
    lockTable.wrWay := Mux(tagHit, hitWayId, replaceWayId)

    val nextpLRUHit = pipeStream.payload.hitId | input.pLRU
    val nextpLRUMiss = replaceWay | input.pLRU

    pLRU.wrEn := False
    pLRU.wrData.assignDontCare()
    pLRU.wrAddr := pipeStream.payload.cmd.address(tagIndexRange)

    val newTransaction = RegNextWhen(pipeStream.cmd.last, pipeStream.fire, True)
    lockTable.wrBurstFinished := pipeStream.cmd.last && pipeStream.fire

    tagMem.wrAddress := pipeStream.payload.cmd.address(tagIndexRange)
    when (pipeStream.fire && newTransaction) {
      lockTable.wrEn := True
      pLRU.wrEn := True
      when(pipeStream.hitId.orR) {

        // Copy the Hit vector when all pLRU bits are set, this only keeps the last hit, reset other bits
        when(nextpLRUHit.andR) {
          pLRU.wrData := pipeStream.payload.hitId
        } otherwise {
          pLRU.wrData := nextpLRUHit
        }

        // On hit mark line as dirty on write
        when (input.cmd.isWrite) {
          tagMem.wrTag.dirty := True
          tagMem.wr(hitWayId) := True
        }
      } otherwise {
        // Update tag for future get
        tagMem.wrTag.tagAddr := pipeStream.payload.cmd.address(tagRange)
        tagMem.wrTag.valid := True
        // Line is dirty at load when it's a write, it will be directly replaced with new value
        tagMem.wrTag.dirty := pipeStream.payload.cmd.isWrite
        tagMem.wr(replaceWayId) := True

        // Copy the Hit vector when all pLRU bits are set, this only keeps the last hit, reset other bits
        when(nextpLRUMiss.andR) {
          pLRU.wrData := replaceWay
        } otherwise {
          pLRU.wrData := nextpLRUMiss
        }
      }
    }

    S2out.lockId := lockTable.wrCounter

    // Stream for pending operations
    val rspContext = DDRContext()
    rspContext.assignFromBits(io.output.rsp.context)
    S2out.missId := reqMissId
    S2out.hit := tagHit

    val hitDataReady = Bool()
    val missDataReady = Bool()
    val fifoMissId = UInt(8 bits)
    val loadMissId = Reg(UInt(8 bits)) init 0x00

    // Advance in items between the loader and the output fifo
    val missDifference = (loadMissId - fifoMissId).asSInt
    val action = pipeStream.translateWith(S2out)
    val actionFifo = action.queue(32)
    missDataReady := (missDifference >= 0) && actionFifo.valid
    val output = actionFifo.continueWhen(missDataReady | hitDataReady)

    hitDataReady := actionFifo.hit && actionFifo.valid
    fifoMissId := actionFifo.payload.missId
  }

  // Data Fetch / Write
  val s3 = new Area() {
    val input = s2.output.m2sPipe()

    case class S3Output() extends Bundle {
      val cmd = Fragment(BmbCmd(ip))
      val wayId = Bits(p.wayCount bits)
      val last = Bool
      val lockId = UInt(lockTable.lockTableAddrBits bits)
    }

    val outputStream = Stream(S3Output())

    val burstLen = input.cmd.length |>> 2
    val burstCount = Reg(UInt(log2Up(p.lineLength*8/op.access.dataWidth) bits)) init 0
    val burstFinished = Bool

    outputStream.valid := input.valid && input.cmd.last
    burstFinished := burstCount === burstLen

    val burstActive = (outputStream.fire && input.cmd.isRead) || (input.fire && input.cmd.isWrite)
    when (burstActive) {
      burstCount := burstCount + 1
      when (burstFinished) {
        burstCount := 0
      }
    }

    val S3out = outputStream.payload
    S3out.cmd := input.payload.cmd
    S3out.wayId := input.payload.wayId
    S3out.lockId := input.payload.lockId
    S3out.last := burstFinished || (input.cmd.isWrite && input.cmd.last)

    val output = outputStream
    // Keep ready while expanding a read burst or when writing to consume inputs from FIFO
    input.ready := output.ready & (burstFinished || input.cmd.isWrite)

    for (i <- 0 until p.wayCount) {
      lineMem(i).hitWrMask := input.cmd.mask
      lineMem(i).hitAddr := input.cmd.address(lineIndexRange) + burstCount
      lineMem(i).hitWrData := input.cmd.data
      lineMem(i).hitWr := input.cmd.isWrite && input.wayId(i)
      lineMem(i).hitEn := burstActive
    }
  }

  // Data access
  val s4 = new Area() {
    val input = s3.output.m2sPipe()

    val S4Out = Fragment(BmbRsp(ip));
    S4Out.fragment.data := lineMem.map(_.hitRdData).read(OHToUInt(input.payload.wayId))
    S4Out.fragment.context := input.cmd.context
    S4Out.fragment.exclusive := False
    S4Out.fragment.opcode := 0
    S4Out.fragment.source := input.cmd.source
    S4Out.last := input.last
    val output = input.translateWith(S4Out)

    lockTable.unlockAddr := input.lockId
    lockTable.unlockEn := input.fire & input.last
  }

  io.input.rsp << s4.output

  /////////////////////////////////////////////////////////////////////////////////////////////////
  // Cache Refill FSM

  val cache_refill = new Area() {
    object CacheLoadState extends SpinalEnum {
        val IDLE, WRITE, LOAD = newElement()
    }
    import CacheLoadState._

    val xferCounter = Reg(UInt(wordLoadAddrBit bits)) init 0
    val loadState = Reg(CacheLoadState()) init IDLE
    val replaceBaseAddr = Reg(UInt(op.access.addressWidth bits))
    val reloadBaseAddr = Reg(UInt(op.access.addressWidth bits))
    val reloadWay = Reg(UInt(wayBits bits))

    val respAddrCounter = Reg(UInt(wordLoadAddrBit bits)) init 0

    val missStream = s2.missQueue
    val readDataValid = Reg(Bool) init False
    val cmdContext = DDRContext()

    missStream.ready := False

    for (i <- 0 until p.wayCount) {
      lineMem(i).missWr := False
      lineMem(i).missEn := False
    }

    io.output.cmd.fragment.context.assignDontCare()
    io.output.rsp.ready := loadState =/= WRITE
    io.output.cmd.opcode.assignDontCare()

    cmdContext.assignDontCare()

    switch (loadState) {
      io.output.cmd.last := False
      io.output.cmd.valid := False

      is (WRITE) {
        readDataValid := True
        cmdContext.write := True
        cmdContext.way := reloadWay

        when (io.output.cmd.fire) {
          // Prepare to get next word
          xferCounter := xferCounter + 1
          readDataValid := False
          when(xferCounter === xferCounter.maxValue) {
            loadState := LOAD
            io.output.cmd.last := True
            readDataValid := False
          }
        }
        for (i <- 0 until p.wayCount) {
          lineMem(i).missEn := True
        }
        io.output.cmd.valid := readDataValid
        io.output.cmd.opcode := Bmb.Cmd.Opcode.WRITE
      }

      // Load line, expect address aligned on beginning of the line in the slot
      is (LOAD) {
        // Only send the read/write command once
        io.output.cmd.fragment.opcode := Bmb.Cmd.Opcode.READ
        when (io.output.cmd.fire) {
          loadState := IDLE
          io.output.cmd.last := True
        }
        cmdContext.write := False
        cmdContext.index := reloadBaseAddr(tagIndexRange)
        cmdContext.way := reloadWay
        io.output.cmd.valid := True

      }

      // Job done, waiting for new commands
      is (IDLE) {
        missStream.ready := True
        readDataValid := False
        xferCounter := 0
        when (missStream.fire && !tagMem.initCache) {

          replaceBaseAddr := missStream.payload.loadAddr
          replaceBaseAddr(wordLoadAddrBit + wordAlignBits - 1 downto 0).clearAll()
          replaceBaseAddr(tagRange) := missStream.payload.writebackAddr

          reloadBaseAddr := missStream.payload.loadAddr
          reloadBaseAddr(wordLoadAddrBit + wordAlignBits - 1 downto 0).clearAll()
          reloadWay := missStream.payload.way

          when (missStream.payload.needsWriteback) {
            loadState := WRITE
          } otherwise {
            loadState := LOAD
          }
        }
      }
    }
    io.output.cmd.context := cmdContext.asBits

    // Response Path
    // TODO give priority to data coming from DDR
    val rspContext = DDRContext()
    rspContext.assignFromBits(io.output.rsp.fragment.context)
    val way = UIntToOh(rspContext.way, p.wayCount)

    for(i <- 0 until p.wayCount) {
      lineMem(i).missAddr := (loadState === WRITE) ? (replaceBaseAddr(tagIndexRange) @@ xferCounter) | (rspContext.index @@ respAddrCounter)
      lineMem(i).missWrData := io.output.rsp.fragment.data
      when (io.output.rsp.fire && !rspContext.write) {
        lineMem(i).missWr := way(i)
        lineMem(i).missEn := way(i)
      }
    }

    when (io.output.rsp.fire && !rspContext.write) {
      respAddrCounter := respAddrCounter + 1
    }

    when (io.output.rsp.fire && io.output.rsp.last && !rspContext.write) {
      s2.loadMissId := s2.loadMissId + 1
    }

    val lineRdData = Vec(lineMem.map(_.missRdData))

    io.output.cmd.fragment.source := 0
    io.output.cmd.fragment.mask.setAll()
    io.output.cmd.fragment.data := lineRdData(reloadWay)
    io.output.cmd.fragment.length := p.lineLength - 1
    io.output.cmd.fragment.address := (loadState === WRITE) ? replaceBaseAddr | reloadBaseAddr
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////
  // Cache Init FSM, cleans the tags before use
  val cache_init = new Area() {
    object CacheInitState extends SpinalEnum {
      val IDLE, INIT = newElement()
    }
    import CacheInitState._

    val initState = RegInit(INIT)
    val addrCounter = Reg(UInt(indexBits bits)) init 0

    tagMem.initCache := initState === INIT
    pLRU.initCache := initState === INIT
    pLRU.wrInit := initState === INIT
    pLRU.wrAddressInit := addrCounter

    tagMem.wrTagInit.tagAddr.assignDontCare()
    tagMem.wrTagInit.error := False
    tagMem.wrTagInit.dirty.assignDontCare()
    tagMem.wrTagInit.valid := False
    tagMem.wrInit := False
    tagMem.wrAddressInit.assignDontCare()

    switch (initState) {
      // Clear all tags on reset
      is (INIT) {
        lockTable.clear := True
        tagMem.wrInit := True

        addrCounter := addrCounter + 1
        tagMem.wrAddressInit := addrCounter;

        when (addrCounter === addrCounter.maxValue) {
          initState := IDLE
        }
      }

      // Job done
      is (IDLE) {
        lockTable.clear := False
      }
    }
  }

}

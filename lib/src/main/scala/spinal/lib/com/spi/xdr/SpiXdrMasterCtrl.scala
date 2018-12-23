package spinal.lib.com.spi.ddr


import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.com.spi.SpiKind
import spinal.lib.fsm.{State, StateMachine}
import spinal.lib.io.TriState

import scala.collection.mutable.ArrayBuffer

case class XdrOutput(rate : Int) extends Bundle with IMasterSlave{
  val write = Bits(rate bits)

  override def asMaster(): Unit = {
    out(write)
  }

  def toTriState(): TriState[Bool] ={
    assert(rate == 2)
    val io = TriState(Bool)
    val clk = ClockDomain.readClockWire
    val writeBuffer = RegNext(write)
    io.write := (clk ? writeBuffer(0))| writeBuffer(1)
    io
  }
}

case class XdrPin(rate : Int) extends Bundle with IMasterSlave{
  val writeEnable = Bool
  val read,write = Bits(rate bits)

  override def asMaster(): Unit = {
    out(write,writeEnable)
    in(read)
  }

  def toTriState(): TriState[Bool] ={
    assert(rate == 2)
    val io = TriState(Bool)
    val clk = ClockDomain.readClockWire
    io.writeEnable := writeEnable
    val writeBuffer = RegNext(write)
    io.write := (clk ? writeBuffer(0))| writeBuffer(1)
    def cd(edge : EdgeKind) = ClockDomain.current.copy(config = ClockDomain.current.config.copy(clockEdge = edge))
    read(0) := cd(RISING)(RegNext(io.read))
    read(1) := cd(FALLING)(RegNext(io.read))
    io
  }
}


case class SpiXdrParameter(dataWidth : Int,
                           ioRate : Int,
                           ssWidth : Int)

case class SpiXdrMaster(val p : SpiXdrParameter) extends Bundle with IMasterSlave{
  import p._

  val sclk = XdrOutput(p.ioRate)
  val data = Vec(XdrPin(p.ioRate), dataWidth)
  val ss   = if(ssWidth != 0) Bits(ssWidth bits) else null

  override def asMaster(): Unit = {
    master(sclk)
    if(ssWidth != 0) out(ss)
    data.foreach(master(_))
  }
}


object SpiXdrMasterCtrl {
  def apply(p : Parameters) = new TopLevel(p)



  def main(args: Array[String]): Unit = {
    SpinalVerilog(new TopLevel(Parameters(8,12,SpiXdrParameter(dataWidth = 4,ssWidth = 3, ioRate = 1)).addFullDuplex(0)))
  }




  case class WriteMapping(pin : Int, phase : Int, source : Int)
  case class ReadMapping(pin : Int, phase : Int, target : Int)
  case class Mod(id : Int, clkRate : Int, slowDdr : Boolean, dataWidth : Int, writeMapping : Seq[WriteMapping], readMapping : Seq[ReadMapping]){
    def bitrate = readMapping.length
  }
  case class Parameters(dataWidth : Int,
                        timerWidth : Int,
                        spi : SpiXdrParameter,
                        mods : ArrayBuffer[Mod] = ArrayBuffer()){

    val ModType = HardType(UInt(log2Up(mods.map(_.id).max + 1) bits))
    def ssGen = spi.ssWidth != 0
    def addFullDuplex(id : Int, rate : Int = 1, ddr : Boolean = false, dataWidth : Int = 8): this.type = addHalfDuplex(id,rate,ddr,1,dataWidth,1)
    def addHalfDuplex(id : Int, rate : Int, ddr : Boolean, spiWidth : Int, dataWidth : Int = 8, readPinOffset : Int = 0): this.type = {
      assert(isPow2(spi.ioRate))
      if(rate == 1) {
        val writeMapping = for (pinId <- (0 until spiWidth);
                                phaseId <- (0 until spi.ioRate)) yield WriteMapping(pinId, phaseId, pinId)

        val readMapping = for (pinId <- (0 until spiWidth)) yield ReadMapping(pinId + readPinOffset, 0, pinId)

        mods += Mod(id, rate, ddr, dataWidth, writeMapping, readMapping)
      } else {
        val pinRate = rate / (if(ddr) 1 else 2)
        val pinDuration =  spi.ioRate / pinRate
        val writeMapping = for (pinId <- (0 until spiWidth);
                                phaseId <- (0 until pinRate);
                                durationId <- (0 until pinDuration)) yield WriteMapping(pinId, phaseId*pinDuration + durationId, (pinRate - phaseId - 1)*spiWidth + pinId)

        val readMapping = for (pinId <- (0 until spiWidth);
                               phaseId <- (0 until pinRate)) yield ReadMapping(pinId + readPinOffset, (phaseId*pinDuration-1) & (spi.ioRate-1), (pinRate - phaseId - 1)*spiWidth + pinId)
        mods += Mod(id, rate, false, dataWidth, writeMapping, readMapping)
      }
      this
    }
  }

  case class Config(p: Parameters) extends Bundle {
    val kind = SpiKind()
    val sclkToogle = UInt(p.timerWidth bits)
    val mod = in(p.ModType())

    val ss = ifGen(p.ssGen) (new Bundle {
      val activeHigh = Bits(p.spi.ssWidth bits)
      val setup = UInt(p.timerWidth bits)
      val hold = UInt(p.timerWidth bits)
      val disable = UInt(p.timerWidth bits)
    })

  }

  case class Cmd(p: Parameters) extends Bundle{
    val kind = Bool
    val read, write = Bool
    val data = Bits(p.dataWidth bits)

    def isData = !kind
    def isSs = kind
    def getSsEnable = data.msb
    def getSsId = U(data(0, log2Up(p.spi.ssWidth) bits))
  }

  case class Rsp(p: Parameters) extends Bundle{
    val data = Bits(p.dataWidth bits)
  }

  case class MemoryMappingParameters(ctrl : Parameters,
                                     cmdFifoDepth : Int = 32,
                                     rspFifoDepth : Int = 32,
                                     cpolInit : Boolean = false,
                                     cphaInit : Boolean = false,
                                     modInit : Int = 0,
                                     sclkToogleInit : Int = 0,
                                     ssSetupInit : Int = 0,
                                     ssHoldInit : Int = 0,
                                     ssDisableInit : Int = 0,
                                     xipInstructionModInit: Int = 0,
                                     xipAddressModInit : Int = 0,
                                     xipDummyModInit : Int = 0,
                                     xipPayloadModInit : Int = 0,
                                     xipConfigWritable : Boolean = true,
                                     xipEnableInit : Boolean = false,
                                     xipInstructionEnableInit : Boolean = true,
                                     xipInstructionDataInit : Int = 0x0B,
                                     xipDummyCountInit : Int = 1,
                                     xipDummyDataInit : Int = 0xFF,
                                     xip : XipBusParameters = null)

  case class XipBusParameters(addressWidth : Int, dataWidth : Int)
  case class XipBus(p : XipBusParameters) extends Bundle with IMasterSlave{
    val cmd = Stream(UInt(p.addressWidth bits))
    val rsp = Flow(Bits(p.dataWidth bits))

    override def asMaster(): Unit = {
      master(cmd)
      slave(rsp)
    }

    def fromPipelinedMemoryBus() = {
      val accessBus = new PipelinedMemoryBus(PipelinedMemoryBusConfig(24,32))
      cmd.valid <> (accessBus.cmd.valid && !accessBus.cmd.write)
      cmd.ready <> accessBus.cmd.ready
      cmd.payload <> accessBus.cmd.address

      rsp.valid <> accessBus.rsp.valid
      rsp.payload <> accessBus.rsp.data
      accessBus
    }
  }

  class TopLevel(val p: Parameters) extends Component {
    setDefinitionName("SpiXdrMasterCtrl")

    val io = new Bundle {
      val config = in(Config(p))
      val cmd = slave(Stream(Cmd(p)))
      val rsp = master(Flow(Rsp(p)))
      val spi = master(SpiXdrMaster(p.spi))


      def driveFrom(bus : BusSlaveFactory, baseAddress : Int = 0)(mapping : MemoryMappingParameters) = new Area {
        import mapping._
        require(cmdFifoDepth >= 1)
        require(rspFifoDepth >= 1)

        require(cmdFifoDepth < 32.kB)
        require(rspFifoDepth < 32.kB)

        //CMD
        val cmdLogic = new Area {
          val streamUnbuffered = Stream(Cmd(p))
          streamUnbuffered.valid := bus.isWriting(address = baseAddress + 0)
          bus.nonStopWrite(streamUnbuffered.data, bitOffset = 0)
          bus.nonStopWrite(streamUnbuffered.write, bitOffset = 8)
          bus.nonStopWrite(streamUnbuffered.read, bitOffset = 9)
          bus.nonStopWrite(streamUnbuffered.kind, bitOffset = 11)


          bus.createAndDriveFlow(Cmd(p),address = baseAddress + 0).toStream
          val (stream, fifoAvailability) = streamUnbuffered.queueWithAvailability(cmdFifoDepth)
          cmd << stream
          bus.read(fifoAvailability, address = baseAddress + 4, 16)
        }

        //RSP
        val rspLogic = new Area {
          val feedRsp = True
          val (stream, fifoOccupancy) = rsp.takeWhen(feedRsp).queueWithOccupancy(rspFifoDepth)
          bus.readStreamNonBlocking(stream, address = baseAddress + 0, validBitOffset = 31, payloadBitOffset = 0)
          bus.read(fifoOccupancy, address = baseAddress + 0, 16)
        }

        //Status
        val interruptCtrl = new Area {
          val cmdIntEnable = bus.createReadAndWrite(Bool, address = baseAddress + 4, 0) init(False)
          val rspIntEnable  = bus.createReadAndWrite(Bool, address = baseAddress + 4, 1) init(False)
          val cmdInt = bus.read(cmdIntEnable & !cmdLogic.stream.valid, address = baseAddress + 4, 8)
          val rspInt = bus.read(rspIntEnable &  rspLogic.stream.valid, address = baseAddress + 4, 9)
          val interrupt = rspInt || cmdInt
        }

        //Configs
        bus.drive(config.kind, baseAddress + 8, bitOffset = 0)
        bus.drive(config.mod, baseAddress + 8, bitOffset = 4)
        bus.drive(config.sclkToogle, baseAddress + 0x20)
        bus.drive(config.ss.setup,   baseAddress + 0x24)
        bus.drive(config.ss.hold,    baseAddress + 0x28)
        bus.drive(config.ss.disable, baseAddress + 0x2C)

        if(xipEnableInit){
          config.kind.cpol init(cpolInit)
          config.kind.cpha init(cphaInit)
          config.mod init(modInit)
          config.sclkToogle init(sclkToogleInit)
          config.ss.setup init(ssSetupInit)
          config.ss.hold init(ssHoldInit)
          config.ss.disable init(ssDisableInit)
        }

        val xip = ifGen(mapping.xip != null) (new Area{
          val xipBus = XipBus(mapping.xip)
          val enable = Reg(Bool)
          val instructionMod = Reg(p.ModType)
          val instructionEnable = Reg(Bool)
          val instructionData = Reg(Bits(8 bits))
          val addressMod = Reg(p.ModType)
          val dummyCount = Reg(UInt(4 bits))
          val dummyData = Reg(Bits(8 bits))
          val dummyMod = Reg(p.ModType)
          val payloadMod = Reg(p.ModType)

          if(xipEnableInit){
            enable init(True)
            instructionMod init(xipInstructionModInit)
            addressMod init(xipAddressModInit)
            dummyMod init(xipDummyModInit)
            payloadMod init(xipPayloadModInit)
            instructionEnable init(Bool(xipInstructionEnableInit))
            instructionData init(xipInstructionDataInit)
            dummyCount init(xipDummyCountInit)
            dummyData init(xipDummyDataInit)
          } else {
            enable init(True)
          }

          bus.write(enable, baseAddress + 0x40)
          if(xipConfigWritable) {
            bus.write(instructionData, baseAddress + 0x44, bitOffset = 0)
            bus.write(instructionEnable, baseAddress + 0x44, bitOffset = 8)
            bus.write(dummyData, baseAddress + 0x44, bitOffset = 16)
            bus.write(dummyCount, baseAddress + 0x44, bitOffset = 24)

            bus.write(instructionMod, baseAddress + 0x48, bitOffset = 0)
            bus.write(addressMod, baseAddress + 0x48, bitOffset = 8)
            bus.write(dummyMod, baseAddress + 0x48, bitOffset = 16)
            bus.write(payloadMod, baseAddress + 0x48, bitOffset = 24)
          }

          val fsm = new StateMachine{
            val doLoad, doPayload, done = False
            val loadedValid = RegInit(False)
            val loadedAddress = Reg(UInt(24 bits))
            val hit = loadedValid && loadedAddress === (xipBus.cmd.payload(23 downto 2) @@ U"00")

            val IDLE, INSTRUCTION, ADDRESS, DUMMY, PAYLOAD = State()
            setEntry(IDLE)

            when(enable){
              cmd.valid := False
              rspLogic.feedRsp := False
            }

            IDLE.whenIsActive{
              when(doLoad){
                cmd.valid := True
                cmd.kind := True
                cmd.data := 1 << cmd.data.high
                when(cmd.ready) {
                  loadedAddress := xipBus.cmd.payload(23 downto 2) @@ U"00"
                  when(instructionEnable) {
                    goto(INSTRUCTION)
                  } otherwise {
                    goto(ADDRESS)
                  }
                }
              }
            }

            INSTRUCTION.whenIsActive{
              cmd.valid := True
              cmd.kind := False
              cmd.write := True
              cmd.read := False
              cmd.data := instructionData
              config.mod := instructionMod
              when(cmd.ready) {
                goto(ADDRESS)
              }
            }

            val counter = Reg(UInt(4 bits)) init(0)
            ADDRESS.onEntry(counter := 0)
            ADDRESS.whenIsActive{
              cmd.valid := True
              cmd.kind := False
              cmd.write := True
              cmd.read := False
              cmd.data := loadedAddress.subdivideIn(8 bits).reverse(counter(1 downto 0)).asBits
              config.mod := addressMod
              when(cmd.ready) {
                counter := counter + 1
                when(counter === 2) {
                  goto(DUMMY)
                }
              }
            }

            DUMMY.onEntry(counter := 0)
            DUMMY.whenIsActive{
              cmd.valid := True
              cmd.kind := False
              cmd.write := True
              cmd.read := False
              cmd.data := dummyData
              config.mod := dummyMod
              when(cmd.ready) {
                counter := counter + 1
                when(counter === dummyCount) {
                  loadedValid := True
                  goto(PAYLOAD)
                }
              }
            }

            PAYLOAD.onEntry(counter := 0)
            PAYLOAD.whenIsActive{
              config.mod := payloadMod
              when(doPayload) {
                cmd.valid := True
                cmd.kind := False
                cmd.write := False
                cmd.read := True
                when(cmd.ready) {
                  counter := counter + 1
                  when(counter === mapping.xip.dataWidth / 8 - 1) {
                    done := True
                    counter := 0
                    loadedAddress := loadedAddress + mapping.xip.dataWidth / 8
                  }
                }
              }

              when(doLoad){
                cmd.valid := True
                cmd.kind := True
                cmd.data := 0
                when(cmd.ready) {
                  loadedValid := False
                  goto(IDLE)
                }
              }
            }

            always{
              when(!enable){
                goto(IDLE)
              }
            }
          }


          xipBus.cmd.ready := False
          when(enable){
            when(xipBus.cmd.valid){
              when(fsm.hit){
                fsm.doPayload := True
                xipBus.cmd.ready := fsm.done
              } otherwise {
                fsm.doLoad := True
              }
            }
          }

          val rspCounter = Counter(mapping.xip.dataWidth/8)
          val rspBuffer = Reg(Bits(mapping.xip.dataWidth-8 bits))
          when(enable && rsp.valid){
            rspCounter.increment()
            rspBuffer := rsp.payload ## (rspBuffer >> 8)
          }

          xipBus.rsp.valid := rspCounter.willOverflow
          xipBus.rsp.payload := rsp.payload ## rspBuffer

          when(!enable){
            xipBus.cmd.ready := True
            xipBus.rsp.valid := RegNext(xipBus.cmd.valid) init(False)
          }

        })
      }
    }

    val timer = new Area{
      val counter = Reg(UInt(p.timerWidth bits))
      val reset = False
      val ss = ifGen(p.ssGen) (new Area{
        val setupHit    = counter === io.config.ss.setup
        val holdHit     = counter === io.config.ss.hold
        val disableHit  = counter === io.config.ss.disable
      })
      val sclkToogleHit = counter === io.config.sclkToogle

      counter := (counter + 1).resized
      when(reset){
        counter := 0
      }
    }



    val widths = p.mods.map(m => m.bitrate).distinct.sorted
    val widthMax = widths.max






    val fsm = new Area {
      val state = RegInit(False)
      val counter = Reg(UInt(log2Up(p.dataWidth) bits)) init(0)
      val bitrateMax = p.mods.map(_.bitrate).max
      val counterPlus = counter + io.config.mod.muxListDc(p.mods.map(m => m.id -> U(m.bitrate, log2Up(bitrateMax + 1) bits))).resized
      val fastRate = io.config.mod.muxListDc(p.mods.map(m => m.id -> Bool(m.clkRate != 1)))
      val isDdr = io.config.mod.muxListDc(p.mods.map(m => m.id -> Bool(m.slowDdr)))
      val readFill, readDone = False
      val ss = RegInit(B((1 << p.spi.ssWidth) - 1, p.spi.ssWidth bits))
      io.spi.ss := ss

      io.cmd.ready := False
      when(io.cmd.valid) {
        when(io.cmd.isData) {
          timer.reset := timer.sclkToogleHit
          when(timer.sclkToogleHit){
            state := !state
          }
          when((timer.sclkToogleHit && (state || isDdr)) || fastRate) {
            counter := counterPlus
            readFill := True
            when(counterPlus === 0){
              io.cmd.ready := True
              readDone := io.cmd.read
              state := False
            }
          }
        } otherwise {
          if (p.ssGen) {
            when(io.cmd.getSsEnable) {
              ss(io.cmd.getSsId) := False
              when(timer.ss.setupHit) {
                io.cmd.ready := True
              }
            } otherwise {
              when(!state) {
                when(timer.ss.holdHit) {
                  state := True
                  timer.reset := True
                }
              } otherwise {
                ss(io.cmd.getSsId) := True
                when(timer.ss.disableHit) {
                  io.cmd.ready := True
                }
              }
            }
          }
        }
      }

      //Idle states
      when(!io.cmd.valid || io.cmd.ready){
        state := False
        counter := 0
        timer.reset := True
      }
    }


    val maxBitRate = p.mods.map(m => m.bitrate).max
    val outputPhy = new Area {
      val rates = p.mods.map(m => m.id -> log2Up(m.clkRate))

      //Generate SCLK
      val sclkWrite = B(0, p.spi.ioRate bits)
      io.spi.sclk.write := sclkWrite ^ B(sclkWrite.range -> io.config.kind.cpol)
      when(io.cmd.valid && io.cmd.isData){
        switch(io.config.mod) {
          for (m <- p.mods) {
            is(m.id){
              m.clkRate match {
                case 1 => sclkWrite := (default -> (fsm.state ^ io.config.kind.cpha))
                case _ => for(bitId <- 0 until p.spi.ioRate){
                  sclkWrite(bitId) := (if((bitId * m.clkRate / p.spi.ioRate) % 2 == 0) io.config.kind.cpha else !io.config.kind.cpha)
                }
              }
            }
          }
        }
      }


      //Get raw data to put on MOSI
      val dataWrite = Bits(maxBitRate bits)
      val widthSel = io.config.mod.muxListDc( p.mods.map(m => m.id -> U(widths.indexOf(m.bitrate), log2Up(widthMax + 1) bits)))
      dataWrite.assignDontCare()
      switch(widthSel){
        for((width, widthId) <- widths.zipWithIndex){
          is(widthId){
            dataWrite(0, width bits) := io.cmd.data.subdivideIn(width bits).reverse(fsm.counter >> log2Up(width))
          }
        }
      }


      //Set MOSI signals
      io.spi.data.foreach(_.writeEnable := False)
      io.spi.data.foreach(_.write.assignDontCare())
      switch(io.config.mod){
        for(mod <- p.mods){
          is(mod.id) {
            when(io.cmd.valid && io.cmd.write){
              mod.writeMapping.map(_.pin).distinct.foreach(i => io.spi.data(i).writeEnable := True)
            }
            for (mapping <- mod.writeMapping) {
              io.spi.data(mapping.pin).write(mapping.phase) := dataWrite(mapping.source)
            }
          }
        }
      }
    }


    val inputPhy = new Area{
      def sync[T <: Data](that : T, init : T = null) = Delay(that,2,init=init)
      val mod = sync(io.config.mod)
      val readFill = sync(fsm.readFill, False)
      val readDone = sync(fsm.readDone, False)
      val buffer = Reg(Bits(p.dataWidth - p.mods.map(_.bitrate).min bits))
      val bufferNext = Bits(p.dataWidth bits).assignDontCare().allowOverride
      val widthSel = mod.muxListDc(p.mods.map(m => m.id -> U(widths.indexOf(m.bitrate), log2Up(widthMax + 1) bits)))
      val dataWrite, dataRead = Bits(maxBitRate bits)
      val dataReadBuffer = RegNextWhen(Cat(io.spi.data.map(_.read(0))), !sync(fsm.state))

      dataRead.assignDontCare()

      switch(mod){
        for(mod <- p.mods){
          is(mod.id) {
            for(mapping <- mod.readMapping) {
              if(mod.clkRate != 1 || mod.slowDdr) {
                dataRead(mapping.target) := io.spi.data(mapping.pin).read(mapping.phase)
              }else{
                assert(mapping.phase == 0)
                dataRead(mapping.target) := dataReadBuffer(mapping.pin)
              }
            }
          }
        }
      }


      switch(widthSel) {
        for ((width,widthId) <- widths.zipWithIndex) {
          is(widthId) {
            bufferNext := (buffer ## dataRead(0, width bits)).resized
            when(readFill) { buffer := bufferNext.resized }
          }
        }
      }

      io.rsp.valid := readDone
      io.rsp.data := bufferNext
    }
  }
}


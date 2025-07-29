package spinal.lib.cpu.riscv.debug

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class DebugModuleCpuConfig(xlen : Int,
                                flen : Int,
                                withFpuRegAccess : Boolean)

case class DebugModuleParameter(version : Int,
                                harts : Int,
                                progBufSize : Int,
                                datacount : Int,
                                hartsConfig : Seq[DebugModuleCpuConfig],
                                withSysBus: Boolean = false)


object DebugDmToHartOp extends SpinalEnum{
  val DATA, EXECUTE, REG_WRITE, REG_READ = newElement()
}

case class DebugDmToHart() extends Bundle{
  val op = DebugDmToHartOp()
  val address = UInt(5 bits)
  val data = Bits(32 bits)
  val size  = UInt(3 bits)
}

case class DebugHartToDm() extends Bundle{
  val address = UInt(4 bits)
  val data = Bits(32 bits)
}

case class DebugHartBus() extends Bundle with IMasterSlave {
  val halted, running, unavailable = Bool()
  val exception, commit, ebreak, redo, regSuccess = Bool()  //Can only be set when the CPU is in debug mode
  val ackReset, haveReset = Bool()
  val resume = FlowCmdRsp()
  val haltReq = Bool()

  val dmToHart = Flow(DebugDmToHart())
  val hartToDm = Flow(DebugHartToDm())

  override def asMaster() = {
    in(halted, running, unavailable, haveReset, exception, commit, ebreak, redo, regSuccess)
    master(resume)
    master(dmToHart)
    slave(hartToDm)
    out(haltReq, ackReset)
  }
}

object DebugModuleCmdErr extends SpinalEnum(binarySequential){
  val NONE, BUSY, NOT_SUPPORTED, EXCEPTION, HALT_RESUME, BUS, OTHER = newElement()
}

case class DebugSysBusCmd() extends Bundle{
  val wr = Bool
  val address = UInt(32 bits)
  val data = Bits(32 bit)
  val size = UInt(2 bit)
}

case class DebugSysBusRsp() extends Bundle{
  val error = Bool
  val data = Bits(32 bit)
}

case class DebugSysBus() extends Bundle with IMasterSlave{
  val cmd = Stream(DebugSysBusCmd())
  val rsp = Flow(DebugSysBusRsp())

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

object DebugModule{
  val CSR_DATA = 0x7B4
  def csrr(dataId : Int, regId : UInt) = B(0x00002073 | (CSR_DATA + dataId << 20), 32 bits) | (regId.asBits << 7).resized
  def csrw(dataId : Int, regId : UInt) = B(0x00001073 | (CSR_DATA + dataId << 20), 32 bits) | (regId.asBits << 15).resized
  def ebreak() = B(0x00100073, 32 bits)
}

case class DebugModule(p : DebugModuleParameter) extends Component{
  import DebugModule._
  val io = new Bundle {
    val ctrl = slave(DebugBus(7))
    val ndmreset = out Bool()
    val harts = Vec.fill(p.harts)(master(DebugHartBus()))
    val sysBus = p.withSysBus generate master(DebugSysBus())
  }

  val factory = new DebugBusSlaveFactory(io.ctrl)

  val dmactive = factory.createReadAndWrite(Bool(), 0x10, 0) init(False)
  val dmCd = ClockDomain(ClockDomain.current.clock, reset = dmactive, config = ClockDomain.current.config.copy(resetKind = ASYNC, resetActiveLevel = LOW))

  val logic = dmCd on new Area{
    val dmcontrol = new Area {
      val ndmreset = factory.createReadAndWrite(Bool(), 0x10, 1) init(False)
      val clrresethaltreq = null //TODO ?
      val setresethaltreq = null //TODO ?
      val hartreset = null

      val hartSelLoNew = factory.nonStopWrite(UInt(10 bits), 16)
      val hartSelHiNew = factory.nonStopWrite(UInt(10 bits), 6)
      val hartSelNew = hartSelHiNew @@ hartSelLoNew
      val hartSelLo = factory.createReadAndWrite(UInt(10 bits), 0x10, 16) init(0)
      val hartSelHi = factory.createReadAndWrite(UInt(10 bits), 0x10, 6) init(0)
      val hartSel = hartSelHi @@ hartSelLo
      val haltSet = factory.setOnSet(False, 0x10, 31)
      val haltClear = factory.setOnClear(False, 0x10, 31)
      val resumeReq = factory.setOnSet(False, 0x10, 30) clearWhen(haltSet)
      val ackhavereset = factory.setOnSet(False, 0x10, 28)

      val hartSelAarsizeLimit = p.hartsConfig.map(v => U(log2Up(v.xlen/8))).read(hartSel.resized)
      val hartSelAarsizeLimitF = p.hartsConfig.map(v => U(log2Up(if(v.withFpuRegAccess) v.flen/8 else 0))).read(hartSel.resized)

      val harts = for((bus, hartId) <- io.harts.zipWithIndex) yield new Area{
        val haltReq = RegInit(False)
        bus.haltReq := haltReq
        bus.resume.setIdle()
        when(hartSelNew === hartId){
          haltReq := (haltReq || haltSet) && !haltClear
          bus.resume.cmd.valid := resumeReq
        }
      }

      io.ndmreset := ndmreset
    }


    val toHarts = Flow(DebugDmToHart()).setIdle()
    val fromHarts = Flow(DebugHartToDm())
    fromHarts.valid := io.harts.map(_.hartToDm.valid).orR
    fromHarts.payload := MuxOH.or(io.harts.map(_.hartToDm.valid), io.harts.map(_.hartToDm.payload), bypassIfSingle = true)
    val harts = for(hartId <- 0 until p.harts) yield new Area{
      val sel = dmcontrol.hartSel === hartId
      val bus = io.harts(hartId)
      val resumeReady = !bus.resume.isPending(1) && RegInit(False).setWhen(bus.resume.cmd.valid)
      def halted = bus.halted
      def running = bus.running
      def unavailable = bus.unavailable
      def haveReset = bus.haveReset
      bus.dmToHart << toHarts.throwWhen(toHarts.op =/= DebugDmToHartOp.DATA && !sel)
      bus.ackReset := RegNext(sel && dmcontrol.ackhavereset)
    }

    val selected = new Area{
      val hart = Reg(UInt(log2Up(p.harts) bits))
      val running = io.harts.map(_.running).read(hart)
      val halted = io.harts.map(_.halted).read(hart)
      val commit = io.harts.map(_.commit).read(hart)
      val regSuccess = io.harts.map(_.regSuccess).read(hart)
      val exception = io.harts.map(_.exception).read(hart)
      val ebreak = io.harts.map(_.ebreak).read(hart)
      val redo = io.harts.map(_.redo).read(hart)
    }

    val haltsum = new Area{
      assert(p.harts <= 32)
      val value = U(0, 32 bits)
      for(g <- 0 until (p.harts+31)/32){
        when(dmcontrol.hartSel >> 5 === g){
          for(hid <- g*32 to (g*32+31 min p.harts-1)){
            value(hid%32) := harts(hid).halted
          }
        }
      }
      factory.read(value, 0x40)
    }

    val dmstatus = new Area{
      val version = factory.read(U(p.version, 4 bits), 0x11, 0)
      val authenticated = factory.read(True, 0x11, 7)

      val anyHalted  = factory.read(harts.map(h =>  h.sel && h.halted     ).orR , 0x11, 8)
      val allHalted  = factory.read(harts.map(h => !h.sel || h.halted     ).andR, 0x11, 9)
      val anyRunning = factory.read(harts.map(h =>  h.sel && h.running    ).orR , 0x11, 10)
      val allRunning = factory.read(harts.map(h => !h.sel || h.running    ).andR, 0x11, 11)
      val anyUnavail = factory.read(harts.map(h =>  h.sel && h.unavailable).orR , 0x11, 12)
      val allUnavail = factory.read(harts.map(h => !h.sel || h.unavailable).andR, 0x11, 13)
      val anyNonExistent = factory.read(dmcontrol.hartSel >= p.harts, 0x11, 14)
      val allNonExistent = factory.read(anyNonExistent, 0x11, 15)
      val anyResumeAck = factory.read(harts.map(h =>  h.sel && h.resumeReady).orR, 0x11, 16)
      val allResumeAck = factory.read(harts.map(h => !h.sel || h.resumeReady).andR, 0x11, 17)
      val anyHaveReset = factory.read(harts.map(h =>  h.sel && h.haveReset).orR, 0x11, 18)
      val allHaveReset = factory.read(harts.map(h => !h.sel || h.haveReset).andR, 0x11, 19)

//      val hasresethaltreq = factory.read(True, 0x11, 5)
      val impebreak = factory.read(True, 0x11, 22)
    }


    val hartInfo = new Area{
      val dataaddr   = factory.read(U(0, 4 bits), 0x12, 0)
      val datasize   = factory.read(U(0, 4 bits), 0x12, 12)
      val dataaccess = factory.read(False, 0x12, 16)
      val nscratch   = factory.read(U(0, 4 bits), 0x12, 20)
    }

    val sb = p.withSysBus generate new Area{
      val sbcs = new Area{
        val sbversion = factory.read(U(1, 3 bits), 0x38, 29)

        val sbaccess = factory.createReadAndWrite(UInt(3 bits), 0x38, 17) init(2)

        val sbbusyerror = factory.createReadAndClearOnSet(Bool(), 0x38, 22) init(False)
        val sbbusy = factory.read(Bool(), 0x38, 21)
        val sbreadonaddr = factory.createReadAndWrite(Bool(), 0x38, 20) init(False)
        val sbautoincrement = factory.createReadAndWrite(Bool(), 0x38, 16) init(False)
        val sbreadondata = factory.createReadAndWrite(Bool(), 0x38, 15) init(False)
        val sberror = factory.createReadAndClearOnSet(UInt(3 bits), 0x38, 12) init(0)
        val sbasize = factory.read(U(32, 7 bits), 0x38, 5)

        // 128 and 64 bit not supported yet
        val sbaccess128 = factory.read(False, 0x38, 4)
        val sbaccess64 = factory.read(False, 0x38, 3)
        // 32 bit access IS supported
        val sbaccess32 = factory.read(True, 0x38, 2)
        // 16 and 8 bit not supported yet
        val sbaccess16 = factory.read(False, 0x38, 1)
        val sbaccess8 = factory.read(False, 0x38, 0)
      }
  
      val sbaddress0 = factory.createReadOnly(UInt(32 bits), 0x39) init(0)
      val sbdata0 = factory.createReadOnly(Bits(32 bits), 0x3c) init(0)

      val sysBusX = new Area {
        val sysBusCmdValid = RegInit(False)
        val sysBusWrite = RegInit(False)
        val sysBusBusy = RegInit(False)
        val sysBusReady = !sbcs.sbbusy && !sbcs.sbbusyerror && (sbcs.sberror === 0)

        sbcs.sbbusy := sysBusBusy

        io.sysBus.cmd.valid := sysBusCmdValid
        io.sysBus.cmd.payload.wr := sysBusWrite
        io.sysBus.cmd.payload.address := sbaddress0
        io.sysBus.cmd.payload.data := sbdata0
        io.sysBus.cmd.payload.size := 3

        when(sysBusBusy && io.sysBus.cmd.fire) {
          sysBusCmdValid := False
          when(sysBusWrite) {
            sysBusBusy := False
          }
        }

        // bus response
        when(io.sysBus.rsp.fire) {
          sbdata0 := io.sysBus.rsp.data
          when(io.sysBus.rsp.error) {
            sbcs.sberror := 7
          }
          sysBusBusy := False
        }

        // sbaddress0 write
        factory.onWrite(0x39) {
          when(sbcs.sbbusy) {
            sbcs.sbbusyerror := True
          } elsewhen(sysBusReady) {
            sbaddress0 := io.ctrl.cmd.payload.data.asUInt
            when(sbcs.sbreadonaddr) {
              when(sbcs.sbaccess =/= 2) {
                // 32 bit accesses only
                sbcs.sberror := 4
              } otherwise {
                sysBusBusy := True
                sysBusWrite := False
                sysBusCmdValid := True
              }
            }
          }
        }

        // sbdata0 write
        factory.onWrite(0x3c) {
          when(sbcs.sbbusy) {
            sbcs.sbbusyerror := True
          } elsewhen(sysBusReady) {
            when(sbcs.sbaccess =/= 2) {
              // 32 bit accesses only
              sbcs.sberror := 4
            } otherwise {
              sbdata0 := io.ctrl.cmd.payload.data
              sysBusBusy := True
              sysBusWrite := True
              sysBusCmdValid := True
            }
          }
        }

        // sbdata0 read
        factory.onRead(0x3c) {
          when(sysBusReady && sbcs.sbreadondata) {
            when(sbcs.sbaccess =/= 2) {
              // 32 bit accesses only
              sbcs.sberror := 4
            } otherwise {
              sysBusBusy := True
              sysBusWrite := False
              sysBusCmdValid := True
              when(sbcs.sbautoincrement) {
                sbaddress0 := sbaddress0 + 4
              }
            }
          }
        }
      }
    }

    val progbufX = new Area{
      val trigged = io.ctrl.cmd.valid && io.ctrl.cmd.write && (io.ctrl.cmd.address & 0x70) === 0x20
      val mem = Mem.fill(p.progBufSize)(Bits(32 bits))
      when(trigged){
        factory.cmdToRsp.error := False
        mem.write(io.ctrl.cmd.address.resized, io.ctrl.cmd.data)
      }
    }

    val dataX = new Area{
      val readMem = Mem.fill(p.datacount)(Bits(32 bits))
      readMem.write(
        address = fromHarts.address.resized,
        data = fromHarts.data,
        enable = fromHarts.valid
      )

      val trigged = False
      val cmdAddress = (io.ctrl.cmd.address - 4).resize(log2Up(p.datacount) bits)
      when(io.ctrl.cmd.valid && io.ctrl.cmd.address >= 0x04 &&  io.ctrl.cmd.address < 0x04 + p.datacount){
        trigged := True

        toHarts.valid   setWhen(io.ctrl.cmd.write)
        toHarts.op      := DebugDmToHartOp.DATA
        toHarts.address := cmdAddress.resized
        toHarts.data    := io.ctrl.cmd.data

        factory.cmdToRsp.error := False
        factory.cmdToRsp.data := readMem.readAsync(cmdAddress.resized)
      }
    }

    val abstractcs = new Area{
      val dataCount = factory.read(U(p.datacount, 4 bits), 0x16, 0)
      val cmdErr = factory.createReadAndClearOnSet(DebugModuleCmdErr(), 0x16, 8) init(DebugModuleCmdErr.NONE)
      val busy = factory.createReadOnly(Bool(), 0x16, 12) init(False)
      val progBufSize = factory.read(U(p.progBufSize, 5 bits), 0x16, 24)

      val noError = cmdErr === DebugModuleCmdErr.NONE
      toHarts.valid clearWhen(busy)
    }

    val abstractAuto = new Area{
      val autoexecdata = factory.createReadAndWrite(Bits(p.datacount bits), 0x18, 0) init(0)
      val autoexecProgbuf = factory.createReadAndWrite(Bits(p.progBufSize bits), 0x18, 16) init(0)

      val trigger = progbufX.trigged && autoexecProgbuf(io.ctrl.cmd.address.resized) || dataX.trigged && autoexecdata(dataX.cmdAddress.resized)
    }

    val command = new StateMachine{
      val withFpuAccess = p.hartsConfig.exists(_.withFpuRegAccess)
      val IDLE, DECODE, READ_INT_REG, WRITE_INT_REG, WAIT_DONE, POST_EXEC, POST_EXEC_WAIT = new State()
      val READ_FPU_REG, WRITE_FPU_REG = withFpuAccess generate new State()

      setEntry(IDLE)
      val executionCounter = Reg(UInt(log2Up(p.progBufSize) bits))
      val commandRequest = factory.isWriting(0x17)
      val data = factory.createWriteOnly(Bits(32 bits), 0x17)
      val access = new Area{
        case class Args() extends Bundle{
          val regno = UInt(16 bits)
          val write = Bool()
          val transfer = Bool()
          val postExec = Bool()
          val aarpostincrement = Bool()
          val aarsize = UInt(3 bits)
        }
        val args = data.as(Args())
        val regnoMsk = if(withFpuAccess) 6 else 5
        val transferFloat = args.regno(5)
        val notSupported = args.aarsize > (transferFloat ? dmcontrol.hartSelAarsizeLimitF | dmcontrol.hartSelAarsizeLimit) || args.aarpostincrement || args.transfer && args.regno(regnoMsk, 16-regnoMsk bits) =/= 0x1000 >> regnoMsk
      }

      val request = commandRequest || abstractAuto.trigger
      when(request && abstractcs.busy && abstractcs.noError){
        abstractcs.cmdErr := DebugModuleCmdErr.BUSY
      }
      when(io.harts.map(e => e.exception).orR){
        abstractcs.cmdErr := DebugModuleCmdErr.EXCEPTION
      }
      when(abstractcs.busy && (progbufX.trigged || dataX.trigged) && abstractcs.noError){
        abstractcs.cmdErr := DebugModuleCmdErr.BUSY
      }

      IDLE.onEntry(
        abstractcs.busy := False
      )
      IDLE.whenIsActive{
        executionCounter := 0
        when(request && abstractcs.noError) {
          when(!io.harts.map(_.halted).read(dmcontrol.hartSel.resized)){
            abstractcs.cmdErr := DebugModuleCmdErr.HALT_RESUME
          } otherwise {
            selected.hart := dmcontrol.hartSel.resized
            abstractcs.busy := True
            goto(DECODE)
          }
        }
      }
      DECODE.whenIsActive{
        goto(IDLE)
        switch(data(31 downto 24)) {
          is(0) { //access register
            when(access.notSupported) {
              abstractcs.cmdErr := DebugModuleCmdErr.NOT_SUPPORTED
            } otherwise {
              when(access.args.postExec){
                goto(POST_EXEC)
              }
              when(access.args.transfer) {
                when(!access.args.regno(5)) {
                  when(access.args.write) {
                    goto(WRITE_INT_REG)
                  } otherwise {
                    goto(READ_INT_REG)
                  }
                } otherwise {
                  if(withFpuAccess) {
                    when(access.args.write) {
                      goto(WRITE_FPU_REG)
                    } otherwise {
                      goto(READ_FPU_REG)
                    }
                  }
                }
              }
            }
          }
          default{
            abstractcs.cmdErr := DebugModuleCmdErr.NOT_SUPPORTED
          }
        }
      }

      def writeInstruction(states : (State, State))(instruction : Bits): Unit ={
        states._1 whenIsActive {
          toHarts.valid := True
          toHarts.op := DebugDmToHartOp.EXECUTE
          toHarts.data := instruction
          goto(states._2)
        }
      }

      writeInstruction(WRITE_INT_REG     -> WAIT_DONE) (csrr(0, access.args.regno(4 downto 0)))
      writeInstruction(READ_INT_REG      -> WAIT_DONE) (csrw(0, access.args.regno(4 downto 0)))

      toHarts.size := access.args.aarsize.resized
      if(withFpuAccess) {
        WRITE_FPU_REG.whenIsActive {
          toHarts.valid := True
          toHarts.op := DebugDmToHartOp.REG_WRITE
          toHarts.address := access.args.regno.resized
          goto(WAIT_DONE)
        }

        READ_FPU_REG.whenIsActive {
          toHarts.valid := True
          toHarts.op := DebugDmToHartOp.REG_READ
          toHarts.address := access.args.regno.resized
          goto(WAIT_DONE)
        }
      }

      WAIT_DONE.whenIsActive{
        when(selected.commit || selected.regSuccess){
          goto(IDLE)
          when(access.args.postExec){
            goto (POST_EXEC)
          }
        }
      }

      POST_EXEC.whenIsActive{
        toHarts.valid := True
        toHarts.op := DebugDmToHartOp.EXECUTE
        toHarts.data := progbufX.mem.readAsync(executionCounter)
        goto(POST_EXEC_WAIT)
      }

      POST_EXEC_WAIT.whenIsActive{
        when(selected.ebreak || selected.exception || selected.commit) {
          executionCounter := executionCounter + 1
          goto(IDLE)
        }
        when(selected.redo || selected.commit && executionCounter =/= p.progBufSize - 1){
          goto(POST_EXEC)
        }
      }
    }
  }
}

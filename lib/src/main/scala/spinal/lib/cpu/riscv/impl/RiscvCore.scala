package spinal.lib.cpu.riscv.impl

import java.text.AttributedString

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.{AhbLite3Master, AhbLite3Config}
import spinal.lib.bus.avalon._
import spinal.lib.bus.avalon._
import spinal.lib.bus.amba4.axi._
import spinal.lib.cpu.riscv.impl.Utils._
import spinal.lib.cpu.riscv.impl.extension.{NativeDataBusExtension, NativeInstructionBusExtension, BarrelShifterLightExtension, CoreExtension}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer




trait BranchPrediction
object disable extends BranchPrediction
object static  extends BranchPrediction
object dynamic extends BranchPrediction

trait RegFileReadKind
object async extends RegFileReadKind
object sync extends RegFileReadKind

trait InstructionBusKind
trait DataBusKind
object cmdStream_rspStream extends InstructionBusKind with DataBusKind
object cmdStream_rspFlow extends InstructionBusKind with DataBusKind


case class RiscvCoreConfig(val pcWidth : Int = 32,
                           val addrWidth : Int = 32,
                           val startAddress : BigInt = 0,
                           val bypassExecute0 : Boolean = true,
                           val bypassExecute1 : Boolean = true,
                           val bypassWriteBack : Boolean = true,
                           val bypassWriteBackBuffer : Boolean = true,
                           val collapseBubble : Boolean = true,
                           val branchPrediction : BranchPrediction = static,
                           val regFileReadyKind : RegFileReadKind = sync,
                           val fastFetchCmdPcCalculation : Boolean = true,
                           val dynamicBranchPredictorCacheSizeLog2 : Int = 4,
                           val branchPredictorHistoryWidth : Int = 2,
                           val invalidInstructionIrqId : Int = 0,
                           val unalignedMemoryAccessIrqId : Int = 1
                       ) {
  val extensions = ArrayBuffer[CoreExtension]()
  def add[T <: CoreExtension](that : T) : T= {
    extensions += that
    that
  }

  def needExecute0PcPlus4 = true //branchPrediction != disable
}

case class CoreInstructionCmd()(implicit p : RiscvCoreConfig) extends Bundle{
  val pc = UInt(p.addrWidth bit)
}

case class CoreInstructionRsp()(implicit p : RiscvCoreConfig) extends Bundle{
  val instruction = Bits(32 bit)
  val pc = UInt(p.addrWidth bit)
  val branchCacheLine = if(p.branchPrediction == dynamic) BranchPredictorLine() else null
}


object CoreInstructionBus{
  def getAvalonConfig(p : RiscvCoreConfig) = AvalonMMConfig.pipelined(
    addressWidth = p.addrWidth,
    dataWidth = 32
  ).getReadOnlyConfig.copy(
    maximumPendingReadTransactions = 1
  )

  def getAhbLite3Config(p : RiscvCoreConfig) = AhbLite3Config(
    addressWidth = p.addrWidth,
    dataWidth = 32
  )

  def getAxi4Config(p : RiscvCoreConfig) = Axi4Config(
    addressWidth = p.addrWidth,
    dataWidth = 32,
    useId = false,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useQos = false,
    useLen = false,
    useResp = false,
    useSize = false
  )
}

case class CoreInstructionBus()(implicit val p : RiscvCoreConfig) extends Bundle with IMasterSlave{
  val cmd = Stream (CoreInstructionCmd())
  val branchCachePort = if(p.branchPrediction == dynamic) MemReadPort(BranchPredictorLine(),p.dynamicBranchPredictorCacheSizeLog2) else null
  val rsp = Stream (CoreInstructionRsp())

  override def asMaster(): Unit = {
    master(cmd)
    slaveWithNull(branchCachePort)
    slave(rsp)
  }

//  def toAxiRead(): AxiBus ={
//    val axiParameters = Axi4Config(
//      addressWidth = 32,
//      dataWidth = 32,
//      mode = READ_ONLY
//    )
//    val axi = new AxiBus(axiParameters)
//
//    axi.readCmd.translateFrom(cmd)((to,from) => {
//      to.addr := from.pc
//      to.prot := 0
//    })
//    rsp.translateFrom(axi.readRsp)((to,from) => {
//      to.instruction := from.data
//    })
//
//    axi
//  }

  def toAvalon(): AvalonMM = {
    val avalonConfig = CoreInstructionBus.getAvalonConfig(p)
    val mm = AvalonMM(avalonConfig)
    val pendingCmd = RegInit(False)
    pendingCmd := (pendingCmd && !mm.readDataValid) || mm.fire
    val haltCmd = rsp.isStall || (pendingCmd && !mm.readDataValid) // Don't overflow the backupFifo and don't have more than one pending cmd

    mm.read := cmd.valid && !haltCmd
    mm.address := cmd.pc
    cmd.ready := mm.waitRequestn && !haltCmd

    val backupFifoIn = Stream(CoreInstructionRsp())
    backupFifoIn.valid := mm.readDataValid
    backupFifoIn.instruction := mm.readData
    backupFifoIn.pc := RegNextWhen(cmd.pc,cmd.ready)

    rsp </< backupFifoIn //1 depth of backup fifo, zero latency

    if(p.branchPrediction == dynamic) {
      branchCachePort.cmd.valid := cmd.fire
      branchCachePort.cmd.payload := (cmd.pc >> 2).resized
      backupFifoIn.branchCacheLine := branchCachePort.rsp
    }

    mm
  }


  def toAhbLite3() : AhbLite3Master = {
    val ahbConfig = CoreInstructionBus.getAhbLite3Config(p)
    val mm = AhbLite3Master(ahbConfig)
    val readDataValid = RegNext(False) setWhen(mm.HTRANS(1) && mm.HREADY) init(False)
    val haltCmd = rsp.isStall

    mm.HTRANS := (cmd.valid && !haltCmd) ? B"10" | B"00"
    mm.HADDR := cmd.pc
    mm.HWRITE := False
    mm.HSIZE := 2
    mm.HBURST := 0
    mm.HPROT := 0
    mm.HWDATA := 0
    mm.HMASTLOCK := False

    cmd.ready := mm.HREADY && !haltCmd

    val backupFifoIn = Stream(CoreInstructionRsp())
    backupFifoIn.valid := readDataValid
    backupFifoIn.instruction := mm.HRDATA
    backupFifoIn.pc := RegNextWhen(cmd.pc,cmd.ready)

    rsp </< backupFifoIn //1 depth of backup fifo, zero latency

    if(p.branchPrediction == dynamic) {
      branchCachePort.cmd.valid := cmd.fire
      branchCachePort.cmd.payload := (cmd.pc >> 2).resized
      backupFifoIn.branchCacheLine := branchCachePort.rsp
    }

    mm
  }


  def toAxi4ReadOnly(): Axi4ReadOnly = {
    val axi4Config = CoreInstructionBus.getAxi4Config(p)
    val mm = Axi4ReadOnly(axi4Config)
    val pendingCmd = RegInit(False)
    pendingCmd := (pendingCmd && !mm.readRsp.valid) || mm.readCmd.fire
    val haltCmd = rsp.isStall || (pendingCmd && !mm.readRsp.valid) // Don't overflow the backupFifo and don't have more than one pending cmd

    mm.readCmd.valid := cmd.valid && !haltCmd
    mm.readCmd.addr  := cmd.pc(mm.readCmd.addr.getWidth -1 downto 2) @@ U"00"
    mm.readCmd.prot  := "110"
    mm.readCmd.cache := "1111"
    cmd.ready := mm.readCmd.ready && !haltCmd

    val backupFifoIn = Stream(CoreInstructionRsp())
    backupFifoIn.valid := mm.readRsp.valid
    backupFifoIn.instruction := mm.readRsp.data
    backupFifoIn.pc := RegNextWhen(cmd.pc,cmd.ready)

    rsp </< backupFifoIn //1 depth of backup fifo, zero latency

    if(p.branchPrediction == dynamic) {
      branchCachePort.cmd.valid := cmd.fire
      branchCachePort.cmd.payload := (cmd.pc >> 2).resized
      backupFifoIn.branchCacheLine := branchCachePort.rsp
    }

    mm.readRsp.ready := True

    mm
  }
}

object CoreDataBus{
  def getAvalonConfig(p : RiscvCoreConfig) = AvalonMMConfig.pipelined(
    addressWidth = 32,
    dataWidth = 32).copy(
      useByteEnable = true,
      maximumPendingReadTransactions = 2
    )


  def getAhbLite3Config(p : RiscvCoreConfig) = AhbLite3Config(
    addressWidth = p.addrWidth,
    dataWidth = 32
  )

  def getAxi4Config(p : RiscvCoreConfig) = Axi4Config(
    addressWidth = p.addrWidth,
    dataWidth = 32,
    useId = false,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useQos = false,
    useLen = false,
    useResp = false
  )
}

case class CoreDataBus()(implicit val p : RiscvCoreConfig) extends Bundle with IMasterSlave{
  val cmd = Stream (CoreDataCmd())
  val rsp = Stream (Bits(32 bit))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }


  //stageCmd can only be false if the minimal latency of reads is >= 2
  def toAvalon(stageCmd : Boolean = true): AvalonMM = {
    val avalonConfig = CoreDataBus.getAvalonConfig(p)
    val mm = AvalonMM(avalonConfig)
    val cmdStage = if(stageCmd) cmd.stage else cmd
    mm.read := cmdStage.valid && !cmdStage.wr
    mm.write := cmdStage.valid && cmdStage.wr
    mm.address := cmdStage.address(cmdStage.address.high downto 2) @@ U"00"
    mm.writeData := cmdStage.size.mux (
      U(0) -> cmdStage.data(7 downto 0) ## cmdStage.data(7 downto 0) ## cmdStage.data(7 downto 0) ## cmdStage.data(7 downto 0),
      U(1) -> cmdStage.data(15 downto 0) ## cmdStage.data(15 downto 0),
      default -> cmdStage.data(31 downto 0)
    )
    mm.byteEnable := (cmdStage.size.mux (
      U(0) -> B"0001",
      U(1) -> B"0011",
      default -> B"1111"
    ) << cmdStage.address(1 downto 0)).resized

    val contextIn = Stream(UInt(2 bit))
    contextIn.valid := cmdStage.fire && !cmdStage.wr
    contextIn.payload := cmdStage.address(1 downto 0)

    val contextOut = contextIn.m2sPipe().s2mPipe()
    contextOut.ready := rsp.fire

    cmdStage.ready := mm.waitRequestn
    rsp.valid := mm.readDataValid
    //rsp.payload := mm.readData >> (contextOut.payload*8)
    rsp.payload := mm.readData
    switch(contextOut.payload){
      is(1){rsp.payload(7 downto 0)  := mm.readData(15 downto 8)}
      is(2){rsp.payload(15 downto 0) := mm.readData(31 downto 16)}
      is(3){rsp.payload(7 downto 0)  := mm.readData(31 downto 24)}
    }

    mm
  }


  def toAhbLite3() : AhbLite3Master = {
    val avalonConfig = CoreDataBus.getAhbLite3Config(p)
    val mm = AhbLite3Master(avalonConfig)
    val readDataValid = RegNext(False) setWhen(mm.HTRANS(1) && mm.HREADY) init(False)
    val cmd = this.cmd.stage
    mm.HTRANS := cmd.valid ? B"10" | B"00"
    mm.HADDR := cmd.address
    mm.HWRITE := cmd.wr
    mm.HSIZE := 2
    mm.HBURST := 0
    mm.HPROT := 1
    mm.HMASTLOCK := False
    mm.HWDATA := RegNextWhen(
      next = cmd.size.mux (
        U(0) -> cmd.data(7 downto 0) ## cmd.data(7 downto 0) ## cmd.data(7 downto 0) ## cmd.data(7 downto 0),
        U(1) -> cmd.data(15 downto 0) ## cmd.data(15 downto 0),
        default -> cmd.data(31 downto 0)
      ),
      cond = mm.HREADY
    )

    val contextIn = Stream(UInt(2 bit))
    contextIn.valid := cmd.fire && !cmd.wr
    contextIn.payload := cmd.address(1 downto 0)

    val contextOut = contextIn.m2sPipe()
    contextOut.ready := rsp.fire

    cmd.ready := mm.HREADY
    rsp.valid := readDataValid
    //rsp.payload := mm.readData >> (contextOut.payload*8)
    rsp.payload := mm.HRDATA
    switch(contextOut.payload){
      is(1){rsp.payload(7 downto 0)  := mm.HRDATA(15 downto 8)}
      is(2){rsp.payload(15 downto 0) := mm.HRDATA(31 downto 16)}
      is(3){rsp.payload(7 downto 0)  := mm.HRDATA(31 downto 24)}
    }

    mm
  }


  //stageCmd can only be false if the minimal latency of reads is >= 2
  def toAxi4Shared(stageCmd : Boolean = true): Axi4Shared = {
    val area = new Area {
      val Axi4SharedConfig = CoreDataBus.getAxi4Config(p)
      val mm = Axi4Shared(Axi4SharedConfig)
      val pendingMax = 7
      val pendingCmd = CounterUpDown(
        stateCount = pendingMax + 1,
        incWhen = mm.sharedCmd.fire,
        decWhen = (mm.readRsp.fire && mm.readRsp.last) || mm.writeRsp.fire
      )
      val pendingIsWrite = RegNextWhen(mm.sharedCmd.write, mm.sharedCmd.fire) randBoot()

      val cmdPreFork = if (stageCmd) cmd.stage else cmd
      val (cmdFork, dataFork) = StreamFork2(cmdPreFork.haltWhen((pendingCmd =/= 0 && (pendingIsWrite ^ cmdPreFork.wr)) || pendingCmd === pendingMax))
      mm.sharedCmd.valid := cmdFork.valid
      mm.sharedCmd.write := cmdFork.wr
      mm.sharedCmd.prot := "010"
      mm.sharedCmd.cache := "1111"
      mm.sharedCmd.size := cmdFork.size.resized
      mm.sharedCmd.addr := cmdFork.address

      val dataStage = dataFork.throwWhen(!dataFork.wr)
      mm.writeData.arbitrationFrom(dataStage)
      mm.writeData.last := True
      mm.writeData.data := dataStage.size.mux(
        U(0) -> dataStage.data(7 downto 0) ## dataStage.data(7 downto 0) ## dataStage.data(7 downto 0) ## dataStage.data(7 downto 0),
        U(1) -> dataStage.data(15 downto 0) ## dataStage.data(15 downto 0),
        default -> dataStage.data(31 downto 0)
      )
      mm.writeData.strb := (dataStage.size.mux(
        U(0) -> B"0001",
        U(1) -> B"0011",
        default -> B"1111"
      ) << dataStage.address(1 downto 0)).resized

      val contextIn = Stream(UInt(2 bit))
      contextIn.valid := cmdFork.fire && !cmdFork.wr
      contextIn.payload := cmdFork.address(1 downto 0)

      val contextOut = contextIn.m2sPipe().s2mPipe()
      contextOut.ready := rsp.fire

      cmdFork.ready := mm.sharedCmd.ready
      rsp.valid := mm.readRsp.valid
      //rsp.payload := mm.readData >> (contextOut.payload*8)
      rsp.payload := mm.readRsp.data
      switch(contextOut.payload) {
        is(1) {
          rsp.payload(7 downto 0) := mm.readRsp.data(15 downto 8)
        }
        is(2) {
          rsp.payload(15 downto 0) := mm.readRsp.data(31 downto 16)
        }
        is(3) {
          rsp.payload(7 downto 0) := mm.readRsp.data(31 downto 24)
        }
      }

      mm.writeRsp.ready := True
      mm.readRsp.ready := True
    }
    area.setName("dBridge")
    area.mm

  }
}

case class CoreDataCmd()(implicit val p : RiscvCoreConfig) extends Bundle{
  val wr = Bool
  val address = UInt(p.addrWidth bit)
  val data = Bits(32 bit)
  val size = UInt(2 bit)
}

case class BranchPredictorLine()(implicit val p : RiscvCoreConfig)  extends Bundle{
  val pc = UInt(p.pcWidth-p.dynamicBranchPredictorCacheSizeLog2-2 bit)
  val history = SInt(p.branchPredictorHistoryWidth bit)
}

case class CoreFetchOutput()(implicit val p : RiscvCoreConfig) extends Bundle{
  val pc = UInt(p.pcWidth bit)
  val instruction = Bits(32 bit)
  val branchCacheLine = BranchPredictorLine()
}

case class CoreDecodeOutput()(implicit val p : RiscvCoreConfig) extends Bundle{
  val pc = UInt(p.pcWidth bit)
  val instruction = Bits(32 bit)
  val ctrl = InstructionCtrl()
  val src0 = Bits(32 bit)
  val src1 = Bits(32 bit)
  val alu_op0 = Bits(32 bit)
  val alu_op1 = Bits(32 bit)
  val doSub = Bool
  val predictorHasBranch = Bool
  val branchHistory = Flow(SInt(p.branchPredictorHistoryWidth bit))
}

case class CoreExecute0Output()(implicit val p : RiscvCoreConfig) extends Bundle{
  val pc = UInt(p.pcWidth bit)
  val instruction = Bits(32 bit)
  val ctrl = InstructionCtrl()
  val br = new Bundle {
    val eq, ltx = Bool
  }
  val src1 = Bits(32 bit)
  val result = Bits(32 bit)
  val adder = UInt(32 bit)
  val predictorHasBranch = Bool
  val branchHistory = Flow(SInt(p.branchPredictorHistoryWidth bit))
  val pcPlus4 = if(p.needExecute0PcPlus4) UInt(32 bit) else null
  val pc_sel = PC()
  val unalignedMemoryAccessException = Bool
  val needMemRsp = Bool
  val dCmdAddress = UInt(p.addrWidth bits)
}

case class CoreExecute1Output()(implicit val p : RiscvCoreConfig) extends Bundle{
  val pc = UInt(p.pcWidth bit)
  val instruction = Bits(32 bit)
  val ctrl = InstructionCtrl()
  val result = Bits(32 bit)
  val regFileAddress = UInt(5 bit)
  val pcPlus4 = UInt(32 bit)
  val unalignedMemoryAccessException = Bool
  val needMemRsp = Bool
  val dCmdAddress = UInt(p.addrWidth bits)
}

case class CoreWriteBack0Output()(implicit val p : RiscvCoreConfig) extends Bundle{
  val addr = UInt(5 bit)
  val data = Bits(32 bit)
}

class RiscvCore(implicit val c : RiscvCoreConfig) extends Component{
  import c._

  //Instruction bus
  val iCmd = Stream(CoreInstructionCmd())
  val iRsp = Stream(CoreInstructionRsp())

  //Data bus
  val dCmd = Stream(CoreDataCmd())
  val dRsp = Stream(Bits(32 bits))
  val dataBusKind : DataBusKind = if(c.extensions.foldLeft(false)(_ || _.needFlowDRsp))
    cmdStream_rspFlow
  else
    cmdStream_rspStream



  //IRQ
  val irqUsages = mutable.HashMap[Int,IrqUsage]()
  if(invalidInstructionIrqId != 0) irqUsages(invalidInstructionIrqId) = IrqUsage(true)
  if(unalignedMemoryAccessIrqId != 0) irqUsages(unalignedMemoryAccessIrqId) = IrqUsage(true)
  for(extension <- extensions){
    for((id,usage) <- extension.getIrqUsage){
      irqUsages(id) = usage
    }
  }
  val irqWidth = irqUsages.foldLeft(0)((max,e) => Math.max(max,e._1)) + 1
  val irqExceptionMask = irqUsages.foldLeft(0)((mask,e) => if(e._2.isException) mask + 1 << e._1 else mask)

  //Memories
  val regFile = Mem(Bits(32 bit),32)
  val brancheCache = Mem(BranchPredictorLine(), 1<<dynamicBranchPredictorCacheSizeLog2) randBoot()


  //Send instruction request to io.i.cmd
  val prefetch = new Area {
    val halt = False
    val pc = Reg(UInt(pcWidth bit)) init(U(startAddress,pcWidth bit))
    val inc = RegInit(False) //when io.i.cmd is stalled, it's used as a token to continue the request the next cycle
    val pcNext = if(fastFetchCmdPcCalculation){
      val pcPlus4 = pc + U(4)
      pcPlus4.addAttribute("keep")
      Mux(inc,pcPlus4,pc)
    }else{
      pc + Mux(inc,U(4),U(0))
    }
    val pcLoad = Flow(pc)
    when(pcLoad.valid){
      pcNext := pcLoad.payload
    }

    val resetDone = RegNext(True) init(False) //Used to not send request while reset is active
    iCmd.valid := resetDone  && !halt
    iCmd.pc := pcNext

    when(iCmd.fire || pcLoad.fire){
      pc := pcNext
    }

    when(iCmd.fire){
      inc := True
    }elsewhen(pcLoad.valid){
      inc := False
    }
  }

  //Join fetchCmd.outInst with io.i.rsp
  val fetch = new Area {
    val outInst = Stream(CoreFetchOutput())
    val throwIt = False
    val flush = False
    when(flush){
      throwIt := True
    }


    val pendingPrefetch = CounterUpDown(stateCount = 4,incWhen = iCmd.fire,decWhen = iRsp.fire)
    when(pendingPrefetch === 3){
      iCmd.valid := False
    }

    val throwRemaining = Reg(UInt(2 bit)) init(0)
    val throwNextIRsp = throwRemaining =/= 0
    when(throwNextIRsp && iRsp.fire){
      throwRemaining := throwRemaining - 1
    }
    when(throwIt){
      throwRemaining := pendingPrefetch - iRsp.valid.asUInt
    }


    outInst.arbitrationFrom(iRsp.throwWhen(throwIt || throwNextIRsp))
    outInst.pc := iRsp.pc
    outInst.instruction := iRsp.instruction
    if(branchPrediction == dynamic)
      outInst.branchCacheLine := iRsp.branchCacheLine
    else
      outInst.branchCacheLine := outInst.branchCacheLine.getZero //don't care
  }


  val decode = new Area{
    val inInst = fetch.outInst.m2sPipe()
    val ctrl = getInstructionCtrl(inInst.instruction)
    val hazard = Bool //Used to stall decode phase because of register file hazard
    val throwIt = False
    val halt = False
    when(hazard){
      halt := True
    }
    val addr0 = inInst.instruction(src0Range).asUInt
    val addr1 = inInst.instruction(src1Range).asUInt
    val addr0IsZero = addr0 === 0
    val addr1IsZero = addr1 === 0

    //read register file
    val srcInstruction = regFileReadyKind match{
      case `async` => inInst.instruction
      case `sync` =>  Mux(inInst.isStall,inInst.instruction,fetch.outInst.instruction)
    }

    val regFileReadAddress0 = srcInstruction(src0Range).asUInt
    val regFileReadAddress1 = srcInstruction(src1Range).asUInt

    val (src0,src1) = regFileReadyKind match{
      case `async` => (regFile.readAsync(regFileReadAddress0),regFile.readAsync(regFileReadAddress1))
      case `sync` =>  (regFile.readSync(regFileReadAddress0),regFile.readSync(regFileReadAddress1))
    }

    val imm = IMM(inInst.instruction)

    // calculate branch target
    val brjmpImm = Mux(ctrl.jmp, imm.j_sext, imm.b_sext)
    val brJumpPc = (inInst.pc + brjmpImm.asUInt).resize(c.pcWidth)

    // branch prediction
    val branchCacheHit = inInst.branchCacheLine.pc === inInst.pc(pcWidth-1 downto 2 + dynamicBranchPredictorCacheSizeLog2)
    val staticBranchPrediction = brjmpImm.msb || ctrl.br === BR.J
    val shouldTakeBranch = Bool
    branchPrediction match{
      case `disable` =>
        shouldTakeBranch := False
      case `static` =>
        shouldTakeBranch := staticBranchPrediction
      case `dynamic` =>
        shouldTakeBranch := staticBranchPrediction
        when(branchCacheHit) {
          shouldTakeBranch := inInst.branchCacheLine.history.msb
        }
    }


    val outInst = Stream(CoreDecodeOutput())

    // branch interface
    val pcLoad = Flow(UInt(pcWidth bit))
    pcLoad.valid := inInst.valid && !throwIt && !hazard && outInst.ready && (ctrl.br =/= BR.JR && ctrl.br =/= BR.N) && ctrl.instVal && shouldTakeBranch
    pcLoad.payload := brJumpPc

    outInst.arbitrationFrom(inInst.throwWhen(throwIt).haltWhen(halt))
    outInst.pc := inInst.pc
    outInst.instruction := inInst.instruction
    outInst.ctrl := ctrl
    outInst.doSub := outInst.ctrl.alu =/= ALU.ADD
    outInst.src0 := Mux(!addr0IsZero, src0, B(0, 32 bit))
    outInst.src1 := Mux(!addr1IsZero, src1, B(0, 32 bit))
    outInst.alu_op0 := outInst.ctrl.op0.mux(
      default -> outInst.src0,
      OP0.IMU -> imm.u.resized,
      OP0.IMZ -> imm.z.resized,
      OP0.IMJB -> brjmpImm
    )
    outInst.alu_op1 := outInst.ctrl.op1.mux(
      default -> outInst.src1,
      OP1.IMI -> imm.i_sext.resized,
      OP1.IMS -> imm.s_sext.resized,
      OP1.PC -> inInst.pc.asBits
    )
    outInst.predictorHasBranch := pcLoad.valid
    outInst.branchHistory.valid := branchCacheHit
    outInst.branchHistory.payload := inInst.branchCacheLine.history


    val flush = False
    when(flush){
      fetch.throwIt := True
      throwIt := True
    }
  }

  val execute0 = new Area {
    val inInst = decode.outInst.m2sPipe(collapseBubble)
    val throwIt = False
    val halt = False
    val haltFromDataRequest = inInst.valid && inInst.ctrl.men && !dCmd.ready

    val ctrl = inInst.ctrl

    val imm = IMM(inInst.instruction)

    //conditional branch calculation
    val br = new Area {
      val signed = BR.isSignedComp(ctrl.br)
      val src0Ext = (inInst.src0.msb && signed) ## inInst.src0
      val src1Ext = (inInst.src1.msb && signed) ## inInst.src1
      val ltx =  (src0Ext.asUInt-src1Ext.asUInt).msb
      val eq = inInst.src0 === inInst.src1


      val pc_sel = inInst.ctrl.br.mux[PC.C](
        default -> PC.INC,
        BR.NE -> Mux(!eq, PC.BRA, PC.INC),
        BR.EQ -> Mux(eq, PC.BRA, PC.INC),
        (BR.GE , BR.GEU) -> Mux(!ltx, PC.BRA, PC.INC),
        (BR.LT , BR.LTU)  -> Mux(ltx, PC.BRA, PC.INC),
        BR.J -> PC.J,
        BR.JR -> PC.JR
      )
    }

    val alu = new Alu
    alu.io.func := inInst.ctrl.alu
    alu.io.doSub := inInst.doSub
    alu.io.src0 := inInst.alu_op0
    alu.io.src1 := inInst.alu_op1

    val outInst = Stream(CoreExecute0Output())
    outInst.arbitrationFrom(inInst.throwWhen(throwIt).haltWhen(halt || haltFromDataRequest))
    outInst.pc := inInst.pc
    outInst.instruction := inInst.instruction
    outInst.predictorHasBranch := inInst.predictorHasBranch
    outInst.branchHistory.valid   := inInst.branchHistory.valid
    outInst.branchHistory.payload := inInst.branchHistory.payload
    outInst.ctrl := ctrl
    outInst.br.eq := br.eq
    outInst.br.ltx := br.ltx
    outInst.pc_sel := br.pc_sel
    outInst.src1 := inInst.src1
    outInst.result := alu.io.result
    outInst.adder := alu.io.adder
    if(c.needExecute0PcPlus4) outInst.pcPlus4 := inInst.pc + 4
    outInst.needMemRsp := inInst.ctrl.men && inInst.ctrl.m === M.XRD
    outInst.dCmdAddress := dCmd.address

    // Send memory read/write requests
    outInst.unalignedMemoryAccessException := inInst.ctrl.men && outInst.ctrl.msk.mux(
      default-> False,
      MSK.H -> dCmd.address(0),
      MSK.W -> (dCmd.address(0) || dCmd.address(1))
    )


    dCmd.valid := inInst.valid && inInst.ctrl.men && !outInst.unalignedMemoryAccessException && !halt && !throwIt && outInst.ready
    dCmd.wr := inInst.ctrl.m === M.XWR
    dCmd.address := outInst.adder
    dCmd.payload.data := inInst.src1
    dCmd.size := inInst.ctrl.msk.mux(
      default -> U(2), //W
      MSK.B -> U(0),
      MSK.H -> U(1)
    )
    val pendingDataCmd = new Area{
      val pendingDataMax = 2
      val readCount = Reg(UInt(log2Up(pendingDataMax+1) bit)) init(0)
      val readCountInc = dCmd.fire && !dCmd.wr
      val readCountDec = dRsp.fire

      when(readCountInc =/= readCountDec){
        readCount := readCount + Mux(readCountInc,U(1),U(readCount.maxValue))
      }
      when(inInst.valid && inInst.ctrl.men && inInst.ctrl.m === M.XRD && readCount === pendingDataMax){
        halt := True
      }
    }

    val flush = False
    when(flush){
      fetch.throwIt := True
      decode.throwIt := True
      throwIt := True
    }
  }

  val execute1 = new Area {
    val inInst = execute0.outInst.m2sPipe(collapseBubble)
    val halt = False
    val throwIt = False

    val pc_sel = inInst.pc_sel


    // branche interface
    val pcLoad = Flow(UInt(pcWidth bit))
    pcLoad.valid := !throwIt && inInst.fire && pc_sel.mux(
      PC.INC -> inInst.predictorHasBranch,
      default -> !inInst.predictorHasBranch
    )

    pcLoad.payload := (c.branchPrediction match {
      case `disable` => inInst.adder
      case _ => pc_sel.mux(
        PC.INC -> inInst.pcPlus4,
        default -> inInst.adder
      )
    })

    // dynamic branch predictor history update
    val line = BranchPredictorLine()
    val newHistory = inInst.branchHistory.payload.resize(branchPredictorHistoryWidth + 1) + Mux(pc_sel === PC.INC,S(1),S(-1))
    line.pc := inInst.pc(pcWidth-1 downto 2 + dynamicBranchPredictorCacheSizeLog2)
    when(inInst.branchHistory.valid){
      line.history := newHistory.resized
    }otherwise {
      line.history := (pc_sel =/= PC.INC).asSInt.resized
    }

    //TODO Performance drop 11/06/2016 ?? 113kcycles
    when(inInst.fire && inInst.ctrl.br =/= BR.JR && inInst.ctrl.br =/= BR.N && inInst.ctrl.br =/= BR.J){
      when(newHistory(newHistory.high downto newHistory.high - 1) =/= S"10" && newHistory(newHistory.high downto newHistory.high - 1) =/= S"01") { //no history overflow  TODO fix me
        brancheCache(inInst.pc(2, dynamicBranchPredictorCacheSizeLog2 bit)) := line
      }
    }

    val outInst = Stream(CoreExecute1Output())
    outInst.arbitrationFrom(inInst.throwWhen(throwIt).haltWhen(halt))
    outInst.pc := inInst.pc
    outInst.result := inInst.result
    outInst.regFileAddress := inInst.instruction(dstRange).asUInt
    outInst.ctrl := inInst.ctrl
    outInst.instruction := inInst.instruction
    outInst.pcPlus4 := (if(c.needExecute0PcPlus4) inInst.pcPlus4 else inInst.pc + 4)
    outInst.unalignedMemoryAccessException := inInst.unalignedMemoryAccessException
    outInst.needMemRsp := inInst.needMemRsp
    outInst.dCmdAddress := inInst.dCmdAddress
    val flush = False
    when(flush){
      fetch.throwIt := True
      decode.throwIt := True
      execute0.throwIt := True
      throwIt := True
    }
  }

  val writeBack = new Area{
    val inInst = execute1.outInst.m2sPipe(collapseBubble)
    val throwIt = !inInst.ctrl.rfen
    val halt = False

    val irq = new Area {
      val sources = B(0,irqWidth bit)
      val mask = Reg(Bits(irqWidth bit)) init(0)
      val masked = sources & mask
      val inhibate = False
      when(((sources & ~mask) & irqExceptionMask) =/= 0){
        halt := True
      }

      when(inInst.valid) {
        if(invalidInstructionIrqId != -1){
          when(!inInst.ctrl.instVal) {
            sources(invalidInstructionIrqId) := True
          }
        }
        if(unalignedMemoryAccessIrqId != -1){
          when(inInst.unalignedMemoryAccessException) {
            sources(unalignedMemoryAccessIrqId) := True
          }
        }
      }
    }


    val pcLoad = Flow(UInt(pcWidth bit))
    pcLoad.valid := False
    pcLoad.payload.assignDontCare()

    val needMemoryResponse = inInst.needMemRsp
    val flushMemoryResponse = RegInit(False)
    dRsp.ready := (dataBusKind match{
      case `cmdStream_rspStream` => False
      case `cmdStream_rspFlow` => True
    })
    when(inInst.valid && needMemoryResponse){
      when(!dRsp.valid) {
        halt := True
      }
      if(dataBusKind == cmdStream_rspStream) dRsp.ready := !halt
    }

    when(execute0.pendingDataCmd.readCount === 0){
      flushMemoryResponse := False
    }
    when(flushMemoryResponse){
      dRsp.ready := True
      halt := True
    }

    val dataRspFormated = inInst.ctrl.msk.mux(
      default -> dRsp.payload, //W
      MSK.B   -> B((31 downto 8) -> (dRsp.payload(7) && ! inInst.instruction(14)),(7 downto 0) -> dRsp.payload(7 downto 0)),
      MSK.H   -> B((31 downto 16) -> (dRsp.payload(15) && ! inInst.instruction(14)),(15 downto 0) -> dRsp.payload(15 downto 0))
    )

    val regFileData = inInst.ctrl.wb.mux (
      default -> B(0,32 bit), //CSR1
      WB.ALU -> inInst.result,
      WB.MEM  -> dataRspFormated,
      WB.PC4  -> (inInst.pcPlus4).asBits.resized
    )


    val outInst = Stream(CoreWriteBack0Output())
    outInst.arbitrationFrom(inInst.throwWhen(throwIt).haltWhen(halt))
    outInst.addr := inInst.regFileAddress
    outInst.data := regFileData

    val regFileWrite = regFile.writePort
    regFileWrite.valid := outInst.fire
    regFileWrite.address := outInst.addr
    regFileWrite.data := regFileData


    val flush = False
    when(flush){
      fetch.throwIt := True
      decode.throwIt := True
      execute0.throwIt := True
      execute1.throwIt := True
      throwIt := True
    }
  }

  //This stage is only about keep a trace of last writeBack, trace used later to avoid read during write hazard on register file
  val writeBackBuffer = new Area{
    val inInst = writeBack.outInst.m2sPipe(collapseBubble)
    inInst.ready := True
  }



  // apply decode/execute1 pcLoad interfaces to fetchCmd.pcLoad
  val branchArbiter = new Area {
    branchPrediction match{
      case `disable` =>
        prefetch.pcLoad.valid := execute1.pcLoad.valid
        prefetch.pcLoad.payload := execute1.pcLoad.payload
        when(execute1.pcLoad.valid) {
          execute0.flush := True
        }
      case `static` | `dynamic` =>
        prefetch.pcLoad.valid := decode.pcLoad.valid
        prefetch.pcLoad.payload := decode.pcLoad.payload
        when(decode.pcLoad.valid){
          fetch.flush := True
        }
        when(execute1.pcLoad.valid){
          execute0.flush :=  True
          prefetch.pcLoad.valid := True
          prefetch.pcLoad.payload := execute1.pcLoad.payload
        }
    }

    when(writeBack.pcLoad.valid){
      execute1.flush :=  True
      prefetch.pcLoad.valid := True
      prefetch.pcLoad.payload := writeBack.pcLoad.payload
    }
  }



  // Check hazard and apply bypass logic
  val hazardTracker = new  Area {
    val src0Hazard = False
    val src1Hazard = False
    decode.hazard := src0Hazard || src1Hazard

    // write back bypass and hazard
    val W2R = new Area {
      val addr0Match = writeBackBuffer.inInst.addr === decode.addr0
      val addr1Match = writeBackBuffer.inInst.addr === decode.addr1
      when(writeBackBuffer.inInst.valid) {
        if (bypassWriteBackBuffer) {
          when(addr0Match) {
            decode.src0 := writeBackBuffer.inInst.data
          }
          when(addr1Match) {
            decode.src1 := writeBackBuffer.inInst.data
          }
        } else {
          when(addr0Match) {
            src0Hazard := True
          }
          when(addr1Match) {
            src1Hazard := True
          }
        }
      }
    }


    // memory access bypass and hazard
    val A = new Area{
      val addr0Match = writeBack.outInst.addr === decode.addr0
      val addr1Match = writeBack.outInst.addr === decode.addr1
      when(writeBack.inInst.valid && writeBack.inInst.ctrl.rfen){
        if(bypassWriteBack) {
          when(addr0Match) {
            decode.src0 := writeBack.regFileData
          }
          when(addr1Match) {
            decode.src1 := writeBack.regFileData
          }
        }
        when((Bool(!bypassWriteBack) || !writeBack.outInst.valid)) {
          when(addr0Match) {
            src0Hazard := True
          }
          when(addr1Match) {
            src1Hazard := True
          }
        }
      }
    }

    // execute1 bypass and hazard
    val E1 = new Area{
      val addr0Match = execute1.outInst.instruction(dstRange).asUInt === decode.addr0
      val addr1Match = execute1.outInst.instruction(dstRange).asUInt === decode.addr1
      when(execute1.inInst.valid && execute1.outInst.ctrl.rfen) {
        if (bypassExecute1) {
          when(execute1.outInst.ctrl.execute1AluBypass) {
            when(addr0Match) {
              decode.src0 := execute1.outInst.result
            }
            when(addr1Match) {
              decode.src1 := execute1.outInst.result
            }
          }
        }
        when((Bool(!bypassExecute1) || !execute1.inInst.ctrl.execute1AluBypass || !execute1.outInst.valid)) {
          when(addr0Match) {
            src0Hazard := True
          }
          when(addr1Match) {
            src1Hazard := True
          }
        }
      }
    }

    // execute0 bypass and hazard
    val E0 = new Area {
      val addr0Match = execute0.outInst.instruction(dstRange).asUInt === decode.addr0
      val addr1Match = execute0.outInst.instruction(dstRange).asUInt === decode.addr1
      when(execute0.inInst.valid && execute0.outInst.ctrl.rfen) {
        if (bypassExecute0) {
          when(execute0.outInst.ctrl.execute0AluBypass) {
            when(addr0Match) {
              decode.src0 := execute0.outInst.result
            }
            when(addr1Match) {
              decode.src1 := execute0.outInst.result
            }
          }
        }
        when((Bool(!bypassExecute0) || !execute0.inInst.ctrl.execute0AluBypass || !execute0.outInst.valid)) {
          when(addr0Match) {
            src0Hazard := True
          }
          when(addr1Match) {
            src1Hazard := True
          }
        }
      }
    }

    when(decode.addr0IsZero || !decode.ctrl.useSrc0){
      src0Hazard := False
    }
    when(decode.addr1IsZero || !decode.ctrl.useSrc1){
      src1Hazard := False
    }
  }

  

  val noDataRspStallLogic = if(dataBusKind == cmdStream_rspFlow) new Area{
    when(execute0.inInst.valid && execute0.inInst.ctrl.men && execute0.inInst.ctrl.m === M.XRD){
      when(execute1.inInst.valid && execute1.inInst.ctrl.canInternalyStallWriteBack0){
        execute0.halt := True
      }
      when(writeBack.inInst.isStall && writeBack.inInst.ctrl.canInternalyStallWriteBack0){
        execute0.halt := True
      }
    }
  }

  
  //Apply core extensions
  for(extension <- extensions){
    val area = extension.applyIt(this)
    area.setName(extension.getName)
  }
  def getInstructionCtrl(instruction : Bits) = {
    applyExtensionTags
    val ctrl = InstructionCtrl(instruction)
    for(extension <- extensions){
      extension.instructionCtrlExtension(instruction,ctrl)
    }
    ctrl
  }
  lazy val applyExtensionTags ={
    var tagCounter = 0
    for(extension <- extensions){
      if(extension.needTag){
        tagCounter += 1
        extension.tag = tagCounter
      }
    }
  }

  // profiling/debug counters
  val performanceCounters = new Area{
    val decode_pcLoad = Counter(1<<30,decode.pcLoad.valid).value.keep()
    val execute1_pcLoad = Counter(1<<30,execute1.pcLoad.valid).value.keep()

    val decode_halt = Counter(1<<30,decode.halt && decode.inInst.valid).value.keep()
    val execute0_halt = Counter(1<<30,execute0.halt && execute0.inInst.valid).value.keep()
    val execute1_halt = Counter(1<<30,execute1.halt && execute1.inInst.valid).value.keep()
    val writeBack_halt = Counter(1<<30,writeBack.halt && writeBack.inInst.valid).value.keep()
  }


  decode.outInst.ctrl.allowPruning
  execute0.inInst.ctrl.allowPruning
  execute0.outInst.ctrl.allowPruning
  execute1.inInst.ctrl.allowPruning
  execute1.outInst.ctrl.allowPruning
  writeBack.inInst.ctrl.allowPruning

  decode.ctrl.extensionData.allowPruning


}

object RiscvCore{
  def main(args: Array[String]) {
    SpinalVhdl{
      implicit val p = RiscvCoreConfig(
        pcWidth = 32,
        addrWidth = 32,
        startAddress = 0x000,
        regFileReadyKind = sync,
        branchPrediction = disable,
        bypassExecute0 = false,
        bypassExecute1 = false,
        bypassWriteBack = false,
        bypassWriteBackBuffer = false,
        collapseBubble = false,
        fastFetchCmdPcCalculation = false
      )

//      p.add(new MulExtension)
//      p.add(new DivExtension)
      //p.add(new BarrelShifterFullExtension)
      p.add(new BarrelShifterLightExtension)
      p.add(new NativeInstructionBusExtension)
      p.add(new NativeDataBusExtension)
      (new RiscvCore).setDefinitionName("TopLevel")   //765 4lut   482reg
    }
  }
}




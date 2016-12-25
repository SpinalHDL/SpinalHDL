package spinal.lib.cpu.riscv.impl.extension


import spinal.core._
import spinal.lib.bus.amba3.ahblite.{AhbLite3Config, AhbLite3Master}
import spinal.lib.bus.avalon.{AvalonMM, AvalonMMConfig}
import spinal.lib.cpu.riscv.impl.Utils._
import spinal.lib.cpu.riscv.impl._
import spinal.lib._

class NativeDataBusExtension extends CoreExtension with AvalonProvider with AhbLite3Provider{
  override def getName: String = "NativeInstructionBus"
  var memBus : CoreDataBus = null
  override def applyIt(core: RiscvCore): Area = new Area{
    memBus = master(CoreDataBus()(core.c)).setName("io_d")
    memBus.cmd << core.dCmd
    memBus.rsp >> core.dRsp
  }

  override def getAvalon(): AvalonMM = memBus.toAvalon()
  override def getAvalonConfig(): AvalonMMConfig = CoreDataBus.getAvalonConfig(memBus.p)

  override def getAhbLite3(): AhbLite3Master = memBus.toAhbLite3()
  override def getAhbLite3Config(): AhbLite3Config = CoreDataBus.getAhbLite3Config(memBus.p)

  override def needFlowDRsp = true
}


class CachedDataBusExtension(c : DataCacheConfig,cutCpuCmdReady : Boolean = false) extends CoreExtension with AvalonProvider{
  override def getName: String = "CachedDataBus"
  var memBus : DataCacheMemBus = null
  override def applyIt(core: RiscvCore): Area = new Area{
    val cache = new DataCache()(c)
    val cacheDCmd = cloneOf(cache.io.cpu.cmd)
    val coreDRsp = cloneOf(core.dRsp)
    cache.io.cpu.cmd << (if(cutCpuCmdReady) cacheDCmd.s2mPipe() else cacheDCmd)
    coreDRsp >> core.dRsp

    cacheDCmd.valid := core.dCmd.valid
    cacheDCmd.wr := core.dCmd.wr
    cacheDCmd.address := core.dCmd.address(core.dCmd.address.high downto 2) @@ U"00"
    cacheDCmd.data := core.dCmd.size.mux (
      U(0) -> core.dCmd.data(7 downto 0) ## core.dCmd.data(7 downto 0) ## core.dCmd.data(7 downto 0) ## core.dCmd.data(7 downto 0),
      U(1) -> core.dCmd.data(15 downto 0) ## core.dCmd.data(15 downto 0),
      default -> core.dCmd.data(31 downto 0)
    )
    cacheDCmd.mask := (core.dCmd.size.mux (
      U(0) -> B"0001",
      U(1) -> B"0011",
      default -> B"1111"
    ) << core.dCmd.address(1 downto 0)).resized
    cacheDCmd.bypass := core.dCmd.address.msb
    cacheDCmd.all := !core.execute0.inInst.instruction(lineBit)
    when(!isMyTag(core.execute0.inInst.ctrl)) {
      cacheDCmd.kind := DataCacheCpuCmdKind.MEMORY
    }otherwise{
      cacheDCmd.kind := Mux(core.execute0.inInst.instruction(evictBit),DataCacheCpuCmdKind.EVICT,DataCacheCpuCmdKind.FLUSH)
    }
    core.dCmd.ready := cacheDCmd.ready

    coreDRsp.valid := cache.io.cpu.rsp.valid
    coreDRsp.payload := cache.io.cpu.rsp.data
    switch(core.writeBack.inInst.dCmdAddress(1 downto 0)){
      is(1){coreDRsp.payload(7 downto 0) := cache.io.cpu.rsp.data(15 downto 8)}
      is(2){coreDRsp.payload(15 downto 0) := cache.io.cpu.rsp.data(31 downto 16)}
      is(3){coreDRsp.payload(7 downto 0) := cache.io.cpu.rsp.data(31 downto 24)}
    }

    memBus = master(DataCacheMemBus()(c)).setName("io_d")
    memBus <> cache.io.mem

    val flushHappend = RegInit(False) //TODO if cpu to cache cmd are buffered, it could not work  if two flush
    flushHappend := (flushHappend || cache.io.flushDone) && !core.execute1.inInst.ready
    when(core.execute1.inInst.valid && core.execute1.inInst.ctrl.fencei){
      core.execute1.halt := !flushHappend
    }
  }
  val evictBit = 26
  val lineBit = 25


  override def needTag: Boolean = true
  override def instructionCtrlExtension(instruction: Bits, ctrl: InstructionCtrl): Unit = {
    when(instruction === M"00001--------------------0001011" || ctrl.fencei){
      applyTag(ctrl)
      ctrl.instVal := True
      ctrl.op1 := OP1.RS
      ctrl.alu := ALU.COPY
      ctrl.men := True
      ctrl.useSrc1 := True
      ctrl.m := M.XWR
    }
  }

  override def getAvalon(): AvalonMM = memBus.toAvalon()
  override def getAvalonConfig(): AvalonMMConfig = c.getAvalonConfig()
}
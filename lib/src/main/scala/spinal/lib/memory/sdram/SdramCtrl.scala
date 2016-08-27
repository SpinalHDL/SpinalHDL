package spinal.lib.memory.sdram

import spinal.core._
import spinal.lib._

import scala.math.BigDecimal.RoundingMode


case class SdramCtrlCmd(c : SdramLayout) extends Bundle{
  val address = UInt(c.totalAddressWidth bits)
  val write = Bool
  val data = Bits(c.dataWidth bits)
  val mask = Bits(c.symbolCount bits)
}

case class SdramCtrlRsp(c : SdramLayout) extends Bundle{
  val data = Bits(c.dataWidth bits)
}

object SdramCtrlFrontendState extends SpinalEnum{
  val BOOT_PRECHARGE,BOOT_REFRESH,BOOT_MODE,RUN = newElement()
}

object SdramCtrlBackendTask extends SpinalEnum{
  val MODE,PRECHARGE_ALL,PRECHARGE_SINGLE,REFRESH,ACTIVE,READ,WRITE = newElement()
}
case class SdramCtrlBackendCmd(c : SdramLayout) extends Bundle{
  val task = SdramCtrlBackendTask()
  val bank = UInt(c.bankWidth bits)
  val rowColumn =  UInt(Math.max(c.columnWidth,c.rowWidth) bits)
  val data = Bits(c.dataWidth bits)
  val mask = Bits(c.symbolCount bits)
}

case class SdramCtrlBank(c : SdramLayout) extends Bundle{
  val active = Bool
  val row = UInt(c.rowWidth bits)
}


case class SdramCtrl(c : SdramLayout,t : SdramTimings,CAS : Int) extends Component{
  import SdramCtrlBackendTask._
  import SdramCtrlFrontendState._

  val io = new Bundle{
    val sdram = master(SdramInterface(c))

    val cmd = slave Stream(SdramCtrlCmd(c))
    val rsp = master Stream(SdramCtrlRsp(c))
  }

  val clkFrequancy = ClockDomain.current.frequency.getValue
  def timeToCycles(time : BigDecimal): BigInt = (clkFrequancy * time).setScale(0, RoundingMode.UP).toBigInt()

  val refresh = new Area{
    val counter = CounterFreeRun(timeToCycles(t.tREF/(1 << c.rowWidth)))
    val pending = RegInit(False) setWhen(counter.willOverflow)
  }

  
  val powerup = new Area {
    val counter = Reg(UInt(log2Up(timeToCycles(t.tPOW)) bits)) init (0)
    val done = RegInit(False)
    when(!done) {
      counter := counter + 1
      when(counter === U(counter.range -> true)) {
        done := True
      }
    }
  }


  val frontend = new Area{
    val banks = Reg(Vec(SdramCtrlBank(c),c.bankCount))
    banks.foreach(_.active init(False))

    val address = new Bundle{
      val column = UInt(c.columnWidth bits)
      val bank   = UInt(c.bankWidth bits)
      val row    = UInt(c.rowWidth bits)
    }
    address.assignFromBits(io.cmd.address.asBits)

    val rsp = Stream(SdramCtrlBackendCmd(c))
    rsp.valid := False
    rsp.task := REFRESH
    rsp.bank := address.bank
    rsp.rowColumn := address.row.resized
    rsp.data := io.cmd.data
    rsp.mask := io.cmd.mask

    io.cmd.ready := False


    val state = RegInit(BOOT_PRECHARGE)
    val bootRefreshCounter = Counter(t.bootRefreshCount)
    switch(state) {
      is(BOOT_PRECHARGE) {
        rsp.task := PRECHARGE_ALL
        when(powerup.done) {
          rsp.valid := True
          when(rsp.ready) {
            state := BOOT_REFRESH
          }
        }
      }
      is(BOOT_REFRESH) {
        rsp.valid := True
        rsp.task := REFRESH
        when(rsp.ready) {
          bootRefreshCounter.increment()
          when(bootRefreshCounter.willOverflowIfInc) {
            state := BOOT_MODE
          }
        }
      }
      is(BOOT_MODE) {
        rsp.valid := True
        rsp.task := MODE
        when(rsp.ready) {
          state := RUN
        }
      }
      default { //RUN
        when(refresh.pending){
          rsp.valid := True
          when(banks.map(_.active).reduce(_ || _)){
            rsp.task := PRECHARGE_ALL
            when(rsp.ready){
              banks.foreach(_.active := False)
            }
          } otherwise {
            rsp.task := REFRESH
            when(rsp.ready){
              refresh.pending := False
            }
          }
        }.elsewhen(io.cmd.valid){
          rsp.valid := True
          val bank = banks(address.bank)
          when(bank.active && bank.row =/= address.row){
            rsp.task := PRECHARGE_SINGLE
            when(rsp.ready){
              banks(address.bank).active := False
            }
          }.elsewhen(!banks(address.bank).active){
            rsp.task := ACTIVE
            val bank = banks(address.bank)
            bank.row := address.row
            when(rsp.ready){
              bank.active := True
            }
          } otherwise {
            io.cmd.ready := rsp.ready
            rsp.task := io.cmd.write ? WRITE | READ
            rsp.rowColumn := address.column.resized
          }
        }
      }
    }
  }
  
  val bubbleInserter = new Area{
    val cmd = frontend.rsp.m2sPipe()
    val rsp = cloneOf(cmd)
    val insertBubble = False
    rsp << cmd.haltWhen(insertBubble) //From this point, bubble should not be collapsed because of sdram timings rules

    def cycleCounter(cycleMax : BigInt,assignCheck : Boolean = false) = new Area {
      val counter = Reg(UInt(log2Up(cycleMax) bits)) init(0)
      val busy = counter =/= 0
      when(busy && rsp.ready){
        counter := counter - 1
      }
      def setCycles(cycles : BigInt) = {
        if (!assignCheck)
          counter := cycles - 1
        else
          when(cycles - 1 >= counter) {
            counter := cycles - 1
          }
      }
      def setTime(time : BigDecimal) = setCycles(timeToCycles(time).max(1))
    }
    def timeCounter(timeMax : BigDecimal,assignCheck : Boolean = false) = cycleCounter(timeToCycles(timeMax),assignCheck)


    val timings = new Area{
      val read   = timeCounter(t.tRCD)
      val write  = cycleCounter(timeToCycles(t.tRCD).max(CAS + 1))

      val banks = (0 until c.bankCount).map(i =>  new Area{
        val precharge = timeCounter(t.tRC,true)
        val active    = timeCounter(t.tRC.max(t.tMRD).max(t.tRFC),true)
      })
    }

    when(cmd.valid){
      switch(cmd.task){
        is(MODE){
          insertBubble := timings.banks(0).active.busy
          when(cmd.ready) {
            timings.banks.foreach(_.active.setTime(t.tMRD))
          }
        }
        is(PRECHARGE_ALL){
          insertBubble := timings.banks.map(_.precharge.busy).orR
          when(cmd.ready) {
            timings.banks(0).active.setTime(t.tRP) //Only banks 0, because next instruction will be a refresh
          }
        }
        is(PRECHARGE_SINGLE){
          insertBubble := timings.banks.map(_.precharge.busy).read(cmd.bank)
          when(cmd.ready) {
            timings.banks.apply(cmd.bank)(_.active.setTime(t.tRP))
          }
        }
        is(REFRESH){
          insertBubble := timings.banks.map(_.active.busy).orR
          when(cmd.ready) {
            timings.banks.foreach(_.active.setTime(t.tRFC))
          }
        }
        is(ACTIVE){
          insertBubble := timings.banks.map(_.active.busy).read(cmd.bank)
          when(cmd.ready) {
            timings.write.setTime(t.tRCD)
            timings.read.setTime(t.tRCD)
            timings.banks.apply(cmd.bank)(_.precharge.setTime(t.tRAS))
            timings.banks.apply(cmd.bank)(_.active.setTime(t.tRC))
          }
        }
        is(READ){
          insertBubble := timings.read.busy
          when(cmd.ready){
            timings.write.setCycles(CAS + 1)
          }
        }
        is(WRITE){
          insertBubble := timings.write.busy
          when(cmd.ready) {
            timings.banks.apply(cmd.bank)(_.precharge.setCycles(t.cWR + timeToCycles(t.tWR)))
          }
        }
      }
    }
  }
  
  val chip = new Area{
    val cmd = cloneOf(bubbleInserter.rsp)
    cmd << bubbleInserter.rsp

    val sdram = Reg(io.sdram)
    (io.sdram.flatten,  sdram.flatten).zipped.foreach((ext,int) => {
      if(ext.isOutput) ext := int
    })
    sdram.CKE.init(False)

    val remoteCke = RegNext(sdram.CKE) init(True)
    val readHistory = History(
      that       = cmd.valid && cmd.task === READ,
      range      = 0 to CAS + 2,
      when       = remoteCke,
      init       = False
    )
    sdram.CKE    := !(readHistory.orR && !io.rsp.ready)

    when(remoteCke){
      sdram.DQ.read := io.sdram.DQ.read
      sdram.CSn  := False
      sdram.RASn := True
      sdram.CASn := True
      sdram.WEn  := True
      sdram.DQ.write := cmd.data
      sdram.DQ.writeEnable := False

      when(cmd.valid) {
        switch(cmd.task) {
          is(PRECHARGE_ALL) {
            sdram.ADDR(10) := True
            sdram.CSn := False
            sdram.RASn := False
            sdram.CASn := True
            sdram.WEn := False
            sdram.DQM := (sdram.DQM.range -> true)
          }
          is(REFRESH) {
            sdram.CSn := False
            sdram.RASn := False
            sdram.CASn := False
            sdram.WEn := True
            sdram.DQM := (sdram.DQM.range -> true)
          }
          is(MODE) {
            sdram.ADDR := 0
            sdram.ADDR(2 downto 0) := 0
            sdram.ADDR(3) := False
            sdram.ADDR(6 downto 4) := CAS
            sdram.ADDR(8 downto 7) := 0
            sdram.ADDR(9) := False
            sdram.BA := 0
            sdram.CSn := False
            sdram.RASn := False
            sdram.CASn := False
            sdram.WEn := False
            sdram.DQM := (sdram.DQM.range -> true)
          }
          is(ACTIVE) {
            sdram.ADDR := cmd.rowColumn.asBits
            sdram.BA := cmd.bank.asBits
            sdram.CSn := False
            sdram.RASn := False
            sdram.CASn := True
            sdram.WEn := True
            sdram.DQM := (sdram.DQM.range -> true)
          }
          is(WRITE) {
            sdram.ADDR := cmd.rowColumn.asBits
            sdram.ADDR(10) := False
            sdram.DQ.writeEnable := True
            sdram.DQ.write := cmd.data
            sdram.DQM := ~cmd.mask
            sdram.BA := cmd.bank.asBits
            sdram.CSn := False
            sdram.RASn := True
            sdram.CASn := False
            sdram.WEn := False
          }
          is(READ) {
            sdram.DQM := 0
            sdram.ADDR := cmd.rowColumn.asBits
            sdram.ADDR(10) := False
            sdram.BA := cmd.bank.asBits
            sdram.CSn := False
            sdram.RASn := True
            sdram.CASn := False
            sdram.WEn := True
          }
          is(PRECHARGE_SINGLE) {
            sdram.BA := cmd.bank.asBits
            sdram.ADDR(10) := False
            sdram.CSn := False
            sdram.RASn := False
            sdram.CASn := True
            sdram.WEn := False
            sdram.DQM := (sdram.DQM.range -> true)
          }
        }
      }
    }

    val backupIn = cloneOf(io.rsp)
    backupIn.valid := readHistory.last & remoteCke
    backupIn.data := sdram.DQ.read

    io.rsp << backupIn.s2mPipe(2)

    cmd.ready := remoteCke
  }
}


object SdramCtrlMain{
  def main(args: Array[String]) {
    val device = SdramDevices.IS42x320D
    SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)).generateVhdl(SdramCtrl(device.config,device.timingGrade7,3))
  }
}
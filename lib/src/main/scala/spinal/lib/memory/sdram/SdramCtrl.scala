package spinal.lib.memory.sdram

import spinal.core._
import spinal.lib._

import scala.math.BigDecimal.RoundingMode


case class SdramCtrlCmd(c : SdramConfig) extends Bundle{
  val address = UInt(c.totalAddressWidth bits)
  val write = Bool
  val data = Bits(c.dataWidth bits)
  val mask = Bits(c.symbolCount bits)
}

case class SdramCtrlRsp(c : SdramConfig) extends Bundle{
  val data = Bits(c.dataWidth bits)
}

object SdramCtrlFrontendState extends SpinalEnum{
  val BOOT_PRECHARGE,BOOT_REFRESH,BOOT_MODE,RUN = newElement()
}

object SdramCtrlBackendTask extends SpinalEnum{
  val MODE,PRECHARGE_ALL,PRECHARGE_SINGLE,REFRESH,ACTIVE,READ,WRITE = newElement()
}
case class SdramCtrlBackendCmd(c : SdramConfig) extends Bundle{
  val task = SdramCtrlBackendTask()
  val bank = UInt(c.bankCount bits)
  val rowColumn =  UInt(Math.max(c.columnWidth,c.rowWidth) bits)
  val data = Bits(c.dataWidth bits)
  val mask = Bits(c.symbolCount bits)
}

case class SdramCtrlBank(c : SdramConfig) extends Bundle{
  val active = Bool
  val row = UInt(c.rowWidth bits)
}


case class SdramCtrl(c : SdramConfig,t : SdramTimings) extends Component{
  import SdramCtrlBackendTask._
  import SdramCtrlFrontendState._

  val io = new Bundle{
    val sdram = master(SdramInterface(c))

    val cmd = Stream(SdramCtrlCmd(c))
    val rsp = Stream(SdramCtrlRsp(c))
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
      val bank   = UInt(log2Up(c.bankCount) bits)
      val row    = UInt(c.rowWidth bits)
    }
    address.assignFromBits(io.cmd.address.asBits)

    val rsp = Stream(SdramCtrlBackendCmd(c))
    rsp.valid := False
    rsp.task.assignDontCare()
    rsp.bank := address.bank
    rsp.rowColumn := address.row.resized
    rsp.data := io.cmd.data
    rsp.mask := io.cmd.mask


    val state = RegInit(BOOT_PRECHARGE)
    val bootRefreshCounter = Counter(t.bootRefreshCount)
    switch(state) {
      is(BOOT_PRECHARGE) {
        when(powerup.done) {
          rsp.task := PRECHARGE_ALL
          when(rsp.ready) {
            state := BOOT_REFRESH
          }
        }
      }
      is(BOOT_REFRESH) {
        rsp.task := REFRESH
        when(rsp.ready) {
          bootRefreshCounter.increment()
          when(bootRefreshCounter.willOverflowIfInc) {
            state := BOOT_MODE
          }
        }
      }
      is(BOOT_MODE) {
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
          val bank = banks(address.bank)
          when(bank.active && bank.row =/= address.row){
            rsp.task := PRECHARGE_SINGLE
            when(rsp.ready){
              banks(address.bank).active := False
            }
          }.elsewhen(!banks(address.bank).active){
            rsp.task := ACTIVE
            when(rsp.ready){
              banks(address.bank).active := True
            }
          } otherwise {
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


    def cycleCounter(cycleMax : BigInt) = new Area {
      val counter = Reg(UInt(log2Up(cycleMax) bits)) init(0)
      val busy = counter =/= 0
      when(busy){
        counter := counter - 1
      }
      def setCycles(cycles : Int) = counter := cycles
      def setTime(time : BigDecimal) = counter := (timeToCycles(time)-1).max(0)
    }
    def timeCounter(timeMax : BigDecimal) = cycleCounter(timeToCycles(timeMax))


    val timings = new Area{
      val read   = timeCounter(t.tRCD)
      val write  = cycleCounter(timeToCycles(t.tRCD).max(t.cCAS))

      val banks = (0 until c.bankCount).map(i =>  new Area{
        val precharge = timeCounter(t.tRC)
        val active    = timeCounter(t.tRC.max(t.tMRD))
      })
    }

    val insertBubble = False
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
            timings.banks.foreach(_.active.setTime(t.tRC))
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
            timings.write.setCycles(t.cCAS)
          }
        }
        is(WRITE){
          insertBubble := timings.write.busy
        }
      }
    }


    rsp << cmd.haltWhen(insertBubble) //From this point, bubble should not be collapsed because of sdram timings rules
  }
  
  val chip = new Area{
    
  }
}

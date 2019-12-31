package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._

case class TimingEnforcer(cpa : CoreParameterAggregate) extends Component{
  import cpa._

  val io = new Bundle {
    val config = in(CoreConfig(cpa))
    val input = slave(Stream(Fragment(CoreTask(cpa))))
    val output = master(Flow(Fragment(CoreTask(cpa))))
  }


  def Timing(loadValid : Bool, loadValue : UInt) = new Area{
    val value = Reg(UInt(cp.timingWidth bits)) init(0)
    val busy = value =/= 0
    value := value - busy.asUInt
    when(loadValid) { value := loadValue }
  }


  //Request to load timings counters
  val trigger = new Area{
    val WR,RAS,RP,RCD,WTR,CCD,RFC,RTP,RRD = False
    val FAW = generation.FAW generate False
  }

  //Banks timing counters
  val timing = new Area {
    val FAW = generation.FAW generate new Area{
      val ptr = RegInit(U"00")
      val slots = (0 to 3).map(i => Timing(ptr === i && trigger.FAW, io.config.FAW))
      val busy = slots.map(_.busy).read(ptr)
      ptr := ptr + U(trigger.FAW)
    }

    val banks = for (bankId <- 0 until pl.sdram.bankCount) yield new Area {
      val hit = io.input.address.bank === bankId
      val WR  = Timing(hit && trigger.WR, io.config.WR)
      val RAS = Timing(hit && trigger.RAS, io.config.RAS)
      val RP  = Timing(hit && trigger.RP, io.config.RP)
      val RCD = Timing(hit && trigger.RCD, io.config.RCD)
      val RTP = Timing(hit && trigger.RTP, io.config.RTP)
    }

    val WTR = Timing(trigger.WTR, io.config.WTR)
    val CCD = Timing(trigger.CCD, pl.sdram.generation.CCD/pl.dataRate-1)
    val RFC = Timing(trigger.RFC, io.config.RFC)
    val RRD = Timing(trigger.RRD, io.config.RRD)
    val WR = banks.map(_.WR.busy).read(io.input.address.bank)
    val RAS = banks.map(_.RAS.busy).read(io.input.address.bank)
    val RP = banks.map(_.RP.busy).read(io.input.address.bank)
    val RCD = banks.map(_.RCD.busy).read(io.input.address.bank)
    val RTP = banks.map(_.RTP.busy).read(io.input.address.bank)
  }

  val timingIssue = False
  timingIssue.setWhen(timing.RFC.busy)
  ??? //TODO
//  switch(io.input.kind) {
//    is(FrontendCmdOutputKind.READ) {
//      timingIssue.setWhen(timing.RCD || timing.CCD.busy || timing.WTR.busy)
//    }
//    is(FrontendCmdOutputKind.WRITE) {
//      timingIssue.setWhen(timing.RCD || timing.CCD.busy || timing.RTP)
//    }
//    is(FrontendCmdOutputKind.ACTIVE) {
//      timingIssue.setWhen(timing.RP || timing.RRD.busy)
//      if(generation.FAW) timingIssue.setWhen(timing.FAW.busy)
//    }
//    is(FrontendCmdOutputKind.PRECHARGE) {
//      timingIssue.setWhen(timing.WR || timing.RAS)
//    }
//    is(FrontendCmdOutputKind.REFRESH) {
//      timingIssue.setWhen(timing.RP)
//    }
//  }

//  when(io.input.fire){
//    switch(io.input.kind) {
//      is(FrontendCmdOutputKind.READ) {
//        trigger.CCD := True
//        trigger.RTP := True
//      }
//      is(FrontendCmdOutputKind.WRITE) {
//        trigger.CCD := True
//        trigger.WTR := True
//        trigger.WR := True
//      }
//      is(FrontendCmdOutputKind.ACTIVE) {
//        trigger.RAS := True
//        trigger.RCD := True
//        trigger.RRD := True
//        if(generation.FAW) trigger.FAW := True
//      }
//      is(FrontendCmdOutputKind.PRECHARGE) {
//        trigger.RP := True
//      }
//      is(FrontendCmdOutputKind.REFRESH) {
//        trigger.RFC := True
//      }
//    }
//  }

  io.output << io.input.haltWhen(timingIssue).toFlow
}

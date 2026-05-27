package spinal.lib.cpu.riscv.debug

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.{DebugId, M2sAgent, M2sParameters, M2sSource, M2sTransfers, Opcode, SizeRange, fabric}
import spinal.lib.cpu.riscv.RiscvHart
import spinal.lib.cpu.riscv.debug._
import spinal.lib.{OHMux, slave, traversableOnceBoolPimped}

import scala.collection.mutable.ArrayBuffer

class DebugModuleFiber() extends Area{
  var harts = ArrayBuffer[RiscvHart]()
  def bindHart(cpu: RiscvHart) = {
    harts += cpu
  }

  def enableSysBus(): Unit = {
    dmp.withSysBus = true;
  }

  val ndmreset = Bool()
  val p = DebugTransportModuleParameter(
    addressWidth = 7,
    version = 1,
    idle = 7
  )

  val dmp = DebugModuleParameter(
    version = 0,
    harts = 0,
    progBufSize = 2,
    datacount = 0,
    hartsConfig = Nil,
    withSysBus = false
  )

  val cmCd = ClockDomain.current
  val debugBuses = ArrayBuffer[DebugBus]()
  val thread = Fiber build new Area{
    dmp.version = p.version + 1;
    dmp.harts = harts.size;
    dmp.datacount = harts.map(_.getXlen()).max/32;
    dmp.hartsConfig = List(DebugModuleCpuConfig(
      xlen = harts.map(_.getXlen()).max,
      flen = harts.map(_.getFlen()).max,
      withFpuRegAccess = false
    ))

    val logic = DebugModule(dmp)

    ndmreset := logic.io.ndmreset

    for(i <- harts.indices){
      val from = logic.io.harts(i)
      val to = harts(i).getDebugBus
//      from <> to
//      to.dmToHart.removeAssignments() <-< from.dmToHart

      from.halted := RegNext(to.halted) init(False)
      from.running := RegNext(to.running) init(False)
      from.unavailable := RegNext(to.unavailable) init(True)
      from.haveReset := RegNext(to.haveReset) init(False)
      from.exception := RegNext(to.exception) init(False)
      from.commit := RegNext(to.commit) init(False)
      from.ebreak := RegNext(to.ebreak) init(False)
      from.redo := RegNext(to.redo) init (False)
      from.regSuccess := RegNext(to.regSuccess) init (False)
      to.haltReq := RegNext(from.haltReq) init (False)
      to.ackReset := RegNext(from.ackReset) init (False)
      to.hartToDm >-> from.hartToDm
      to.dmToHart <-< from.dmToHart
      to.resume.cmd <-< from.resume.cmd
      to.resume.rsp >-> from.resume.rsp
    }

    //Assumes only one interface is used on the field
    logic.io.ctrl.cmd.valid := debugBuses.map(_.cmd.valid).orR
    logic.io.ctrl.cmd.payload := OHMux.or(debugBuses.map(_.cmd.valid), debugBuses.map(_.cmd.payload), true)
    for(db <- debugBuses) {
      db.cmd.ready := logic.io.ctrl.cmd.ready
      db.rsp << logic.io.ctrl.rsp
    }
  }

  def withJtagTap(jtagFrequency : ClockDomain.ClockFrequency = UnknownFrequency()) = {
    val db = DebugBus(p.addressWidth); debugBuses += db
    Fiber build new Area {
      val logic = DebugTransportModuleJtagTap(
        p,
        debugCd = cmCd,
        jtagFrequency = jtagFrequency
      )
      db <> logic.io.bus
      val jtag = logic.io.jtag.toIo
    }
  }

  def withJtagInstruction() = {
    val db = DebugBus(p.addressWidth); debugBuses += db
    Fiber build new Area {
      val logic = DebugTransportModuleTunneled(
        p = p,
        jtagCd = ClockDomain.current,
        debugCd = cmCd
      )
      db <> logic.io.bus
      val instruction = logic.io.instruction.toIo
    }
  }

  def makeSysbusTilelink() = new Area{
    val dmNode = spinal.lib.bus.tilelink.fabric.Node.master()

    val filter = new fabric.TransferFilter()
    filter.up << dmNode

    val logic = Fiber build new Area {
      dmNode.m2s.parameters.load(M2sParameters(
        addressWidth = 32,
        dataWidth = 32, //TODO
        masters = List(M2sAgent(
          name = null,
          mapping = List(M2sSource(
            id = SizeMapping(0,1),
            emits = M2sTransfers.allGetPut(SizeRange(1, 4))
          ))
        ))
      ))
      dmNode.m2s.setProposedFromParameters() // Here, we just ignore the negotiation phase
      dmNode.s2m.supported.load(dmNode.s2m.proposed)

      assert(harts.map(_.getXlen()).max == 32)

      val sb = thread.logic.io.sysBus
      val a = dmNode.bus.a
      val d = dmNode.bus.d

      a.arbitrationFrom(sb.cmd)
      a.opcode  := sb.cmd.wr.mux(Opcode.A.PUT_FULL_DATA, Opcode.A.GET)
      a.param   := 0
      a.source  := 0
      a.address :=  sb.cmd.address
      a.size := 2 //sb.cmd.size.muxListDc(())
      a.mask.setAll() //((U(1) << sb.cmd.size) -1) << sb.cmd.address.take(log2Up(node.bus.p.sizeBytes))
      a.data    := sb.cmd.data
      a.corrupt := False
      a.debugId := DebugId.withPostfix(a.source)

      d.ready := True
      sb.rsp.valid := d.valid
      sb.rsp.data := d.data
      sb.rsp.error := False
    }
  }
}


class DebugModuleSocFiber(withJtagTap : Boolean, withJtagInstruction : Boolean) extends Area{
  val dm = new DebugModuleFiber()
  val tck = withJtagInstruction generate in(Bool())
  val tap = withJtagTap generate dm.withJtagTap()
  val instruction = withJtagInstruction generate ClockDomain(tck)(dm.withJtagInstruction())

  def bindHart(cpu: RiscvHart) = {
    dm.bindHart(cpu)
  }
}

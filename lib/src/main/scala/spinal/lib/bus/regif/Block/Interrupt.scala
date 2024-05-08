package spinal.lib.bus.regif.Block

import spinal.core.{Bool, SpinalInfo}
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import spinal.lib.bus.regif.{AccessType, Apb3BusInterface, SymbolName}

class Interrupt {
//  val bus = Apb3(Apb3Config(32, 32))
//  val busif = Apb3BusInterface(bus, sizeMap = )

//  def geneintr4Regs(time: Int) = {
//    val RAW = busif.newRegAt(reg.offset.value, reg.doc + "\n[EINTR4 Couple-RAW]" + "\nInterrupt Raw status Register\n set when event \n clear raw when write 1")(SymbolName(s"${reg.name}_INT_RAW"))
//    val FORCE = busif.newReg(reg.doc + "\n[EINTR4 Couple-FORCE]" + "\nInterrupt Force  Register\n for SW debug use \n write 1 set raw")(SymbolName(s"${reg.name}_INT_FORCE"))
//    val MASK = busif.newReg(reg.doc + "\n[EINTR4 Couple-MASK]" + "\nInterrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${reg.name}_INT_MASK"))
//    val STATUS = busif.newReg(reg.doc + "\n[EINTR4 Couple-STATUS]" + "\nInterrupt status Register\n status = raw && (!mask)")(SymbolName(s"${reg.name}_INT_STATUS"))
//    SpinalInfo(s"Generate INTR-Reg: ${reg.name} <0x${reg.offset.value.hexString(16)}> [${reg.doc}]")
//    val pre = if (time == 1) "" else s"_${time - 1}"
//    val int_status = reg.fields.filter(_.access != "NA").map { f =>
//      val nm = f.name + pre
//      val event = Bool().setName(f.name).asInput()
//      val raw = RAW.field(Bool(), AccessType.W1C, resetValue = 0, doc = f.doc + s"raw, default 0")(SymbolName(s"${nm}_raw"))
//      FORCE.parasiteField(raw, AccessType.W1S, resetValue = 0, doc = f.doc + s"force, write 1 set, debug use")
//      val mask = MASK.field(Bool(), AccessType.RW, resetValue = f.reset.value, doc = f.doc + s"mask, default 1, int off")(SymbolName(s"${nm}_mask" ))
//      val status = STATUS.field(Bool(), AccessType.RO, resetValue = 0, doc = f.doc + s"stauts default 0")(SymbolName(s"${nm}_status"))
//      raw.setWhen(event)
//      status := raw && (!mask)
//      status
//    }
//    val intmerged = int_status.reduceLeft(_ || _)
//    intmerged.setName(s"${reg.name.toLowerCase()}", weak = true)
//    if (intsBatchOut) {
//      int_status.asBits().setName(s"${reg.name.toLowerCase()}_status" + pre)
//    }
//    if (c.IntrRegOut) {
//      intmerged.setName(s"${reg.name.toLowerCase()}" + "_wire" + pre)
//      val ret = spinal.core.Reg(Bool()) init False
//      ret.asOutput().setName(s"${reg.name.toLowerCase()}")
//      ret := intmerged
//    } else {
//      intmerged.setName(s"${reg.name.toLowerCase()}").asOutput()
//    }
//    int_status
//  }
//
//  def geneintr5Regs(time: Int) = {
//    val RAW = busif.newRegAt(reg.offset.value, reg.doc + "\n[EINTR5 Couple-RAW]" + "\nInterrupt Raw status Register\n set when event \n clear raw when write 1")(SymbolName(s"${reg.name}_INT_RAW"))
//    val FORCE = busif.newReg(reg.doc + "\n[EINTR5 Couple-FORCE]" + "\nInterrupt Force  Register\n for SW debug use \n write 1 set raw")(SymbolName(s"${reg.name}_INT_FORCE"))
//    val MASK_SET = busif.newReg(reg.doc + "\n[EINTR5 Couple-MASK_SET]" + "\nInterrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${reg.name}_INT_MASK_SET"))
//    val MASK_CLR = busif.newReg(reg.doc + "\n[EINTR5 Couple-MASK_CLR]" + "\nInterrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${reg.name}_INT_MASK_CLR"))
//    val STATUS = busif.newReg(reg.doc + "\n[EINTR5 Couple-STATUS]" + "\nInterrupt status Register\n status = raw && (!mask)")(SymbolName(s"${reg.name}_INT_STATUS"))
//    SpinalInfo(s"Generate INTR-Reg: ${reg.name} <0x${reg.offset.value.hexString(16)}> [${reg.doc}]")
//    val pre = if (time == 1) "" else s"_${time - 1}"
//    val int_status = reg.fields.filter(_.access != "NA").map { f =>
//      val nm = f.name+pre
//      val event = Bool().setName(f.name).asInput()
//      val raw = RAW.field(Bool(), AccessType.W1C, resetValue = 0, doc = f.doc + s"raw, default 0")(SymbolName(s"${nm}_raw"))
//      FORCE.parasiteField(raw, AccessType.W1S, resetValue = 0, doc = f.doc + s"force, write 1 set, debug use")
//      val mask = MASK_SET.field(Bool(), AccessType.W1S, resetValue = f.reset.value, doc = f.doc + s"mask,write 1 set, default 1, int off")(SymbolName(s"${nm}_mask"))
//      MASK_CLR.parasiteField(mask, AccessType.W1C, resetValue = f.reset.value, doc = f.doc + s"mask, write 1 clr")
//      val status = STATUS.field(Bool(), AccessType.RO, resetValue = 0, doc = f.doc + s"stauts default 0")(SymbolName(s"${nm}_status" ))
//      raw.setWhen(event)
//      status := raw && (!mask)
//      status
//    }
//    val intmerged = int_status.reduceLeft(_ || _)
//    intmerged.setName(s"${reg.name.toLowerCase()}", weak = true)
//    if (intsBatchOut) {
//      int_status.asBits().setName(s"${reg.name.toLowerCase()}_status")
//    }
//    if (c.IntrRegOut) {
//      intmerged.setName(s"${reg.name.toLowerCase()}" + "_wire")
//      val ret = spinal.core.Reg(Bool()) init False
//      ret.asOutput().setName(s"${reg.name.toLowerCase()}")
//      ret := intmerged
//    } else {
//      intmerged.setName(s"${reg.name.toLowerCase()}").asOutput()
//    }
//    int_status
//  }
//  def gen0msintrRegs(time:Int)={
//    val STATUS = busif.newRegAt(reg.offset.value,reg.doc+"Interrupt status Register\n status = int_level ")(SymbolName(s"${reg.name}_INT_STATUS"))
//    SpinalInfo(s"Generate INTR-Reg: ${reg.name} <0x${reg.offset.value.hexString(16)}> [${reg.doc}]")
//    val pre = if (time == 1) "" else s"_${time - 1}"
//    val status_intr_all = reg.fields.filter(_.access != "NA").map { f =>
//      val nm = f.name+pre
//      val status_in=Bool().setName(f.name).asInput()
//      val status = STATUS.field(Bool(), AccessType.RO, resetValue = 0, doc = f.doc+s"stauts")(SymbolName(s"${nm}_status"))
//      status := status_in
//      status
//    }
//    val status_intr=status_intr_all.reduceLeft(_ || _)
//    if (c.IntrRegOut) {
//      status_intr.setName(s"${reg.name.toLowerCase()}" + "_wire")
//      val ret = spinal.core.Reg(Bool()) init False
//      ret.asOutput().setName(s"${reg.name.toLowerCase()}")
//      ret := status_intr
//    } else {
//      status_intr.setName(s"${reg.name.toLowerCase()}").asOutput()
//    }
//    status_intr_all
//  }
//
//  def gen1msintr3Regs(time:Int)={
//    val RAW = busif.newRegAt(reg.offset.value, reg.doc + "\n[1MSINTR3 Couple-RAW]" + "\n STATUS Interrupt Raw status Register\n RO")(SymbolName(s"${reg.name}_INT_RAW"))
//    val MASK = busif.newReg( reg.doc + "\n[1MSINTR3 Couple-MASK]" +"Interrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${reg.name}_INT_MASK"))
//    val STATUS = busif.newReg( reg.doc + "\n[1MSINTR3 Couple-STATUS]"+"Interrupt status Register\n status = raw && (!mask)")(SymbolName(s"${reg.name}_INT_STATUS"))
//    SpinalInfo(s"Generate INTR-Reg: ${reg.name} <0x${reg.offset.value.hexString(16)}> [${reg.doc}]")
//    val pre = if (time == 1) "" else s"_${time - 1}"
//    val status_intr_all = reg.fields.filter(_.access != "NA").map { f =>
//      val nm=f.name+pre
//      val status_in=Bool().setName(f.name).asInput()
//      val raw   = RAW.field(Bool(), AccessType.RO, resetValue = 0, doc = f.doc + s"raw, default 0")(SymbolName(s"${nm}_raw"))
//      val mask  = MASK.field(Bool(), AccessType.RW, resetValue = f.reset.value, doc =f.doc+ s"mask" )(SymbolName(s"${nm}_mask"))
//      val status= STATUS.field(Bool(), AccessType.RO, resetValue = 0, doc = f.doc + s"stauts" )(SymbolName(s"${nm}_status"))
//      raw := status_in
//      status := raw && (!mask)
//      status
//    }
//    val status_intr=status_intr_all.reduceLeft(_ || _)
//    if (intsBatchOut) {
//      status_intr_all.asBits().setName(s"${reg.name.toLowerCase()}_status")
//    }
//    if (c.IntrRegOut) {
//      status_intr.setName(s"${reg.name.toLowerCase()}" + "_wire")
//      val ret = spinal.core.Reg(Bool()) init False
//      ret.asOutput().setName(s"${reg.name.toLowerCase()}")
//      ret := status_intr
//    } else {
//      status_intr.setName(s"${reg.name.toLowerCase()}").asOutput()
//    }
//    status_intr_all
//  }
//  def gen1msintr2Regs(time:Int)={
//    val MASK = busif.newRegAt(reg.offset.value, reg.doc + "\n[1MSINTR2 Couple-MASK]" +"Interrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${reg.name}_INT_MASK"))
//    val STATUS = busif.newReg( reg.doc + "\n[1MSINTR2 Couple-STATUS]"+"Interrupt status Register\n status = raw && (!mask)")(SymbolName(s"${reg.name}_INT_STATUS"))
//    SpinalInfo(s"Generate INTR-Reg: ${reg.name} <0x${reg.offset.value.hexString(16)}> [${reg.doc}]")
//    val pre = if (time == 1) "" else s"_${time - 1}"
//    val status_intr_all = reg.fields.filter(_.access != "NA").map { f =>
//      val nm=f.name+pre
//      val status_in=Bool().setName(f.name).asInput()
//      val mask  = MASK.field(Bool(), AccessType.RW, resetValue = f.reset.value, doc =f.doc+ s"mask" )(SymbolName(s"${nm}_mask"))
//      val status= STATUS.field(Bool(), AccessType.RO, resetValue = 0, doc = f.doc + s"stauts" )(SymbolName(s"${nm}_status"))
//      status := status_in && (!mask)
//      status
//    }
//    val status_intr=status_intr_all.reduceLeft(_ || _)
//    if (intsBatchOut) {
//      status_intr_all.asBits().setName(s"${reg.name.toLowerCase()}_status")
//    }
//    if (c.IntrRegOut) {
//      status_intr.setName(s"${reg.name.toLowerCase()}" + "_wire")
//      val ret = spinal.core.Reg(Bool()) init False
//      ret.asOutput().setName(s"${reg.name.toLowerCase()}")
//      ret := status_intr
//    } else {
//      status_intr.setName(s"${reg.name.toLowerCase()}").asOutput()
//    }
//    status_intr_all
//  }
//  def gen2msintr4Regs(time:Int)={
//    val RAW = busif.newRegAt(reg.offset.value, reg.doc + "\n[2MSINTR4 Couple-RAW]" + "\n STATUS Interrupt Raw status Register\n RO")(SymbolName(s"${reg.name}_INT_RAW"))
//    val MASK_SET = busif.newReg( reg.doc + "\n[2MSINTR4 Couple-MASK_SET]" +"Interrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${reg.name}_INT_MASK_SET"))
//    val MASK_CLR = busif.newReg( reg.doc + "\n[2MSINTR4 Couple-MASK_CLR]" +"Interrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${reg.name}_INT_MASK_CLR"))
//    val STATUS = busif.newReg( reg.doc + "\n[2MSINTR4 Couple-STATUS]"+"Interrupt status Register\n status = raw && (!mask)")(SymbolName(s"${reg.name}_INT_STATUS"))
//    SpinalInfo(s"Generate INTR-Reg: ${reg.name} <0x${reg.offset.value.hexString(16)}> [${reg.doc}]")
//    val pre = if (time == 1) "" else s"_${time - 1}"
//    val status_intr_all = reg.fields.filter(_.access != "NA").map { f =>
//      val nm=f.name+pre
//      val status_in=Bool().setName(f.name).asInput()
//      val raw   = RAW.field(Bool(), AccessType.RO, resetValue = 0, doc = f.doc + s"raw, default 0")(SymbolName(s"${nm}_raw"))
//      val mask  = MASK_SET.field(Bool(), AccessType.W1C, resetValue = f.reset.value, doc =f.doc+ s"mask" )(SymbolName(s"${nm}_mask"))
//      MASK_CLR.parasiteField(mask, AccessType.W1C, resetValue = f.reset.value, doc = f.doc + s"mask, write 1 clr")
//      val status= STATUS.field(Bool(), AccessType.RO, resetValue = 0, doc = f.doc + s"stauts" )(SymbolName(s"${nm}_status"))
//      raw := status_in
//      status := raw && (!mask)
//      status
//    }
//    val status_intr=status_intr_all.reduceLeft(_ || _)
//    if (intsBatchOut) {
//      status_intr_all.asBits().setName(s"${reg.name.toLowerCase()}_status")
//    }
//    if (c.IntrRegOut) {
//      status_intr.setName(s"${reg.name.toLowerCase()}" + "_wire")
//      val ret = spinal.core.Reg(Bool()) init False
//      ret.asOutput().setName(s"${reg.name.toLowerCase()}")
//      ret := status_intr
//    } else {
//      status_intr.setName(s"${reg.name.toLowerCase()}").asOutput()
//    }
//    status_intr_all
//  }
//  def gen2msintr3Regs(time:Int)={
//    val MASK_SET = busif.newRegAt(reg.offset.value ,reg.doc + "\n[2MSINTR3 Couple-MASK_SET]" +"Interrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${reg.name}_INT_MASK_SET"))
//    val MASK_CLR = busif.newReg( reg.doc + "\n[2MSINTR3 Couple-MASK_CLR]" +"Interrupt Mask   Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${reg.name}_INT_MASK_CLR"))
//    val STATUS = busif.newReg( reg.doc + "\n[2MSINTR3 Couple-STATUS]"+"Interrupt status Register\n status = raw && (!mask)")(SymbolName(s"${reg.name}_INT_STATUS"))
//    SpinalInfo(s"Generate INTR-Reg: ${reg.name} <0x${reg.offset.value.hexString(16)}> [${reg.doc}]")
//    val pre = if (time == 1) "" else s"_${time - 1}"
//    val status_intr_all = reg.fields.filter(_.access != "NA").map { f =>
//      val nm=f.name+pre
//      val status_in=Bool().setName(f.name).asInput()
//      val mask  = MASK_SET.field(Bool(), AccessType.W1C, resetValue = f.reset.value, doc =f.doc+ s"mask" )(SymbolName(s"${nm}_mask"))
//      MASK_CLR.parasiteField(mask, AccessType.W1C, resetValue = f.reset.value, doc = f.doc + s"mask, write 1 clr")
//      val status= STATUS.field(Bool(), AccessType.RO, resetValue = 0, doc = f.doc + s"stauts" )(SymbolName(s"${nm}_status"))
//      status := status_in && (!mask)
//      status
//    }
//    val status_intr=status_intr_all.reduceLeft(_ || _)
//    if (intsBatchOut) {
//      status_intr_all.asBits().setName(s"${reg.name.toLowerCase()}_status")
//    }
//    if (c.IntrRegOut) {
//      status_intr.setName(s"${reg.name.toLowerCase()}" + "_wire")
//      val ret = spinal.core.Reg(Bool()) init False
//      ret.asOutput().setName(s"${reg.name.toLowerCase()}")
//      ret := status_intr
//    } else {
//      status_intr.setName(s"${reg.name.toLowerCase()}").asOutput()
//    }
//    status_intr_all
//  }
}

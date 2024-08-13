package spinal.lib.bus

import spinal.core._

import scala.collection.mutable.ListBuffer

package object regif {
  def formatResetValue(value: BigInt, bitCount: Int): String = {
    val hexCount = scala.math.ceil(bitCount / 4.0).toInt
    val unsignedValue = if (value >= 0) value else ((BigInt(1) << bitCount) + value)
    s"${bitCount}'h%${hexCount}s".format(unsignedValue.toString(16)).replace(' ', '0')
  }

  implicit class BusIfIntr(bi: BusIf) {
    @deprecated(message = "", since = "2022-12-31")
    def FactoryInterruptWithMask(regNamePre: String, triggers: Bool*): Bool = {
      triggers.size match {
        case 0 => SpinalError("There have no inputs Trigger signals")
        case x if x > bi.busDataWidth => SpinalError(s"Trigger signal numbers exceed Bus data width ${bi.busDataWidth}")
        case _ =>
      }
      val ENS    = bi.newReg("Interrupt Enable Register")(SymbolName(s"${regNamePre}_ENABLES"))
      val MASKS  = bi.newReg("Interrupt Mask   Register")(SymbolName(s"${regNamePre}_MASK"))
      val STATUS = bi.newReg("Interrupt status Register")(SymbolName(s"${regNamePre}_STATUS"))
      val intWithMask = new ListBuffer[Bool]()
      triggers.foreach(trigger => {
        val en   = ENS.field(1 bits, AccessType.RW, doc= "int enable register")(SymbolName(s"_en"))(0)
        val mask = MASKS.field(1 bits, AccessType.RW, doc= "int mask register")(SymbolName(s"_mask"))(0)
        val stat = STATUS.field(1 bits, AccessType.RC, doc= "int status register")(SymbolName(s"_stat"))(0)
        when(trigger && en) {stat.set()}
        intWithMask +=  mask && stat
      })
      intWithMask.foldLeft(False)(_||_)
    }
    /*
      interrupt with Raw/Force/Mask/Status 4 Register Interface
      **/
    def interruptFactory(regNamePre: String, triggers: Bool*): Bool = interruptFactoryAt(bi.getRegPtr(), regNamePre, triggers:_*)
    def interruptFactoryAt(addrOffset: BigInt, regNamePre: String, triggers: Bool*): Bool = {
      require(triggers.size > 0 && triggers.size <= bi.busDataWidth, s"The maxNum should be ${bi.busDataWidth} else split 2 or more groups")
      val IntrGrp = this.newIntrRFMS4(addrOffset, "")(SymbolName(s"${regNamePre}"))
      triggers.foreach{ event =>
        IntrGrp.field(event, maskRstVal = 0x1, doc = "")(SymbolName(s"${event.getPartialName()}"))
      }
      IntrGrp.intr()(SymbolName(s"${regNamePre}"))
    }
    /*
      interrupt with Raw/Force/Mask_SET/Mask_CLR/Status 5 Register Interface
      It is to solve the atomic problem that may arise from operating the same mask address on multiple processor cores.
      Currently, two independent mask addresses operate on the same mask register to ensure:
      Two processor cores can independently set the state of one bit without affecting other bits
      */
    def interrupt_W1SCmask_FactoryAt(addrOffset: BigInt, regNamePre: String, triggers: Bool*): Bool = {
      require(triggers.size > 0 && triggers.size <= bi.busDataWidth, s"The maxNum should be ${bi.busDataWidth} else split 2 or more groups")
      val IntrGrp = this.newIntrRFMMS5(addrOffset, "")(SymbolName(s"${regNamePre}"))
      triggers.foreach{ event =>
        IntrGrp.field(event, maskRstVal = 0x1, doc = "")(SymbolName(s"${event.getPartialName()}"))
      }
      IntrGrp.intr()(SymbolName(s"${regNamePre}"))
    }
    /*
      interrupt with Raw/Mask/Status 3 Register Interface
      **/
    def interruptFactoryNoForce(regNamePre: String, triggers: Bool*): Bool = interruptFactoryNoForceAt(bi.getRegPtr, regNamePre, triggers:_*)
    def interruptFactoryNoForceAt(addrOffset: BigInt, regNamePre: String, triggers: Bool*): Bool = {
      require(triggers.size > 0 && triggers.size <= bi.busDataWidth, s"The maxNum should be ${bi.busDataWidth} else split 2 or more groups")
      val IntrGrp = this.newIntrRMS3(addrOffset, "")(SymbolName(s"${regNamePre}"))
      triggers.foreach{ event =>
        IntrGrp.field(event, maskRstVal = 0x1, doc = "")(SymbolName(s"${event.getPartialName()}"))
      }
      IntrGrp.intr()(SymbolName(s"${regNamePre}"))
    }
    /*
      interrupt with Mask/Status 2 Register Interface
      always used for sys_level_int merge
      **/
    def interruptLevelFactory(regNamePre: String, levels: Bool*): Bool = interruptLevelFactoryAt(bi.getRegPtr, regNamePre, levels:_*)
    def interruptLevelFactoryAt(addrOffset: BigInt, regNamePre: String, levels: Bool*): Bool = {
      require(levels.size > 0 && levels.size <= bi.busDataWidth, s"The maxNum should be ${bi.busDataWidth} else split 2 or more groups")
      val IntrGrp = this.newIntrMS2(addrOffset, "")(SymbolName(s"${regNamePre}"))
      levels.foreach{ level =>
        IntrGrp.field(level, maskRstVal = 0x1, doc = "")(SymbolName(s"${level.getPartialName()}"))
      }
      IntrGrp.intr()(SymbolName(s"${regNamePre}"))
    }
    /*
     interrupt with Mask_SET/Mask_CLR/Status 3 Register Interface
     always used for sys_level_int merge
     **/
    def interruptLevel_W1SCmask_FactoryAt(addrOffset: BigInt, regNamePre: String, levels: Bool*): Bool = {
      require(levels.size > 0 && levels.size <= bi.busDataWidth, s"The maxNum should be ${bi.busDataWidth} else split 2 or more groups")
      val IntrGrp = this.newIntrMMS3(addrOffset, "")(SymbolName(s"${regNamePre}"))
      levels.foreach{ level =>
        IntrGrp.field(level, maskRstVal = 0x1, doc = "")(SymbolName(s"${level.getPartialName()}"))
      }
      IntrGrp.intr()(SymbolName(s"${regNamePre}"))
    }

    def newRegSCRAt(addr: BigInt, doc: String, sec: Secure)(implicit symbol: SymbolName): RegSCR = new RegSCR(name = symbol.name, offset = addr, doc = doc, bi = bi, sec, grp = bi.newGrpTag(symbol.name))
    def newRegSCRAt(addr: BigInt, doc: String)(implicit symbol: SymbolName): RegSCR = new RegSCR(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))
    def newRegSCR(doc: String)(implicit symbol: SymbolName): RegSCR = new RegSCR(name = symbol.name, offset = bi.getRegPtr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))

    def newRegSCAt(addr: BigInt, doc: String, sec: Secure)(implicit symbol: SymbolName): RegSC = new RegSC(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = sec,  grp = bi.newGrpTag(symbol.name))
    def newRegSCAt(addr: BigInt, doc: String)(implicit symbol: SymbolName): RegSC = new RegSC(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = null,  grp = bi.newGrpTag(symbol.name))
    def newRegSC(doc: String)(implicit symbol: SymbolName): RegSC = new RegSC(name = symbol.name, offset = bi.getRegPtr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))

    def newIntrRFMS4(addr: BigInt, doc: String, sec: Secure)(implicit symbol: SymbolName): IntrRFMS4 = new IntrRFMS4(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = sec, grp = bi.newGrpTag(symbol.name))
    def newIntrRFMS4(addr: BigInt, doc: String)(implicit symbol: SymbolName): IntrRFMS4 = new IntrRFMS4(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))
    def newIntrRFMS4(doc: String)(implicit symbol: SymbolName): IntrRFMS4 = new IntrRFMS4(name = symbol.name, offset = bi.getRegPtr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))

    def newIntrRMS3(addr: BigInt, doc: String, sec: Secure)(implicit symbol: SymbolName): IntrRMS3 = new IntrRMS3(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = sec, grp = bi.newGrpTag(symbol.name))
    def newIntrRMS3(addr: BigInt, doc: String)(implicit symbol: SymbolName): IntrRMS3 = new IntrRMS3(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))
    def newIntrRMS3(doc: String)(implicit symbol: SymbolName): IntrRMS3 = new IntrRMS3(name = symbol.name, offset = bi.getRegPtr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))

    def newIntrRFMMS5(addr: BigInt, doc: String, sec: Secure)(implicit symbol: SymbolName): IntrRFMMS5 = new IntrRFMMS5(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = sec, grp = bi.newGrpTag(symbol.name))
    def newIntrRFMMS5(addr: BigInt, doc: String)(implicit symbol: SymbolName): IntrRFMMS5 = new IntrRFMMS5(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))
    def newIntrRFMMS5(doc: String)(implicit symbol: SymbolName): IntrRFMMS5 = new IntrRFMMS5(name = symbol.name, offset = bi.getRegPtr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))

    def newIntrOMS3(addr: BigInt, doc: String, sec: Secure)(implicit symbol: SymbolName): IntrOMS3 = new IntrOMS3(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = sec, grp = bi.newGrpTag(symbol.name))
    def newIntrOMS3(addr: BigInt, doc: String)(implicit symbol: SymbolName): IntrOMS3 = new IntrOMS3(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))
    def newIntrOMS3(doc: String)(implicit symbol: SymbolName): IntrOMS3 = new IntrOMS3(name = symbol.name, offset = bi.getRegPtr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))

    def newIntrOMMS4(addr: BigInt, doc: String, sec: Secure)(implicit symbol: SymbolName): IntrOMMS4 = new IntrOMMS4(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = sec, grp = bi.newGrpTag(symbol.name))
    def newIntrOMMS4(addr: BigInt, doc: String)(implicit symbol: SymbolName): IntrOMMS4 = new IntrOMMS4(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))
    def newIntrOMMS4(doc: String)(implicit symbol: SymbolName): IntrOMMS4 = new IntrOMMS4(name = symbol.name, offset = bi.getRegPtr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))

    def newIntrMS2(addr: BigInt, doc: String, sec: Secure)(implicit symbol: SymbolName): IntrMS2 = new IntrMS2(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = sec, grp = bi.newGrpTag(symbol.name))
    def newIntrMS2(addr: BigInt, doc: String)(implicit symbol: SymbolName): IntrMS2 = new IntrMS2(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))
    def newIntrMS2(doc: String)(implicit symbol: SymbolName): IntrMS2 = new IntrMS2(name = symbol.name, offset = bi.getRegPtr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))

    def newIntrMMS3(addr: BigInt, doc: String, sec: Secure)(implicit symbol: SymbolName): IntrMMS3 = new IntrMMS3(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = sec, grp = bi.newGrpTag(symbol.name))
    def newIntrMMS3(addr: BigInt, doc: String)(implicit symbol: SymbolName): IntrMMS3 = new IntrMMS3(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))
    def newIntrMMS3(doc: String)(implicit symbol: SymbolName): IntrMMS3 = new IntrMMS3(name = symbol.name, offset = bi.getRegPtr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag(symbol.name))


    def newIntrS1(addr: BigInt, doc: String, sec: Secure)(implicit symbol: SymbolName): IntrS1 = new IntrS1(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = sec, grp = bi.newGrpTag("-"))
    def newIntrS1(addr: BigInt, doc: String)(implicit symbol: SymbolName): IntrS1 = new IntrS1(name = symbol.name, offset = addr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag("-"))
    def newIntrS1(doc: String)(implicit symbol: SymbolName): IntrS1 = new IntrS1(name = symbol.name, offset = bi.getRegPtr, doc = doc, bi = bi, sec = null, grp = bi.newGrpTag("-"))
  }
}

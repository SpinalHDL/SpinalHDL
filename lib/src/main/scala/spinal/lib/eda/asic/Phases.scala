package spinal.lib.eda.asic

import spinal.core._
import spinal.core.internals.{PhaseContext, PhaseNetlist}
import spinal.lib.KeepAttribute

class PhaseAsicSanity() extends PhaseNetlist {
  override def impl(pc: PhaseContext): Unit = {
    pc.walkStatements{s =>
      s match {
        case s : SpinalTagReady => s.removeTags(KeepAttribute.all)
        case _ =>
      }
      s match {
        case bt : BaseType if bt.isReg && bt.clockDomain.config.resetKind == BOOT => PendingError(s"Got a reg with BOOT reset kind $bt")
        case bt : BaseType if bt.isReg && bt.clockDomain.config.resetKind == SYNC => PendingError(s"Got a reg with SYNC reset kind $bt")
        case _ =>
      }
      s match {
        case bt : BaseType if bt.isReg && bt.hasTag(spinal.core.randomBoot) => bt.removeTag(spinal.core.randomBoot)
        case _ =>
      }
    }
  }
}

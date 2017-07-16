package spinal.lib.eda.bench

import spinal.lib.eda.altera.QuartusFlow

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future

trait Target {
  def synthesise(rtl: Rtl, workspace: String): Report
  def getFamilyName() : String
}

object AlteraStdTargets {
  def apply(quartusCycloneIIPath : String = null, quartusCycloneIVPath : String = null, quartusCycloneVPath : String = null): Seq[Target] = {
    val targets = ArrayBuffer[Target]()

    if(quartusCycloneIIPath != null) {
      targets += new Target {
        override def getFamilyName(): String = "Cyclone V"

        override def synthesise(rtl: Rtl, workspace: String): Report = {
          QuartusFlow(
            quartusPath = quartusCycloneVPath,
            workspacePath = workspace,
            toplevelPath = rtl.getRtlPath(),
            family = getFamilyName(),
            device = "5CSEMA5F31C6"
          )
        }
      }
    }

    if(quartusCycloneIIPath != null) {
      targets += new Target {
        override def getFamilyName(): String = "Cyclone IV"

        override def synthesise(rtl: Rtl, workspace: String):  Report = {
          QuartusFlow(
            quartusPath = quartusCycloneIVPath,
            workspacePath = workspace,
            toplevelPath = rtl.getRtlPath(),
            family = getFamilyName(),
            device = "EP4CE30F29C6"
          )
        }
      }
    }
    if(quartusCycloneIIPath != null) {
      targets += new Target {
        override def getFamilyName(): String = "Cyclone II"

        override def synthesise(rtl: Rtl, workspace: String): Report = {
          QuartusFlow(
            quartusPath = quartusCycloneIIPath,
            workspacePath = workspace,
            toplevelPath = rtl.getRtlPath(),
            family = getFamilyName(),
            device = "EP2C35F672C6"
          )
        }
      }
    }

    targets
  }
}

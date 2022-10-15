package spinal.lib.tools

import spinal.core._
import scala.language._

/**
 * Module topology analyzer. It provides some methods that return the input or output pins,
 * all sub-modules or sub-blackboxes, all clocks inside, and apply condition to the returning
 * results.
 * @param module the module being analyzed
 */
class ModuleAnalyzer(module: Module) {
  def allInputs: Set[BaseType] = module.getAllIo.filter(_.isInputOrInOut).toSet
  def getInputs(filter: BaseType => Boolean): Set[BaseType] = allInputs.filter(filter)

  def allOutputs: Set[BaseType] = module.getAllIo.filter(_.isOutputOrInOut).toSet
  def getOutputs(filter: BaseType=> Boolean): Set[BaseType] = allOutputs.filter(filter)

  def allClocks: Set[ClockDomain] = {
    val ret = Set.newBuilder[ClockDomain]
    module.walkComponents {comp=>
      comp.dslBody.walkDeclarations {ds=>
        ds.foreachClockDomain{cd=>
          ret += cd
        }
      }
    }
    ret.result()
  }
  def getClocks(filter: ClockDomain=> Boolean): Set[ClockDomain] = allClocks.filter(filter)

  def allRegisters: Set[BaseType] = {
    val ret = Set.newBuilder[BaseType]
    module.dslBody.walkDeclarations {
      case ds: BaseType if ds.isReg=>
        ret += ds
      case _=>
    }
    ret.result()
  }
  def getRegisters(filter: BaseType=> Boolean): Set[BaseType] = allRegisters.filter(filter)

  def getCells(cond: Module=> Boolean): Set[Module] = {
    val ret = Set.newBuilder[Module]
    module.walkComponents{m=>
      if (cond(m)) ret += m
    }
    ret.result()
  }
  def getLibCells(cond: BlackBox=> Boolean): Set[BlackBox] = {
    val ret = Set.newBuilder[BlackBox]
    module.walkComponents{
      case bb: BlackBox if cond(bb) =>
        ret += bb
      case _=>
    }
    ret.result()
  }
  def getNets(cond: BaseType=> Boolean): Set[BaseType] = {
    val ret = Set.newBuilder[BaseType]
    module.walkComponents{m=>
      m.dslBody.walkDeclarations {
        case ds: BaseType if cond(ds)=>
          ret += ds
        case _=>
      }
    }
    ret.result()
  }
  def getPins(cond: BaseType=> Boolean): Set[BaseType] = {
    val ret = Set.newBuilder[BaseType]
    module.walkComponents{m=>
      m.dslBody.walkDeclarations {
        case ds: BaseType if (ds.isInputOrInOut || ds.isOutput) && cond(ds) =>
          ret += ds
        case _=>
      }
    }
    ret.result()
  }
  def getLibPins(cond: BaseType=> Boolean): Set[BaseType] = {
    val ret = Set.newBuilder[BaseType]
    val cellTrue = (_: BlackBox) => true
    getLibCells(cellTrue).foreach{bb=>
      val e = new ModuleAnalyzer(bb)
      ret ++= e.getPins(cond)
    }
    ret.result()
  }
  def getPort(cond: BaseType=> Boolean): Set[BaseType] = {
    val e = new ModuleAnalyzer(module.globalData.toplevel)
    (e.allInputs ++ e.allOutputs).filter(cond)
  }

}

object ModuleAnalyzer {
  implicit def toElaborator(module: Module): ModuleAnalyzer = new ModuleAnalyzer(module)
  val cellTrue = (_: Component) => true
  val dataTrue = (_: Data) => true
}

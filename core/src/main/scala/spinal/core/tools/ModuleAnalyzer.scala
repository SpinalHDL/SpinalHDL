package spinal.core.tools

import spinal.core._

import scala.language._
import scala.collection.mutable

/** Module topology analyzer. It provides some methods that return the input or output pins,
  * all sub-modules or sub-blackboxes, all clocks inside, and filter the returned
  * results.
  * @param module the module being analyzed
  */
class ModuleAnalyzer(module: Module) {

  /** Get all input ports of the module
    * @return set of base type
    */
  def getInputs: mutable.LinkedHashSet[BaseType] =
    module.getAllIo.asInstanceOf[mutable.LinkedHashSet[BaseType]].filter(_.isInputOrInOut)

  /** Get the input ports matching the condition
    * @param filter the predicate to filter the input ports
    * @return set of filtered input ports
    */
  def getInputs(filter: BaseType => Boolean): mutable.LinkedHashSet[BaseType] = getInputs.filter(filter)

  /** Get all output ports
    * @return set of output base type
    */
  def getOutputs: mutable.LinkedHashSet[BaseType] =
    module.getAllIo.asInstanceOf[mutable.LinkedHashSet[BaseType]].filter(_.isOutputOrInOut)

  /** Get the output ports matching the condition
    * @param filter the predicate to filter the output ports
    * @return set of filtered output ports
    */
  def getOutputs(filter: BaseType => Boolean): mutable.LinkedHashSet[BaseType] = getOutputs.filter(filter)

  /** Get all the clock domains inside the module
    * @return set of the clock domains
    */
  def getClocks: mutable.LinkedHashSet[ClockDomain] = {
    val ret = mutable.LinkedHashSet.newBuilder[ClockDomain]
    module.walkComponents { comp =>
      comp.dslBody.walkDeclarations { ds =>
        ds.foreachClockDomain { cd =>
          ret += cd
        }
      }
    }
    ret.result()
  }

  /** Get the clock domains matching the condition
    * @param filter the predicate to filter the clock domains
    * @return set of clock domain
    */
  def getClocks(filter: ClockDomain => Boolean): mutable.LinkedHashSet[ClockDomain] = getClocks.filter(filter)

  /** Get all the registers inside the module
    * @return set of register of base type
    */
  def getRegisters: mutable.LinkedHashSet[BaseType] = {
    val ret = mutable.LinkedHashSet.newBuilder[BaseType]
    module.dslBody.walkDeclarations {
      case ds: BaseType if ds.isReg =>
        ret += ds
      case _ =>
    }
    ret.result()
  }

  /** Get the registers matching the condition
    * @param filter the predicate to filter the registers
    * @return set of filtered registers
    */
  def getRegisters(filter: BaseType => Boolean): mutable.LinkedHashSet[BaseType] = getRegisters.filter(filter)

  /** Get the submodule instances that meet the condition
    * @param cond the condition that instance should meet.
    * @return set of sub module instances
    */
  def getCells(cond: Module => Boolean): mutable.LinkedHashSet[Module] = {
    val ret = mutable.LinkedHashSet.newBuilder[Module]
    module.walkComponents { m =>
      if (cond(m)) ret += m
    }
    ret.result()
  }

  /** Get the sub-blackbox instances that meet the condition
    * @param cond the condition that instance should meet.
    * @return set of sub blackbox instances
    */
  def getLibCells(cond: BlackBox => Boolean): mutable.LinkedHashSet[BlackBox] = {
    val ret = mutable.LinkedHashSet.newBuilder[BlackBox]
    module.walkComponents {
      case bb: BlackBox if cond(bb) =>
        ret += bb
      case _ =>
    }
    ret.result()
  }

  /** Get the wire/net inside the module
    * @param cond the filtering condition
    * @return set of the base type nets
    */
  def getNets(cond: BaseType => Boolean): mutable.LinkedHashSet[BaseType] = {
    val ret = mutable.LinkedHashSet.newBuilder[BaseType]
    module.walkComponents { m =>
      m.dslBody.walkDeclarations {
        case ds: BaseType if cond(ds) =>
          ret += ds
        case _ =>
      }
    }
    ret.result()
  }

  /** Get pins of the sub-module instance inside the module.
    * @param cond the filtering condition
    * @return set of the pin type
    */
  def getPins(cond: BaseType => Boolean): mutable.LinkedHashSet[BaseType] = {
    val ret = mutable.LinkedHashSet.newBuilder[BaseType]
    module.walkComponents { m =>
      m.dslBody.walkDeclarations {
        case ds: BaseType if (ds.isInputOrInOut || ds.isOutput) && cond(ds) =>
          ret += ds
        case _ =>
      }
    }
    ret.result()
  }

  /** Get pins of the sub-blackbox instance inside the module.
    * @param cond the filtering condition
    * @return set of the pin type
    */
  def getLibPins(cond: BaseType => Boolean): mutable.LinkedHashSet[BaseType] = {
    val ret = mutable.LinkedHashSet.newBuilder[BaseType]
    val cellTrue = (_: BlackBox) => true
    getLibCells(cellTrue).foreach { bb =>
      val e = new ModuleAnalyzer(bb)
      ret ++= e.getPins(cond)
    }
    ret.result()
  }

  /** Get the toplevel module's ports
    * @param cond the filtering condition
    * @return set of the ports
    */
  def getPorts(cond: BaseType => Boolean): mutable.LinkedHashSet[BaseType] = {
    val e = new ModuleAnalyzer(module.globalData.toplevel)
    (e.getInputs ++ e.getOutputs).filter(cond)
  }

}

object ModuleAnalyzer {

  /** Implicitly convert Module/Component into ModuleAnalyzer */
  implicit def toAnalyzer(module: Module): ModuleAnalyzer = new ModuleAnalyzer(module)

  /** handy always-true filter */
  val cellTrue = (_: Component) => true
  val dataTrue = (_: BaseType) => true
}

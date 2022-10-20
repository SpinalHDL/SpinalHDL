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
  /**
    * Return all input port of the module
    * @return set of base type
    */
  def allInputs: Set[BaseType] = module.getAllIo.filter(_.isInputOrInOut).toSet

  /**
    * Filter the input ports
    * @param filter - the predicate to filter the input ports
    * @return set of filtered input ports
    */
  def getInputs(filter: BaseType => Boolean): Set[BaseType] = allInputs.filter(filter)

  /**
    * Get all output ports
    * @return set of output base type
    */
  def allOutputs: Set[BaseType] = module.getAllIo.filter(_.isOutputOrInOut).toSet

  /**
    * Filter the output ports
    * @param filter - the predicate to filter the output ports
    * @return set of filtered output ports
    */
  def getOutputs(filter: BaseType=> Boolean): Set[BaseType] = allOutputs.filter(filter)

  /**
    * Get all the clock domains inside the module
    * @return set of the clock domains
    */
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

  /**
    * Filter the clock domains
    * @param filter - the predicate to filter the clock domains
    * @return set of clock domain
    */
  def getClocks(filter: ClockDomain=> Boolean): Set[ClockDomain] = allClocks.filter(filter)

  /**
    * Get all the registers inside the module
    * @return set of register of base type
    */
  def allRegisters: Set[BaseType] = {
    val ret = Set.newBuilder[BaseType]
    module.dslBody.walkDeclarations {
      case ds: BaseType if ds.isReg=>
        ret += ds
      case _=>
    }
    ret.result()
  }

  /**
    * Filter the registers
    * @param filter  - the predicate to filter the clock domains
    * @return set of filtered registers
    */
  def getRegisters(filter: BaseType=> Boolean): Set[BaseType] = allRegisters.filter(filter)

  /**
    * Get the submodule instances that meet the condition
    * @param cond - the condition that instance should meet.
    * @return - set of sub module instances
    */
  def getCells(cond: Module=> Boolean): Set[Module] = {
    val ret = Set.newBuilder[Module]
    module.walkComponents{m=>
      if (cond(m)) ret += m
    }
    ret.result()
  }
  /**
    * Get the sub-blackbox instances that meet the condition
    * @param cond - the condition that instance should meet.
    * @return - set of sub blackbox instances
    */
  def getLibCells(cond: BlackBox=> Boolean): Set[BlackBox] = {
    val ret = Set.newBuilder[BlackBox]
    module.walkComponents{
      case bb: BlackBox if cond(bb) =>
        ret += bb
      case _=>
    }
    ret.result()
  }

  /**
    * Get the wire/net inside the module
    * @param cond the filtering condition
    * @return set of the base type nets
    */
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

  /**
    * Get pins of the sub-module instance inside the module.
    * @param cond the filtering condition
    * @return set of the pin type
    */
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
  /**
    * Get pins of the sub-blackbox instance inside the module.
    * @param cond the filtering condition
    * @return set of the pin type
    */
  def getLibPins(cond: BaseType=> Boolean): Set[BaseType] = {
    val ret = Set.newBuilder[BaseType]
    val cellTrue = (_: BlackBox) => true
    getLibCells(cellTrue).foreach{bb=>
      val e = new ModuleAnalyzer(bb)
      ret ++= e.getPins(cond)
    }
    ret.result()
  }

  /**
    * Get the toplevel module's ports
    * @param cond the filtering condition
    * @return set of the ports
    */
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

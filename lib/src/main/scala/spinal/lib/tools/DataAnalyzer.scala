package spinal.lib.tools

import spinal.core._
import spinal.core.internals._

import scala.collection.immutable.ListSet
import scala.language.implicitConversions
import scala.collection.mutable

/** Base type data analyzer. It provides some utilities that help designer analyze the
  * circuit at the runtime.
  * @param data - the data to be analyzed. The analyze unit is BaseType signal like Bits/UInt/SInt/Bool
  */
class DataAnalyzer(data: BaseType) {

  /** Return all the fan-ins signals of this data
    * @return - a set of fan-ins signals.
    */
  def getFanIn: mutable.LinkedHashSet[BaseType] = {
    val ret = mutable.LinkedHashSet.newBuilder[BaseType]
    data.foreachStatements { st =>
      if (!(st.isInstanceOf[InitAssignmentStatement] || st.isInstanceOf[InitialAssignmentStatement])) {
        st.foreachDrivingExpression {
          case bt: BaseType => ret += bt
          case _            =>
        }
        st.walkParentTreeStatementsUntilRootScope { tree =>
          tree.walkDrivingExpressions {
            case bt: BaseType => ret += bt
            case _            =>
          }
        }
      }
    }
    ret.result()
  }

  /** Filter the fan-ins according to the condition.
    * @param cond - a predicate to filter the fan-in
    * @return a set of fan-ins signals
    */
  def getFanIn(cond: BaseType => Boolean): mutable.LinkedHashSet[BaseType] = getFanIn.filter(cond)

  /** Iterate on the filtered fan-ins.
    * @param cond -  a predicate to filter the fan-in
    * @param func - the function applied on the data
    */
  def walkFanIn(cond: BaseType => Boolean)(func: BaseType => Unit): Unit = getFanIn(cond).foreach(func)

  /** Return all the fan-outs signals of this data
    * @return - a set of fan-outs signals.
    */
  def getFanOut: mutable.LinkedHashSet[BaseType] = {
    import ModuleAnalyzer._
    data.globalData.toplevel.getNets { bt =>
      val e = new DataAnalyzer(bt)
      e.getFanIn.contains(data)
    }
  }

  /** Filter the fan-outs according to the condition.
    * @param cond - a predicate to filter the fan-out
    * @return a set of fan-outs signals
    */
  def getFanOut(cond: BaseType => Boolean): mutable.LinkedHashSet[BaseType] = getFanOut.filter(cond)

  /** Iterate on the filtered fan-outs.
    * @param cond -  a predicate to filter the fan-outs
    * @param func - the function applied on the data
    */
  def walkFanOut(cond: BaseType => Boolean)(func: BaseType => Unit): Unit = getFanOut(cond).foreach(func)
}

object DataAnalyzer {
  implicit def toAnalyzer(data: BaseType): DataAnalyzer = new DataAnalyzer(data)
}

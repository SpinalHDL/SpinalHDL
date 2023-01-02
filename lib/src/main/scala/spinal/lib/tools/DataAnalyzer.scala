package spinal.lib.tools

import spinal.core._
import spinal.core.internals._

import scala.collection.immutable.ListSet
import scala.language.implicitConversions
import scala.collection.mutable

/** Base type data analyzer. It provides some utilities that help designer analyze the
  * circuit at runtime.
  * @param data the data to be analyzed. The analyzed unit is BaseType signal like Bits/UInt/SInt/Bool
  */
class DataAnalyzer(data: BaseType) {

  /** Get all the fan-in signals of this data
    * @return a set of fan-in signals.
    */
  def getFanIn: mutable.LinkedHashSet[BaseType] = {
    val ret = mutable.LinkedHashSet.newBuilder[BaseType]

    def traceBaseTypeFromExpression(e: Expression): mutable.LinkedHashSet[BaseType] = {
      val ret = mutable.LinkedHashSet.newBuilder[BaseType]
      e.foreachDrivingExpression {
        case bt: BaseType => ret += bt
        case e            => ret ++= traceBaseTypeFromExpression(e)
      }
      ret.result()
    }

    data.foreachStatements { st =>
      if (!(st.isInstanceOf[InitAssignmentStatement] || st.isInstanceOf[InitialAssignmentStatement])) {
        st.foreachDrivingExpression { e =>
          e match {
            case bt: BaseType => ret += bt
            case e            => ret ++= traceBaseTypeFromExpression(e)
          }
        }
        st.walkParentTreeStatementsUntilRootScope { tree =>
          tree.walkDrivingExpressions { e =>
            e match {
              case bt: BaseType => ret += bt
              case e            => ret ++= traceBaseTypeFromExpression(e)
            }
          }
        }
      }
    }
    ret.result()
  }

  /** Get the fan-ins matching the condition
    * @param cond a predicate to filter the fan-ins
    * @return a set of fan-in signals
    */
  def getFanIn(cond: BaseType => Boolean): mutable.LinkedHashSet[BaseType] = getFanIn.filter(cond)

  /** Iterate on the filtered fan-ins.
    * @param cond a predicate to filter the fan-ins
    * @param func the function applied on the data
    */
  def walkFanIn(cond: BaseType => Boolean)(func: BaseType => Unit): Unit = getFanIn(cond).foreach(func)

  /** Get all the fan-out signals of this data
    * @return a set of fan-out signals.
    */
  def getFanOut: mutable.LinkedHashSet[BaseType] = {
    import ModuleAnalyzer._
    data.globalData.toplevel.getNets { bt =>
      val e = new DataAnalyzer(bt)
      e.getFanIn.contains(data)
    }
  }

  /** Get the fan-outs matching the condition
    * @param cond a predicate to filter the fan-outs
    * @return a set of fan-out signals
    */
  def getFanOut(cond: BaseType => Boolean): mutable.LinkedHashSet[BaseType] = getFanOut.filter(cond)

  /** Iterate on the filtered fan-outs.
    * @param cond a predicate to filter the fan-outs
    * @param func the function applied on the data
    */
  def walkFanOut(cond: BaseType => Boolean)(func: BaseType => Unit): Unit = getFanOut(cond).foreach(func)
}

object DataAnalyzer {

  /** Implicitly convert the BaseType into DataAnalyzer */
  implicit def toAnalyzer(data: BaseType): DataAnalyzer = new DataAnalyzer(data)
}

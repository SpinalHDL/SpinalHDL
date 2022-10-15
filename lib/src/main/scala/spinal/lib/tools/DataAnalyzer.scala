package spinal.lib.tools
import spinal.core._
import spinal.core.internals._

import scala.language.implicitConversions

class DataAnalyzer(data: BaseType) {
  def allFanIn: Set[BaseType] = {
    val ret = Set.newBuilder[BaseType]
    data.foreachStatements {st=>
      if (!(st.isInstanceOf[InitAssignmentStatement] || st.isInstanceOf[InitialAssignmentStatement])) {
        st.foreachDrivingExpression{
          case bt: BaseType=> ret += bt
          case _=>
        }
        st.walkParentTreeStatementsUntilRootScope{tree=> tree.walkDrivingExpressions{
          case bt: BaseType=> ret += bt
          case _=>
        }}
      }
    }
    ret.result()
  }
  def getFanIn(cond: BaseType=> Boolean) = allFanIn.filter(cond)
  def walkFanIn(cond: BaseType=> Boolean)(func: BaseType=> Unit): Unit = getFanIn(cond).foreach(func)

  def allFanOut: Set[BaseType] = {
    import ModuleAnalyzer._
    data.globalData.toplevel.getNets{bt=>
      val e = new DataAnalyzer(bt)
      e.allFanIn.contains(data)
    }
  }
  def getFanOut(cond: BaseType=> Boolean): Set[BaseType] = allFanOut.filter(cond)
  def walkFanOut(cond: BaseType=> Boolean)(func: BaseType=> Unit): Unit = getFanOut(cond).foreach(func)
}

object DataAnalyzer {
  implicit def toAnalyzer(data: BaseType): DataAnalyzer = new DataAnalyzer(data)
}

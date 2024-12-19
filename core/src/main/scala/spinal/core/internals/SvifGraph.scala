package spinal.core.internals

import spinal.core._
import scala.collection.{immutable, mutable}

class SvifGraph(
  val intfSet: mutable.LinkedHashSet[Interface], 
) {
  var child: SvifGraph = null

  //var parent: SvifGraph = null
  var count: Int = 0

  def anyIntf: Interface = {
    assert(intfSet.size > 0)
    for (interface <- intfSet.view) {
      return interface
    }
    return null
  }
  def origDefinitionName: String = {
    //assert(interface != null)
    val myIntf = anyIntf
    if (myIntf == null) {
      return ""
    }
    myIntf.origDefinitionName
  }
  //--------
  def addChild(newChild: SvifGraph): Unit = {
    //newChild.parent = this
    newChild.count = this.count + 1
    child = newChild
  }
  //--------

}

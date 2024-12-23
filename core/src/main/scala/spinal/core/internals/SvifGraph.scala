package spinal.core.internals

import spinal.core._
import scala.collection.{immutable, mutable}

class SvifGraph(
  val intfSet: mutable.HashSet[Interface], 
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
    child = newChild
    //child.parent = this
    child.count = this.count + 1
  }
  //--------
  def findChildInterface(interface: Interface): Option[SvifGraph] = {
    if (intfSet.contains(interface)) {
      return Some(this)
    } else if (child != null) {
      return child.findChildInterface(interface=interface)
    } else {
      return None
    }
  }
  //def getBottom(): SvifGraph = {
  //  if (child == null) {
  //    this
  //  } else {
  //    child.getBottom()
  //  }
  //}

}

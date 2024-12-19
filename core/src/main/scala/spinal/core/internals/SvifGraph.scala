package spinal.core.internals

import spinal.core._
import scala.collection.{immutable, mutable}

class SvifGraph(
  // maps from `emitInterface(interface)()` to the set of `Interface`s that have that
  // `emitInterface(interface).result()`
  //val intfCache: mutable.ArrayBuffer[(StringBuilder, mutable.LinkedHashSet[Interface])],
  val intfSet: mutable.LinkedHashSet[Interface], 
  //val intfMap: mutable.LinkedHashMap[Interface, 
  //val interfaceCache: StringBuilder,

  //val origDefinitionName: String,
  //var interfaceString: StringBuilder,
) {
  //val intfSet = mutable.LinkedHashSet[Interface]()
  //val children = mutable.ArrayBuffer[SvifGraph]()
  var child: SvifGraph = null
  //val auxIntfSet = mutable.LinkedHashSet[Interface]()

  // where are the members?
  //val extGraphNameMap = mutable.LinkedHashSet[(String, Int)]()
  //var child: SvifGraph = null
  var parent: SvifGraph = null
  //var newName: String = ""
  var count: Int = 0

  def anyIntf: Interface = {
    assert(intfSet.size > 0)
    //var ret: Interface = null
    //var prevInterface: Interface = null
    for (/*(*/interface/*, idx)*/ <- intfSet.view) {
      //if (prevInterface != null) {
      //  if (prevInterface.origDefinitionName != interface.origDefinitionName) {
      //    println(
      //      s"firstIntf: eek! "
      //      + s"prev:${prevInterface.origDefinitionName} curr:${interface.origDefinitionName}"
      //    )
      //    assert(false)
      //  }
      //  //println(
      //  //  s"firstIntf (idx == ${idx}): Found this other `prevInterface`: "
      //  //  + s"${prevInterface.getName()} ${prevInterface.origDefinitionName}"
      //  //)
      //}
      return interface
      //if (idx == 0) {
      //  //if (prevInterface == null) {
      //  //  println(
      //  //    s"was this value first? (idx == ${idx})"
      //  //  )
      //  //}
      //  //ret = interface
      //  return interface
      //  //return ret
      //}
      //prevInterface = interface
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
    //--------
    // debug `assert`s until I get my logic entirely right
    //assert(newChild != null)
    //assert(newChild.children.size == 0)
    //assert(newChild.parent == null)
    //assert(!children.contains(newChild))
    //assert(children.size == 0) // temporary so I don't have to change back my data structures for now
    //--------
    //assert(child == null)
    //child = newChild
    //parent.child = this
    //parent.children += this
    newChild.parent = this
    newChild.count = this.count + 1
    child = newChild
    //children += newChild
  }
  //--------
  //def addChild(child: SvifGraph): Boolean = {
  //  assert(child != null)
  //  if (!children.contains(child)) {
  //    // TODO: might be slow... maybe replace it with `mutable.LinkedHashSet[SvifGraph]`?
  //    children += child
  //    return true
  //  } else {
  //    return false
  //  }
  //}
  //def setInterface(newInterface: Interface): Unit = {
  //  assert(newInterface != null)
  //  assert(interface == null)
  //  interface = newInterface
  //}
  //--------
  //def findCount(toFind: Int): Option[SvifGraph] = {
  //  if (count == toFind) {
  //    return Some(this)
  //  } 
  //  //else if (child == null) {
  //  //  return None
  //  //} else {
  //  //  return child.findCount(toFind=toFind)
  //  //}
  //  for (child <- children) {
  //    child.findCount(toFind=toFind) match {
  //      case Some(childGraph) => {
  //        return Some(childGraph)
  //      }
  //      case None => {
  //      }
  //    }
  //  }
  //  return None
  //}
  def findInterface(toFind: Interface): (Boolean, SvifGraph) = {
    //assert(intfSet.size > 0)
    if (intfSet.contains(toFind)) {
      return (false, this)
    } else if (
      //children.size != 0
      child != null
    ) {
      //assert(children.size == 1)
      return (
        //children(0)
        child
      ).findInterface(toFind=toFind)
    } else {
      return (true, null)
    }
    //for (child <- children) {
    //  child.findInterface(toFind=toFind) match {
    //    case Some(childGraph) => {
    //      return Some(childGraph)
    //    }
    //    case None =>
    //  }
    //}
    //return None
    //else if (child != null) {
    //  return child.findInterface(toFind=toFind)
    //} else {
    //  return (true, this)
    //}
  }

  def findInterfaceString(
    emitInterfaceFunc: (Interface) => StringBuilder,
    toFind: StringBuilder,
  ): (Boolean, SvifGraph) = {
    var prevInterface: Interface = null
    //for ((interface, idx) <- intfSet.zipWithIndex) {
    //  if (idx > 0) {
    //    val currEmitResult: String = emitInterfaceFunc(interface).result()
    //    val prevEmitResult: String = emitInterfaceFunc(prevInterface).result()
    //    if (currEmitResult != prevEmitResult) {
    //      println(
    //        s"eek! findInterfaceString(): "
    //      )
    //      println(
    //        s"curr: "
    //        + currEmitResult
    //      )
    //      println(
    //        "-------- --------"
    //      )
    //      println(
    //        s"prev: "
    //        + prevEmitResult
    //      )
    //      println(
    //        "-------- --------"
    //      )
    //    }
    //  }
    //  prevInterface = interface
    //  //if (emitInterfaceFunc(firstIntf).result() == toFind.result()) {
    //  //  
    //  //}
    //}
    val searchEmitResult = emitInterfaceFunc(anyIntf).result()
    if (searchEmitResult == toFind.result()) {
      //println(
      //  s"Apparently, these are the same:"
      //)
      //println(
      //  s"searchEmitResult: "
      //  + searchEmitResult
      //)
      //println(
      //  "-------- --------"
      //)
      //println(
      //  s"toFind.result(): "
      //  + toFind.result()
      //)
      //println(
      //  "-------- --------"
      //)
      return (false, this) //(this, count)
    } else if (
      //children.size != 0
      child != null
    ) {
      //assert(children.size == 1)
      return (
        //children(0)
        child
      ).findInterfaceString(emitInterfaceFunc=emitInterfaceFunc, toFind=toFind)
    } else {
      //println(
      //  s"Couldn't find this interface:"
      //)
      //println(
      //  s"toFind.result(): "
      //  + toFind.result()
      //)
      //println(
      //  "-------- --------"
      //)
      return (true, this) //None//(this, -1)
    }
    //for (child <- children) {
    //  child.findInterfaceString(emitInterfaceFunc=emitInterfaceFunc, toFind=toFind) match {
    //    case Some(childGraph) => {
    //      return Some(childGraph)
    //    }
    //    case None =>
    //  }
    //}
    //return None
  }
  def getTop(): SvifGraph = {
    if (
      //children.size != 0
      child != null
    ) {
      //assert(children.size == 1)
      return (
        //children(0)
        child
      ).getTop()
    } else {
      return this
    }
  }
  //--------
  //def findLowest(
  //  toFind: StringBuilder,
  //): Option[SvifGraph] = {
  //  return None
  //}
  //def findUpChild(base: Data, parentIntf: Interface): Option[(SvifGraph, Interface)] = {
  //  val rootIFList = base.rootIFList()
  //  //assert(rootIFList.contains(parentIntf))
  //  //if (!rootIFList.contains(parent)) {
  //  //  return None
  //  //}
  //  //val rootIFListReverse = rootIFList.reverse
  //  //def innerFunc(
  //  //  idx: Int,
  //  //): Option[(SvifGraph, Interface)] = {
  //  //  val retIntf = rootIFListReverse(idx)
  //  //  findUpInterface(toFind=retIntf) match {
  //  //    case Some(ret) => {
  //  //      return Some((ret._1, retIntf))
  //  //    }
  //  //    case None => {
  //  //      if (idx - 1 >= 0) {
  //  //        return innerFunc(idx=idx - 1)
  //  //      } else {
  //  //        return None
  //  //      }
  //  //    }
  //  //  }
  //  //}
  //  //return innerFunc(idx=rootIFListReverse.size - 1)
  //  def innerFind(
  //  ): Unit = {
  //  }
  //  for ((interface, intfIdx) <- rootIFList.zipWithIndex) {
  //  }
  //  //def innerFunc(
  //  //  idx: Int,
  //  //): Option[(SvifGraph, Interface)] = {
  //  //  return None
  //  //}
  //  //return innerFunc(idx=rootIFList.size - 1)
  //  return None
  //}
  //def topNode(): SvifGraph = {
  //  if (parent == null) {
  //    return this
  //  } else {
  //    return parent.topNode()
  //  }
  //}
  //def hasChild(otherInterface: Interface): Boolean = {
  //  true
  //}
}

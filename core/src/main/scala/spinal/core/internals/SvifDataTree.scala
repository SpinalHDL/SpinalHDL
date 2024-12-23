package spinal.core.internals

import spinal.core._
import scala.collection.{immutable, mutable}

class SvifDataTree(
  val data: Data
) {
  def findChildData(toFind: Data): Option[SvifDataTree] = {
    if (data == toFind) {
      return Some(this)
    }
    data match {
      case rootVec: Vec[_] => {
        for (child <- rootVec.view) {
          val childTree = new SvifDataTree(data=child)
          childTree.findChildData(toFind) match {
            case Some(ret) => {
              return Some(ret)
            }
            case None =>
          }
        }
      }
      case _ => 
    }
    return None
  }
  //val vecSize: Option[Int] = {
  //  data match {
  //    case vec: Vec[_] => Some(vec.size)
  //    case _ => None
  //  }
  //}
  private def getParentVecAndThisIdx(): Option[(Vec[_], Int)] = {
    if (data.IFvecParent == null) {
      return None
    }
    data.IFvecParent match {
      case parentVec: Vec[_] => {
        for ((vecElem, vecIdx) <- parentVec.view.zipWithIndex) {
          if (vecElem == data) {
            return Some((parentVec, vecIdx))
          }
        }
      }
    }
    return None
  }
  val parentVecAndThisIdx: Option[(Vec[_], Int)] = getParentVecAndThisIdx()
}
//class SvifDataTree(
//  val data: Data,
//) {
//  //var top: SvifDataTree = null
//  //var parent: SvifDataTree = null
//  //val children = mutable.ArrayBuffer[SvifDataTree]()
//
//  //def contains(node: SvifDataTree): Boolean = {
//  //  if (top == null) {
//  //    top = this
//  //    assert(parent == null)
//  //    assert(children.size == 0)
//  //  }
//  //}
//
//  //private def setTopIfNeeded(): Unit = {
//  //  if (top == null) {
//  //    top = this
//  //    assert(parent == null)
//  //    assert(children.size == 0)
//  //  }
//  //}
//  //def findData(toFind: Data): Option[SvifDataTree] = {
//  //  setTopIfNeeded()
//  //  top.findChildData(toFind=toFind)
//  //}
//  //def findChildData(toFind: Data): Option[SvifDataTree] = {
//  //  if (data == toFind) {
//  //    return Some(this)
//  //  } else if (children.size > 0) {
//  //    for (child <- children.view) {
//  //      child.findChildData(toFind=toFind) match {
//  //        case Some(tree) => {
//  //          return Some(tree)
//  //        }
//  //        case None =>
//  //      }
//  //    }
//  //  } 
//  //  return None
//  //}
//  def findChildData(toFind: Data): Option[Data] = {
//    if (data == toFind) {
//      //return Some(this)
//      return Some(data)
//    } else { 
//      data match {
//      }
//    }
//    //if (children.size > 0) {
//    //  for (child <- children.view) {
//    //    child.findChildData(toFind=toFind) match {
//    //      case Some(tree) => {
//    //        return Some(tree)
//    //      }
//    //      case None =>
//    //    }
//    //  }
//    //} 
//    return None
//  }
//  //def addChild(
//  //  newChild: SvifDataTree
//  //): Unit = {
//  //  assert(newChild != null)
//  //  assert(newChild.children.size == 0)
//  //  //assert(findChildData(newChild.data) == None)
//  //  setTopIfNeeded()
//  //  top.findChildData(newChild.data) match {
//  //    case Some(_) => {
//  //      // just for debugging!
//  //      assert(false)
//  //    }
//  //    case None =>
//  //  }
//  //  children += newChild
//  //  children.last.top = this.top
//  //  children.last.parent = this
//  //  //children.last
//  //}
//  def size: Option[Int] = {
//    data match {
//      case vec: Vec[_] => Some(vec.size)
//      case _ => None
//    }
//  }
//}

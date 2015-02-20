/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package spinal

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC18F on 21.08.2014.
 */

object InputNormalize {
  def none(node: Node): Unit = {

  }
  def regImpl(node: Node): Unit = {
    val targetWidth = node.getWidth
    Misc.normalizeResize(node, RegS.getDataInputId, targetWidth)
    if(node.asInstanceOf[Reg].isUsingReset) Misc.normalizeResize(node, RegS.getInitialValueId, targetWidth)
  }

  def memReadImpl(node: Node): Unit = {
    //not here
    //Misc.normalizeResize(node, MemReadSync.getAddressId, node.asInstanceOf[].addressWidth)
  }

  def memWriteImpl(node: Node): Unit = {
    Misc.normalizeResize(node, MemWrite.getDataId, node.getWidth)
    Misc.normalizeResize(node, MemWrite.getAddressId, node.asInstanceOf[Mem[_]].addressWidth)
  }

  def nodeWidth(node: Node): Unit = {
    val targetWidth = node.getWidth
    for (i <- 0 until node.inputs.size)
      Misc.normalizeResize(node, i, targetWidth)
  }
  def inputWidthMax(node: Node): Unit = {
    val targetWidth = Math.max(node.inputs(0).getWidth, node.inputs(1).getWidth)
    for (i <- 0 until node.inputs.size)
      Misc.normalizeResize(node, i, targetWidth)
  }
}

object WidthInfer {
  def inputMaxWidthl(node: Node): Int = {
    node.inputs.foldLeft(-1)((best, n) => Math.max(best, if (n != null) n.getWidth else -1))
  }
  def regImpl(node: Node): Int = {
    val dataIn = node.inputs(0)
    val init = node.inputs(1)
    math.max(if (dataIn != node) dataIn.getWidth else -1,if(node.asInstanceOf[Reg].isUsingReset) init.getWidth else -1)
  }

  def cumulateInputWidth(node: Node): Int = {
    node.inputs.foldLeft(0)((old, n) => old + Math.max(0, n.getWidth))
  }

  def intLit1Width(node: Node): Int = {
    node.inputs(1).asInstanceOf[IntLiteral].value.toInt
  }
  def input0Width(node: Node): Int = {
    node.inputs(0).getWidth
  }

  def shiftLeftWidth(node: Node): Int = {
    return node.inputs(0).getWidth + node.inputs(1).asInstanceOf[MinMaxProvider].maxValue.toInt
  }
  def shiftRightWidth(node: Node): Int = {
    return input0Width(node)
  }

  def oneWidth(node: Node): Int = 1

}







abstract class Node extends ContextUser with ScalaLocated with SpinalTagReady with GlobalDataUser{
  val consumers = new ArrayBuffer[Node]
  val inputs = new ArrayBuffer[Node]



  var instanceCounter = globalData.getInstanceCounter

  def getWidth: Int = {
    if (globalData.nodeAreInferringWidth) {
      inferredWidth
    } else {
      val isFirst = globalData.nodeGetWidthWalkedSet.isEmpty
      if (globalData.nodeGetWidthWalkedSet.contains(this))
        SpinalError(s"Can't calculate width of $this when design is in construction phase")

      globalData.nodeGetWidthWalkedSet += this
      var temp: Int = 0;
      if (isFirst) {
        try {
          temp = calcWidth
        } catch {
          case e: Exception => {
            globalData.nodeGetWidthWalkedSet.clear()
            throw e
          }
        }
      } else {
        temp = calcWidth
      }

      if (temp == -1) {
        globalData.nodeGetWidthWalkedSet.clear()
        SpinalError(s"Can't infer width on $this because of unspecified width")
      }

      globalData.nodeGetWidthWalkedSet -= this

      if (isFirst) globalData.nodeGetWidthWalkedSet.clear()
      temp
    }
  }

  def calcWidth: Int


  def inferWidth: Boolean = {
    val newWidth: Int = calcWidth
    if (newWidth == -1) {
      return true
    } else if (newWidth != inferredWidth) {
      inferredWidth = newWidth
      return true;
    } else {
      return false
    }
  }


  var inferredWidth = -1

  def normalizeInputs: Unit = {

  }

  def setInput(node: Node): Unit = {
    inputs(0) = node
  }


  def getClassIdentifier: String = this.getClass.getSimpleName

  def isInBlackBoxTree = component.isInBlackBoxTree
  def nonRecursiveToString(): String = {
    toString()
  }
}


class NoneNode extends Node{
  override def calcWidth: Int = 0
}
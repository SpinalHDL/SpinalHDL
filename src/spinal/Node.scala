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
    for (i <- 0 until 2)
      Misc.normalizeResize(node, i, targetWidth)
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
    math.max(if(dataIn != node)dataIn.getWidth else -1, if(init != node)init.getWidth else -1)
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

object Node {


  var areInferringWidth = false
  val getWidthWalkedSet: mutable.Set[Node] = mutable.Set[Node]()
  val widthInferredCheck = ArrayBuffer[() => Unit]()
}

//object NoNode extends Node{
//  def calcWidth: Int = 0
//}

abstract class Node extends ComponentLocated {
  val consumers = new ArrayBuffer[Node]
  val inputs = new ArrayBuffer[Node]





  var dontSimplify = false
  def dontSimplifyIt = dontSimplify = true



  def getWidth: Int = {
    if (Node.areInferringWidth) {
      inferredWidth
    } else {
      val isFirst = Node.getWidthWalkedSet.isEmpty
      if (Node.getWidthWalkedSet.contains(this))
        SpinalError(s"Can't calculate width of $this when design is in construction phase")

      Node.getWidthWalkedSet += this
      var temp: Int = 0;
      if (isFirst) {
        try {
          temp = calcWidth
        } catch {
          case e: Exception => {
            Node.getWidthWalkedSet.clear()
            throw e
          }
        }
      }else{
        temp = calcWidth
      }

      if(temp == -1){
        Node.getWidthWalkedSet.clear()
        SpinalError(s"Can't infer width on $this because of unspecified width")
      }

      Node.getWidthWalkedSet -= this

      if (isFirst) Node.getWidthWalkedSet.clear()
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



  def nonRecursiveToString() : String = {
    toString()
  }
}

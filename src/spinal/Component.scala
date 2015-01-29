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


object Component {
  def apply[T <: Component](c: T): T = {
    pop(c);
    return c;
  }


  val stack = new SafeStack[Component]


  def push(c: Component): Unit = {
    //  if (when.stack.size() != 0) throw new Exception("Creating a component into hardware conditional expression")
    stack.push(c)
  }

  def pop(c: Component): Unit = {
    try {
      if(lastPoped == c) return;
      lastPoped = c
      stack.pop(c)
    } catch{
      case e: Exception => SpinalError(s"You probably forget the 'Component(new ${stack.head().getClass.getName})' into ${c.getClass.getName}")
    }
  }

  var lastPoped : Component = null

  def current = stack.head()
}


abstract class Component extends Nameable {
  val localScope = new Scope()

  def io: Data

  var outBindingHosted = mutable.Map[BaseType, OutBinding]()
  val additionalNodesRoot = mutable.Set[BaseType]()
  var definitionName = ""
  val level = Component.stack.size()
  val kinds = ArrayBuffer[Component]()
  val parent = Component.current
  if (parent != null) {
    parent.kinds += this;
  }
  def isTopLevel : Boolean = parent == null

  var nodes: ArrayBuffer[Node] = null


  var pulledDataCache = mutable.Map[Data, Data]()

  if (Component.stack.stack.isEmpty) {
    BackendToComponentBridge.defaultClock.component = this
    BackendToComponentBridge.defaultReset.component = this
  }
  Component.push(this)


  def parents(of: Component = this, list: List[Component] = Nil): List[Component] = {
    if (of.parent == null) return list
    parents(of.parent, of.parent :: list)
  }

  def nameElements(): Unit = {
    Misc.reflect(this, (name, obj) => {
      obj match {
        case component: Component => {
          component.setWeakName(name)
        }
        case namable: Nameable => {
          if (!namable.isInstanceOf[ComponentLocated] || namable.asInstanceOf[ComponentLocated].component == this)
            namable.setWeakName(name)
        }
        case _ =>
      }
    })
  }

  def allocateNames(): Unit = {
    for (node <- nodes) node match {
      case nameable: Nameable => {
        if (nameable.isUnnamed) {
          nameable.setWeakName("zz")
        }
        if (nameable.isWeak)
          nameable.setName(localScope.allocateName(nameable.getName()));
        else
          localScope.iWantIt(nameable.getName())
      }
      case _ =>
    }
    for (kind <- kinds) {
      if (kind.isUnnamed) {
        var name = kind.getClass.getSimpleName
        name = Character.toLowerCase(name.charAt(0)) + (if (name.length() > 1) name.substring(1) else "");
        kind.setWeakName(name)
      }
      kind.setName(localScope.allocateName(kind.getName()));
    }
  }


  def getNodeIo = {
    val nodeIo = mutable.Set[BaseType]()
    if (nodes == null) {
      io.flatten.foreach(nodeIo += _._2)
    } else {
      nodes.foreach(node => node match {
        case b: BaseType => if (b.isIo) nodeIo += b
        case _ =>
      })
    }
    nodeIo
  }

  /*
    def getRegs = nodes.filter(node => node match{
      case b : BaseType => b.isReg
      case _ => false
    })*/
  def getRegs = {
    val regs = new ArrayBuffer[BaseType]()
    nodes.foreach(node => node match {
      case b: BaseType => if (b.isReg) regs += b
      case _ =>
    })
    regs
  }


  def findBinding(baseType: BaseType): OutBinding = {
    outBindingHosted.getOrElse(baseType, null)
  }


  def endComponent = Component.pop(this)
}



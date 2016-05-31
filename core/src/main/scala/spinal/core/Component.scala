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

package spinal.core

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object Component {
  def apply[T <: Component](c: T): T = {
    //    c.io.flatten.foreach(_._2.isIo = true)
    //    pop(c);
    //    c.userParentCalledDef
    return c
  }

  def push(c: Component): Unit = {
    //  if (when.stack.size() != 0) throw new Exception("Creating a component into hardware conditional expression")
    GlobalData.get.componentStack.push(c)
  }

  def pop(c: Component): Unit = {
    GlobalData.get.componentStack.pop(c)
  }

  //var lastPoped : Component = null

  def current: Component = current(GlobalData.get)

  def current(globalData: GlobalData): Component = globalData.componentStack.head()
}


// TODO Delayed init is deprecated
abstract class Component extends NameableByComponent with GlobalDataUser with ScalaLocated with DelayedInit with Stackable{

  override def delayedInit(body: => Unit) = {
    body

    if ((body _).getClass.getDeclaringClass == this.getClass) {
      // this.io.flatten.foreach(_.isIo = true)
      this.nameElements()
      Component.pop(this)
      this.userParentCalledDef
    }
  }


  //def io: Data
  private[core] val ioSet = mutable.Set[BaseType]()
  //  private[core] lazy val areaClassSet = {
  //    val ret = mutable.Map[Object,Object]()
  //
  //    def walk(that : Object) : Unit = {
  //      Misc.reflect(that, (name, obj) => {
  //        obj match {
  //          case obj : Area => {
  //            val req = ret.get(obj.getClass)
  //            if(req.isDefined){
  //              ret.put(obj.getClass , null)
  //            }else{
  //              ret.put(obj.getClass, obj)
  //            }
  //            walk(obj)
  //          }
  //        }
  //      })
  //    }
  //    walk(this)
  //    ret
  //  }

  val userCache = mutable.Map[Object, mutable.Map[Object, Object]]()
  private[core] val localScope = new Scope()
  private[core] val prePopTasks = mutable.ArrayBuffer[() => Unit]()
  private[core] val kindsOutputsToBindings = mutable.Map[BaseType, BaseType]()
  private[core] val kindsOutputsBindings = mutable.Set[BaseType]()
  private[core] val additionalNodesRoot = mutable.Set[Node]()
  var definitionName: String = null
  private[core] val level = globalData.componentStack.size()
  val kinds = ArrayBuffer[Component]()
  val parent = Component.current

  if (parent != null) {
    parent.kinds += this;
  } else {
    setWeakName("toplevel")
  }

  def addPrePopTask(task : () => Unit) = prePopTasks += task
//  def addPrePopTask(task :  => Unit) = prePopTasks += (() => {task; println("asd")})

  val clockDomain = ClockDomain.current

  def setDefinitionName(name: String): this.type = {
    definitionName = name
    this
  }

  private[core] def isTopLevel: Boolean = parent == null
  private[core] val initialAssignementCondition = globalData.conditionalAssignStack.head()
  var nodes: ArrayBuffer[Node] = null

  private[core] var pulledDataCache = mutable.Map[Data, Data]()

  Component.push(this)

  def parents(of: Component = this, list: List[Component] = Nil): List[Component] = {
    if (of.parent == null) return list
    parents(of.parent, of.parent :: list)
  }

  private[core] def reflectIo: Data = {
    try {
      val clazz = this.getClass
      val m = clazz.getMethod("io")
      m.invoke(this).asInstanceOf[Data]
    } catch {
      case _: Throwable => null
    }
  }

  def nameElements(): Unit = {
    Misc.reflect(this, (name, obj) => {
      obj match {
        case component: Component => {
          if (component.parent == this)
            component.setWeakName(name)
        }
        case nameable: Nameable => {
          if (!nameable.isInstanceOf[ContextUser])
            nameable.setWeakName(name)
          else if (nameable.asInstanceOf[ContextUser].component == this)
            nameable.setWeakName(name)
          else {
            for (kind <- kinds) {
              //Allow to name a component by his io reference into the parent component
              if (kind.reflectIo == nameable) {
                kind.setWeakName(name)
              }
            }
          }
        }
        case _ =>
      }
    })
  }

  private[core] def allocateNames(): Unit = {
    localScope.allocateName("zz")
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


  def getAllIo: mutable.Set[BaseType] = {

    if (nodes == null) {
      ioSet
    } else {
      val nodeIo = mutable.Set[BaseType]()
      nodes.foreach(node => node match {
        case b: BaseType => if (b.isIo) nodeIo += b
        case _ =>
      })
      nodeIo
    }

  }



  private[core] def getOrdredNodeIo = getAllIo.toList.sortWith(_.instanceCounter < _.instanceCounter)

  private[core] def getDelays = {
    val delays = new ArrayBuffer[SyncNode]()
    nodes.foreach(node => node match {
      case delay: SyncNode => delays += delay
      case _ =>
    })
    delays
  }

  private[core] def userParentCalledDef: Unit = {

  }

  private[core] def isInBlackBoxTree: Boolean = if (parent == null) false else parent.isInBlackBoxTree

  private[core] override def getComponent(): Component = parent


  override def getDisplayName(): String = if (isNamed) super.getDisplayName() else "[" + getClass.getSimpleName + "]"

  def getParentsPath(sep: String = "/"): String = if (parent == null) "" else parents().map(_.getDisplayName()).reduce(_ + sep + _)

  def getPath(sep: String = "/"): String = (if (parent == null) "" else (getParentsPath(sep) + sep)) + this.getDisplayName()

  def getGroupedIO(ioBundleBypass: Boolean): Seq[Data] = {
    val ret = mutable.Set[Data]()
    val ioBundle = if (ioBundleBypass) reflectIo else null
    def getRootParent(that: Data): Data = if (that.parent == null || that.parent == ioBundle) that else getRootParent(that.parent)
    for (e <- getAllIo) {
      ret += getRootParent(e)
    }
    ret.toSeq.sortBy(_.instanceCounter)
  }

  override def postPushEvent(): Unit = {
  //  println("push " + this.getClass.getSimpleName)
  }
  override def prePopEvent(): Unit = {
   // println("pop " + this.getClass.getSimpleName)
    for(t <- prePopTasks){
      t()
    }
    prePopTasks.clear()
  }



}



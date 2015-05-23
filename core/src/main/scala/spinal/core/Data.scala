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


object Data {

  def doPull[T <: Data](srcData: T, finalComponent: Component, useCache: Boolean = false, propagateName: Boolean = false): T = {
    val startComponent = srcData.component
    if (useCache) {
      val finalComponentCacheState = finalComponent.pulledDataCache.getOrElse(srcData, null)
      if (finalComponentCacheState != null) return finalComponentCacheState.asInstanceOf[srcData.type]
    }

    if (startComponent == finalComponent || (startComponent != null && finalComponent == startComponent.parent && srcData.isIo)) {
      finalComponent.pulledDataCache.put(srcData, srcData)
      return srcData
    }

    val startComponentStack = if (startComponent == null) List[Component](null) else startComponent.parents() :+ startComponent
    var componentPtr = finalComponent
    var nextData: srcData.type = null.asInstanceOf[srcData.type]
    var ret: srcData.type = null.asInstanceOf[srcData.type]

    //pull from final component to lower component (fall)
    while (!startComponentStack.contains(componentPtr)) {
      if (useCache) {
        val cacheState = componentPtr.pulledDataCache.getOrElse(srcData, null)
        if (cacheState != null) {
          Component.push(componentPtr)
          nextData assignFrom(cacheState, false)
          Component.pop(componentPtr)
          return ret
        }
      }
      Component.push(componentPtr)
      val from = srcData.clone()
      if (propagateName)
        from.setCompositeName(srcData)
      from.asInput
      from.isIo = true
      if (nextData != null) nextData := from else ret = from
      Component.pop(componentPtr)

      if (useCache) componentPtr.pulledDataCache.put(srcData, from)
      nextData = from
      componentPtr = componentPtr.parent
    }

    val lowerComponent = componentPtr
    var risePath = if (lowerComponent == null) List[Component]() else startComponentStack.takeRight(startComponentStack.size - 1 - componentPtr.parents().size)
    //pull from lower component to start component (rise)
    while (!risePath.isEmpty) {
      if (useCache) {
        val cacheState = componentPtr.pulledDataCache.getOrElse(srcData, null)
        if (cacheState != null) {
          Component.push(componentPtr)
          nextData assignFrom(cacheState, false)
          Component.pop(componentPtr)
          return ret
        }
      }
      val fromComponent = risePath.head
      if (fromComponent != startComponent || srcData.isIo == false) {
        Component.push(fromComponent)
        val from = srcData.clone()
        if (propagateName)
          from.setCompositeName(srcData)
        from.isIo = true
        from.asOutput
        Component.pop(fromComponent)
        Component.push(componentPtr)
        if (nextData != null) nextData := from else ret = from
        Component.pop(componentPtr)

        if (useCache) componentPtr.pulledDataCache.put(srcData, from)
        nextData = from
        componentPtr = fromComponent
      }
      risePath = risePath.tail
    }

    Component.push(nextData.component)
    nextData := srcData
    Component.pop(nextData.component)

    ret
  }
}

class DataPimper[T <: Data](pimpIt: T) {
  def ===(that: T): Bool = pimpIt.isEguals(that)
  def !==(that: T): Bool = pimpIt.isNotEguals(that)


  def :=(that: T): Unit = pimpIt assignFrom(that, false)

  //Use as \= to have the same behavioral than VHDL variable
  def \(that: T) : T = {
    val ret = cloneOf(that)
    ret := pimpIt
    ret.whenScope = pimpIt.whenScope
    ret := that
    ret
  }

  def <>(that: T): Unit = pimpIt autoConnect that
  def init(that: T): T = pimpIt.initImpl(that)
}

trait Data extends ContextUser with Nameable with Assignable with AttributeReady with SpinalTagReady with GlobalDataUser with ScalaLocated {
  var dir: IODirection = null
  var isIo = false

  def asInput: this.type = {
    dir = in;
    this
  }
  def asOutput: this.type = {
    dir = out;
    this
  }

  def isOutputDir: Boolean = dir == out
  def isInputDir: Boolean = dir == in

  def isOutput: Boolean = dir == out && isIo
  def isInput: Boolean = dir == in && isIo
  def isDirectionLess: Boolean = dir == null || !isIo
  def flip: this.type = {
    for (e <- flatten) {
      e.dir match {
        case `in` => e.dir = out
        case `out` => e.dir = in
        case _ => SpinalError(s"Can't flip a data that is direction less $e")
      }
    }
    this
  }


  val parentData = globalData.dataStack.head()

  def getZero: this.type = {
    val ret = clone()
    ret.flatten.foreach(t => {
      t := t.getZero
    })

    ret
  }

  def flatten: Seq[BaseType]

  def pull: this.type = Data.doPull(this, Component.current, false, false)

  //  def :-(that: => SSelf): this.type = {
  //    val task = () => {
  //      this := that
  //    }
  //    component.postCreationTask += task
  //
  //    this
  //  }

  def ##(right: Data): Bits = this.toBits ## right.toBits

  def toBits: Bits
  def assignFromBits(bits: Bits): Unit


  def isEguals(that: Data): Bool// = (this.flatten, that.flatten).zipped.map((a, b) => a.isEguals(b)).reduceLeft(_ && _)
  def isNotEguals(that: Data): Bool// = (this.flatten, that.flatten).zipped.map((a, b) => a.isNotEguals(b)).reduceLeft(_ || _)
  def autoConnect(that: Data): Unit// = (this.flatten, that.flatten).zipped.foreach(_ autoConnect _)
  def autoConnectBaseImpl(that: Data): Unit = {
    if (this.component == that.component) {
      if (this.component == Component.current) {
        sameFromInside
      } else if (this.component.parent == Component.current) {
        sameFromOutside
      } else SpinalError("You cant autoconnect from here")
    } else if (this.component.parent == that.component.parent) {
      kindAndKind
    } else if (this.component == that.component.parent) {
      parentAndKind(this, that)
    } else if (this.component.parent == that.component) {
      parentAndKind(that, this)
    } else SpinalError("Don't know how autoconnect")

    def sameFromOutside: Unit = {
      if (this.isOutput && that.isInput) {
        that := this
      } else if (this.isInput && that.isOutput) {
        this assignFrom (that,false)
      } else SpinalError("Bad input output specification for autoconnect")
    }
    def sameFromInside: Unit = {
      if (that.isOutputDir && this.isInputDir) {
        that := this
      } else if (that.isInputDir && this.isOutputDir) {
        this assignFrom (that,false)
      } else SpinalError("Bad input output specification for autoconnect")
    }

    def kindAndKind: Unit = {
      if (this.isOutput && that.isInput) {
        that := this
      } else if (this.isInput && that.isOutput) {
        this assignFrom (that,false)
      } else SpinalError("Bad input output specification for autoconnect")
    }

    def parentAndKind(p: Data, k: Data): Unit = {
      if (k.isOutput) {
        p := k
      } else if (k.isInput) {
        k := p
      } else SpinalError("Bad input output specification for autoconnect")
    }
  }


  def getBitsWidth: Int

  def keep: this.type = {
    flatten.foreach(t => t.component.additionalNodesRoot += t);
    dontSimplifyIt
    this
  }

  def dontSimplifyIt: this.type = {
    flatten.foreach(_.dontSimplifyIt)
    this
  }
  def allowSimplifyIt: this.type = {
    flatten.foreach(_.allowSimplifyIt)
    this
  }

  override def add(attribute: Attribute): Unit = {
    flatten.foreach(_.add(attribute))
  }

  def isReg: Boolean = flatten.foldLeft(true)(_ && _.isReg)

  /*private[core] */
  def initImpl(init: Data): this.type = {
    // if (!isReg) SpinalError(s"Try to set initial value of a data that is not a register ($this)")
    val regInit = clone()
    regInit := init
    for ((e, initElement) <- (this.flatten, regInit.flatten).zipped) {
      def recursiveSearch(ptr: Node): Unit = {
        if (ptr.component != init.component) SpinalError(s"Try to set initial value of a data that is not in current component ($this)")
        ptr match {
          case bt: BaseType => {
            if (bt.isReg)
              bt.inputs(0).asInstanceOf[Reg].setInitialValue(initElement)
            else
              recursiveSearch(bt)
          }
          case _ => SpinalError(s"Try to set initial value of a data that is not a register ($this)")
        }
      }

      //maybe need to restor commented ?
      if (initElement.inputs(0) != null /* && initElement.inputs(0).inputs(0) != null*/ ) {
        recursiveSearch(e)
      }
    }
    this
  }

  /*private[core] */
  //  def next(next: SSelf): this.type = {
  //    if (!isReg) SpinalError(s"Try to set next value of a data that is not a register ($this)")
  //    this := next
  //    this
  //  }


  def randBoot(): this.type = {
    flatten.foreach(_.addTag(spinal.core.randomBoot))
    this
  }


  override def clone(): this.type = {
    try {
      val clazz = this.getClass
      val constructor = clazz.getConstructors.head
      val constrParamCount = constructor.getParameterTypes.size
      //No param =>
      if (constrParamCount == 0) return constructor.newInstance().asInstanceOf[this.type]

      def constructorParamsAreVal: this.type = {
        val outer = clazz.getFields.find(_.getName == "$outer")
        val constructor = clazz.getDeclaredConstructors.head
        val argumentCount = constructor.getParameterTypes.size - (if (outer.isDefined) 1 else 0)
        val fields = clazz.getDeclaredFields
        val arguments = (0 until argumentCount) map { i =>
          val fieldName = fields(i).getName
          val getter = clazz.getMethod(fieldName)
          getter.invoke(this)
        }
        if (outer.isEmpty)
          return constructor.newInstance(arguments: _*).asInstanceOf[this.type]
        else {
          val args = (outer.get.get(this) :: Nil) ++ arguments
          return constructor.newInstance(args: _*).asInstanceOf[this.type]
        }
      }
      //Case class =>
      if (ScalaUniverse.isCaseClass(this)) {
        return constructorParamsAreVal
      }

      //Inner class with no user parameters
      if (constrParamCount == 1) {
        val outer = clazz.getFields.find(_.getName == "$outer")
        if (outer.isDefined) {
          return constructor.newInstance(outer.get.get(this)).asInstanceOf[this.type]
        }
      }

      if (clazz.getAnnotations.find(_.isInstanceOf[valClone]).isDefined) {
        return constructorParamsAreVal
      }

      needCloneImpl()

    } catch {
      case npe: java.lang.reflect.InvocationTargetException if npe.getCause.isInstanceOf[java.lang.NullPointerException] =>
        needCloneImpl()
      case e: java.lang.Exception =>
        needCloneImpl()
    }

    def needCloneImpl(): this.type = {
      SpinalError(
        """
          |*** Spinal can't clone one of your datatype
          |*** You have two way to solve that :
          |*** In place to declare a "class Bundle(args){}", create a "case class Bundle(args){}"
          |*** Or override by your self the bundle clone function
          |*** The error is """.stripMargin + this.getScalaLocationString);
      null
    }
    null
  }

}


//
//abstract class CustomData extends Data{
//  override def getBitsWidth: Int = flatten.map(_.getBitsWidth).reduce(_ + _)
//  override def assignFromBits(bits: Bits): Unit = {
//    var offset = 0
//    for(e <- flatten){
//      e.assignFromBits(bits(e.getBitsWidth + offset-1,offset))
//      offset += e.getBitsWidth
//    }
//  }
//  override def toBits: Bits = Cat(flatten.reverse.map(_.toBits))
//  override private[spinal] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = {
//    assert(!conservative)
//    assert(that.getClass == this.getClass)
//    val t = that.asInstanceOf[CustomData]
//    (this.flatten,t.flatten).zipped.map(_.assignFromImpl(_,conservative))
//  }
//}
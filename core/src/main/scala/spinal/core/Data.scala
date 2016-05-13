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
//import scala.reflect.runtime.{universe => ru}


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
      from.asInput()
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
        from.asOutput()
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

//Should not extends AnyVal, Because it create kind of strange call stack move that make error reporting miss accurate
class DataPimper[T <: Data](val pimpIt: T){
  def ===(that: T): Bool = pimpIt.isEguals(that)
  def =/=(that: T): Bool = pimpIt.isNotEguals(that)
  @deprecated("Use =/= instead")
  def !==(that: T): Bool = this =/= that



  def := (that: T): Unit = {
    if(that.isInstanceOf[BitVector])
      pimpIt.asInstanceOf[BitVector] := that.asInstanceOf[BitVector]
    else
      pimpIt assignFrom(that, false)
  }


//  def := [T2 <: T](that: T2): Unit = pimpIt assignFrom(that, false)

  //Use as \= to have the same behavioral than VHDL variable
  def \(that: T) : T = {
    val ret = cloneOf(that)
    ret := pimpIt
    ret.flatten.foreach(_.conditionalAssignScope = pimpIt.conditionalAssignScope)
    ret.globalData.overridingAssignementWarnings = false
    ret := that
    ret.globalData.overridingAssignementWarnings = true
    ret
  }

  def <>(that: T): Unit = pimpIt autoConnect that
  def init(that: T): T = pimpIt.initImpl(that)
  def default(that : => T) : T ={
    val c = if(pimpIt.dir == in)
      Component.current.parent
    else
      Component.current

    Component.push(c)
    pimpIt.defaultImpl(that)
    Component.pop(c)
    pimpIt
  }

  def mapList[T <: Data](mappings: Seq[(Any, T)]): T = {
    SpinalMap.list(pimpIt,mappings)
  }
  def map[T <: Data](mappings: (Any, T)*): T = {
    SpinalMap.list(pimpIt,mappings)
  }
}

abstract class WidthChecker(val consumer : Node,val provider : Node) {
  consumer.globalData.widthCheckers += this

  def check() : String = {
    if(consumer.inferredWidth == -1 || provider.inferredWidth == -1) return null
    def isLiteralBitWidthUnspecified(n : Node) : Boolean = {
      n match{
        case n : BitsLiteral => return ! n.hasSpecifiedBitCount
        case n : BitVector => return if(n.isFixedWidth) false else isLiteralBitWidthUnspecified(n.inputs(0))
        case _ => return false
      }
    }
    if(isLiteralBitWidthUnspecified(provider)) return null
    if(provider.hasTag(tagAutoResize)) return null
    return checkImpl()
  }
  def checkImpl() : String = null
}
class WidthCheckerReduce(consumer : Node,provider : Node) extends WidthChecker(consumer,provider){
//  def checkImpl() : String = {
//    if( consumer.getWidth <= provider.getWidth ) return null
//    return ":< assignement error ! Bit width assemption is wrong"
//  }
}
class WidthCheckerAugment(consumer : Node,provider : Node) extends WidthChecker(consumer,provider){
//  def checkImpl() : String = {
//    if( consumer.getWidth >= provider.getWidth ) return null
//    return ":> assignement error ! Bit width assemption is wrong"
//  }
}
class WidthCheckerEguals(consumer : Node,provider : Node) extends WidthChecker(consumer,provider){
//  def checkImpl() : String = {
//    if( consumer.getWidth == provider.getWidth ) return null
//    return " := assignement error ! Bit width assemption is wrong"
//  }
}

//Should not extends AnyVal, Because it create kind of strange call stack move that make error reporting miss accurate
class BitVectorPimper[T <: BitVector](val pimpIt: T)  {
  def :=(that: T): Unit ={
  //  new WidthCheckerEguals(pimpIt,that)
    pimpIt assignFrom(that, false)
  }

  @deprecated
  def :<=(that: T): Unit = {
  //  new WidthCheckerReduce(pimpIt,that)
    pimpIt assignFrom(that.resized, false)
  }

  @deprecated
  def :>=(that: T): Unit = {
  //  new WidthCheckerAugment(pimpIt,that)
    pimpIt assignFrom(that.resized, false)
  }

  def :~=(that: T): Unit = pimpIt assignFrom(that.resized, false)
}

trait Data extends ContextUser with NameableByComponent with Assignable with AttributeReady with SpinalTagReady with GlobalDataUser with ScalaLocated {
  private[core] var dir: IODirection = null
  private[core] def isIo = dir != null

  var parent : Data = null
  def getRootParent : Data = if(parent == null) this else parent.getRootParent

  def asInput(): this.type = {
    dir = in;
    this
  }
  def asOutput(): this.type = {
    dir = out;
    this
  }

 // def assignDontCare() : Unit = ???

  def isOutput: Boolean = dir == out
  def isInput: Boolean = dir == in
  def isDirectionLess: Boolean = dir == null
  def flip(): this.type  = {
    dir match {
      case `in` => dir = out
      case `out` => dir = in
      case _ => SpinalError(s"Can't flip a data that is direction less $this")
    }
    this
  }

  def asData = this.asInstanceOf[Data]
  def getZero: this.type

  def flatten: Seq[BaseType]
  def flattenLocalName: Seq[String]

  def pull(): this.type = Data.doPull(this, Component.current, false, false)

  //  def :-(that: => SSelf): this.type = {
  //    val task = () => {
  //      this := that
  //    }
  //    component.postCreationTask += task
  //
  //    this
  //  }

  def ##(right: Data): Bits = this.asBits ## right.asBits

  def asBits: Bits
  def assignFromBits(bits: Bits): Unit
  def assignFromBits(bits: Bits,hi : Int,low : Int): Unit
  def assignFromBits(bits: Bits,offset: Int, bitCount: BitCount): Unit = this.assignFromBits(bits,offset + bitCount.value -1,offset)
  def assignDontCare() : this.type = {
    flatten.foreach(_.assignDontCare())
    this
  }

  private[core] def isEguals(that: Any): Bool// = (this.flatten, that.flatten).zipped.map((a, b) => a.isEguals(b)).reduceLeft(_ && _)
  private[core] def isNotEguals(that: Any): Bool// = (this.flatten, that.flatten).zipped.map((a, b) => a.isNotEguals(b)).reduceLeft(_ || _)
  @deprecated("Use resized instead")
  def autoResize() : this.type = this.resized
  def resized : this.type ={
    val ret = this.clone
    ret.assignFrom(this,false)
    ret.addTag(tagAutoResize)
    return ret
  }

  private[core] def autoConnect(that: Data): Unit// = (this.flatten, that.flatten).zipped.foreach(_ autoConnect _)
  private[core] def autoConnectBaseImpl(that: Data): Unit = {
    def error(message : String) = SpinalError(message + "\n" + this + "\n" + that)
    if (this.component == that.component) {
      if (this.component == Component.current) {
        sameFromInside
      } else if (this.component.parent == Component.current) {
        sameFromOutside
      } else error("You cant autoconnect from here")
    } else if (this.component.parent == that.component.parent) {
      kindAndKind
    } else if (this.component == that.component.parent) {
      parentAndKind(this, that)
    } else if (this.component.parent == that.component) {
      parentAndKind(that, this)
    } else error("Don't know how autoconnect")


    def sameFromOutside: Unit = {
      if (this.isOutput && that.isInput) {
        that := this
      } else if (this.isInput && that.isOutput) {
        this := that
      } else error("Bad input output specification for autoconnect")
    }
    def sameFromInside: Unit = {
      (this.dir,that.dir) match {
        case (`out`,`in`) => this := that
        case (`out`,null) => this := that
        case (null,`in`) => this := that
        case (`in`,`out`) => that := this
        case (`in`,null) => that := this
        case (null,`out`) => that := this
        case _ =>  error("Bad input output specification for autoconnect")
      }
    }

    def kindAndKind: Unit = {
      if (this.isOutput && that.isInput) {
        that := this
      } else if (this.isInput && that.isOutput) {
        this := that
      } else error("Bad input output specification for autoconnect")
    }

    def parentAndKind(p: Data, k: Data): Unit = {
      if (k.isOutput) {
        p := k
      } else if (k.isInput) {
        k := p
      } else error("Bad input output specification for autoconnect")
    }
  }

  def getBitsWidth: Int

  def keep(): this.type = {
    flatten.foreach(t => t.component.additionalNodesRoot += t);
    dontSimplifyIt()
    this
  }

  def dontSimplifyIt(): this.type = {
    flatten.foreach(_.dontSimplifyIt())
    this
  }
  def allowSimplifyIt(): this.type = {
    flatten.foreach(_.allowSimplifyIt())
    this
  }

  override def add(attribute: Attribute): this.type = {
    flatten.foreach(_.add(attribute))
    this
  }

  def isReg: Boolean = flatten.foldLeft(true)(_ && _.isReg)


  /*private[core] */
  private[core] def initImpl(init: Data): this.type = {
    // if (!isReg) SpinalError(s"Try to set initial value of a data that is not a register ($this)")
    val regInit = clone()
    regInit := init
    for ((e, initElement) <- (this.flatten, regInit.flatten).zipped) {
      def recursiveSearch(ptr: Node): Unit = {
        //if (ptr.component != init.component) SpinalError(s"Try to set initial value of a data that is not in current component ($this)")
        ptr match {
          case bt: BaseType => {
            if (bt.isReg)
              bt.inputs(0).asInstanceOf[Reg].setInitialValue(initElement)
            else
              recursiveSearch(ptr.inputs(0))
          }
          case _ => SpinalError(s"Try to set initial value of a data that is not a register ($this)")
        }
      }

      //maybe need to restor commented ?
      //if (initElement.inputs(0) != null /* && initElement.inputs(0).inputs(0) != null*/ ) {
      recursiveSearch(e)
     // }
    }
    this
  }

  private[core] def defaultImpl(init: Data): this.type = {
    // if (!isReg) SpinalError(s"Try to set initial value of a data that is not a register ($this)")
    val regInit = clone()
    regInit := init
    for ((e, initElement) <- (this.flatten, regInit.flatten).zipped) {
      e.defaultValue = initElement
    }
    this
  }

  /*private[core] */
  //  def next(next: SSelf): this.type = {
  //    if (!isReg) SpinalError(s"Try to set next value of a data that is not a register ($this)")
  //    this := next
  //    this
  //  }

  //Usefull for register that doesn't need a reset value in RTL, but need a randome value for simulation (avoid x-propagation)
  def randBoot(): this.type = {
    flatten.foreach(_.addTag(spinal.core.randomBoot))
    this
  }


  override def clone: this.type = {
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
          val arg = getter.invoke(this)
          if(arg.isInstanceOf[Data]){
            arg.asInstanceOf[Data].clone
          } else{
            arg
          }
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
        var outerField = clazz.getFields.find(_.getName == "$outer")
        if(!outerField.isDefined) outerField = clazz.getDeclaredFields.find(_.getName == "$outer")
        if(outerField.isDefined){
          val outer = outerField.get
          outer.setAccessible(true)
          return constructor.newInstance(outer.get(this)).asInstanceOf[this.type]
        }
        val c = clazz.getMethod("getComponent").invoke(this).asInstanceOf[Component]
        val pt = constructor.getParameterTypes.apply(0)
        if(c.getClass.isAssignableFrom(pt)){
          return constructor.newInstance(c).asInstanceOf[this.type]
        }
//        val a = c.areaClassSet.get(pt)
//        if(a.isDefined && a.get != null){
//          return constructor.newInstance(a.get).asInstanceOf[this.type]
//        }
      }


//      if (clazz.getAnnotations.find(_.isInstanceOf[valClone]).isDefined) {
//        return constructorParamsAreVal
//      }

      needCloneImpl()

    } catch {
      case npe: java.lang.reflect.InvocationTargetException if npe.getCause.isInstanceOf[java.lang.NullPointerException] =>
        needCloneImpl()
      case e: java.lang.Exception =>
        needCloneImpl()
    }

    def needCloneImpl(): this.type = {
      SpinalError(
        s"""
          |*** Spinal can't clone ${this.getClass} datatype
          |*** You have two way to solve that :
          |*** In place to declare a "class Bundle(args){}", create a "case class Bundle(args){}"
          |*** Or override by your self the bundle clone function
          |*** The error is """.stripMargin + this.getScalaLocationString);
      null
    }
    null
  }

  override def getComponent(): Component = component
  def getComponents() : Seq[Component] = (component.parents() ++ Seq(component))

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
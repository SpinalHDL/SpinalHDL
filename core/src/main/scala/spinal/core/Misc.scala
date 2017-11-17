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

import spinal.core.internals._


import java.lang.reflect.Field

import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.reflect.ClassTag
import scala.runtime.Nothing$

/** Use to give value by reference to a function */
case class Ref[T](var value: T)

//case class valClone() extends scala.annotation.StaticAnnotation

//Give number of bit to encode a given number of states
object log2Up {
  def apply(value: BigInt): Int = {
    if (value < 0) SpinalError(s"No negative value ($value) on ${this.getClass.getSimpleName}")
    (value - 1).bitLength
  }
}

object isPow2 {
  def apply(that: BigInt): Boolean = {
    if (that < 0) return false
    return that.bitCount == 1
  }
}

object roundUp {
  def apply(that: BigInt, by : BigInt): BigInt = {
    return (that / by) * by + (if(that % by != 0) by else 0)
  }
}

object cloneOf {
  //Return a new data with the same data structure than the given parameter (including bit width)
  def apply[T <: Data](that: T): T = that.clone().asInstanceOf[T]
  def apply[T <: Data](that: HardType[T]): T = that()
}

object weakCloneOf {
  //Return a new data with the same data structure than the given parameter (execept bit width)
  def apply[T <: Data](that: T): T = {
    val ret = cloneOf(that)
    ret.flatten.foreach(_ match {
      case bv : BitVector => bv.unfixWidth()
      case _ =>
    })
    ret
  }
}

object widthOf {
//  Return the number of bit of the given data
  def apply[T <: Data](that: T): Int = that.getBitsWidth
}

object HardType{
  implicit def implFactory[T <: Data](t : T) = new HardType(t)
}

class HardType[T <: Data](t : T){
  def apply() = cloneOf(t)
}

object signalCache {
  def apply[T <: Data](key: Object, subKey: Object, factory: () => T): T = {
    val cache = Component.current.userCache.getOrElseUpdate(key, scala.collection.mutable.Map[Object, Object]())
    cache.getOrElseUpdate(subKey, factory()).asInstanceOf[T]
  }
}

object Cat {
  def apply(data: Data*): Bits = apply(data.toList.reverse)

  def apply[T <: Data](data: Iterable[T]) = {
    if (data.isEmpty) B(0, 0 bit)
    else data.map(_.asBits).reduce((a,b) => b ## a)
  }
}


object Mux {
  def apply[T <: Data](sel: Bool, whenTrue: T, whenFalse: T): T = {
    Multiplex.complexData(sel, whenTrue, whenFalse)
  }
  def apply[T <: SpinalEnum](sel: Bool, whenTrue: SpinalEnumElement[T], whenFalse: SpinalEnumElement[T]): SpinalEnumCraft[T] = {
    Multiplex.complexData(sel, whenTrue(), whenFalse())
  }
  def apply[T <: SpinalEnum](sel: Bool, whenTrue: SpinalEnumCraft[T], whenFalse: SpinalEnumElement[T]): SpinalEnumCraft[T] = {
    Multiplex.complexData(sel, whenTrue, whenFalse())
  }
  def apply[T <: SpinalEnum](sel: Bool, whenTrue: SpinalEnumElement[T], whenFalse: SpinalEnumCraft[T]): SpinalEnumCraft[T] = {
    Multiplex.complexData(sel, whenTrue(), whenFalse)
  }
}

object Sel{
  def apply[T <: Data](default : T,mappings : (Bool,T)*) :T = seq(default,mappings)
  def seq[T <: Data](default : T,mappings : Seq[(Bool,T)]): T ={
    val result = cloneOf(default)
    result := default
    for((cond,value) <- mappings.reverseIterator){
      when(cond){
        result := value
      }
    }
    result
  }
}

object Analog{
  def apply[T <: Data](that : HardType[T]) : T = that().setAsAnalog()
}

object SpinalMap {
  def apply[K <: BaseType, T <: Data](addr: K, mappings: (Any, T)*): T = list(addr,mappings)

  def list[K <: BaseType, T <: Data](addr: K, mappings: Seq[(Any, T)]): T = {
    val result : T = weakCloneOf(mappings.head._2)

    switch(addr){
      for ((cond, value) <- mappings) {
        cond match {
          case product: Product => {
            is.list(product.productIterator) {
              result := value
            }
          }
          case `default` => {
            default {
              result := value
            }
          }
          case _ => {
            is(cond) {
              result := value
            }
          }
        }
      }
    }
    result
  }
}

//TODO DOC
object Select{
  def apply[T <: Data](default: T, mappings: (Bool, T)*): T = list(default,mappings)
  def apply[T <: Data](mappings: (Any, T)*): T = list(mappings)

  def list[ T <: Data]( defaultValue: T, mappings: Seq[(Bool, T)]): T = {
    val result : T = cloneOf(defaultValue)

    var ptr : WhenContext = null

    mappings.foreach{case (cond,that) => {
      if(ptr == null){
        ptr = when(cond){
          result := that
        }
      }else{
        ptr = ptr.elsewhen(cond){
          result := that
        }
      }
    }}

    if(ptr == null){
      result := defaultValue
    }else{
      ptr.otherwise{
        result := defaultValue
      }
    }
    result
  }

  def list[T <: Data](mappings: Seq[(Any, T)]): T = {
    val defaultValue = mappings.find(_._1 == default)
    if(!defaultValue.isDefined) new Exception("No default element in SpinalMap (default -> xxx)")
    val filterd = mappings.filter(_._1 != default).map(t => (t._1.asInstanceOf[Bool] -> t._2))
    list(defaultValue.get._2,filterd)
  }
}


trait AssertNodeSeverity
object NOTE     extends AssertNodeSeverity
object WARNING  extends AssertNodeSeverity
object ERROR    extends AssertNodeSeverity
object FAILURE  extends AssertNodeSeverity

@deprecated("Use cloneable instead")
object wrap{
  def apply[T <: Bundle](that : => T) : T = {
    val ret : T = that
    ret.cloneFunc = (() => that)
    ret
  }
}
object cloneable{
  def apply[T <: Bundle](that : => T) : T = {
    val ret : T = that
    ret.cloneFunc = (() => that)
    ret
  }
}

class NamingScope(parent : NamingScope = null) {
  var lock = false
  val map = mutable.Map[String, Int]()
  def allocateName(name: String): String = {
    assert(!lock)
    val lowerCase = name.toLowerCase
    val count = map.get(lowerCase).getOrElse(0)
    map(lowerCase) = count + 1
    val finalCount =  (count + (if(parent != null) parent.map.get(lowerCase).getOrElse(0) else 0))
    if (finalCount == 0) name else name + "_" + finalCount
  }

  def getUnusedName(name: String): String = {
    val lowerCase = name.toLowerCase
    val count = (map.get(lowerCase).getOrElse(0) + (if(parent != null) parent.map.get(lowerCase).getOrElse(0) else 0))
    if (count == 0) name else name + "_" + count
  }


  def lockName(name: String): Unit = {
    assert(!lock)
    val lowerCase = name.toLowerCase
    val count = map.get(lowerCase).getOrElse(1)
    map(lowerCase) = count
  }

  def iWantIt(name: String,errorMessage : => String): Unit = {
    assert(!lock)
    val lowerCase = name.toLowerCase
    if (map.contains(lowerCase) ||  (parent != null && parent.map.contains(lowerCase)))
      PendingError(errorMessage)
    map(lowerCase) = 1
  }

  def lockScope(): Unit ={
    this.lock = true
  }

  def newChild = new NamingScope(this)
//  def copy() : Scope = {
//    val cpy = new Scope
//    map.foreach{case (n,i) => cpy.map.put(n,i)}
//    cpy
//  }
}

/*
class UniqueNameAllocator {
  val map = mutable.Map[String, Int]()


  def get(obj: Object): String = {
    val clazz = obj.getClass()
    val name = clazz.getSimpleName
    val count = map.get(name).getOrElse(0)
    map(name) = count + 1
    if (count == 0) clazz.getSimpleName else clazz.getSimpleName + "_" + count
  }


}*/

trait Stackable{
  def postPushEvent() = {}
  def postPopEvent() = {}
  def prePopEvent() = {}
}

class SafeStackWithStackable[T <: Stackable] extends SafeStack[T]{
  override def push(e: T): Unit = {
    super.push(e)
    if(e != null) e.postPushEvent()
  }

  override def pop(e: T): Unit = {
    if(e != null) e.prePopEvent()
    super.pop(e)
    if(e != null) e.postPopEvent()
  }
}

class SafeStack[T] {
  val stack = new Stack[T]()

  def push(e: T): Unit = {
    stack.push(e)
  }

  def pop(e: T): Unit = {
    if (stack.head != e)
      throw new Exception("Stack pop fail")
    stack.pop()
  }

  def head() = stack.headOption.getOrElse(null.asInstanceOf[T])

  def oldest() = stack.lastOption.getOrElse(null.asInstanceOf[T])

  def isEmpty: Boolean = stack.isEmpty

  def size() = stack.size

  def reset = stack.clear()
}

object SpinalExit {
  def apply(message: String = "") = {
    throw new SpinalExit("\n" + message)
  }
  val errorsMessagesSeparator = "*" * 120 + "\n" + "*" * 120
}
object SpinalLog{
  def tag(name: String, color: String): String =
    if (System.console != null)
      s"[${color}${name}${Console.RESET}]"
    else
      s"[${name}]"
}

object SpinalProgress {
  def apply(message: String) = println(s"${SpinalLog.tag("Progress", Console.BLUE)} at ${f"${Driver.executionTime}%1.3f"} : $message")
}

object SpinalInfo {
  def apply(message: String) = println(s"${SpinalLog.tag("Info", Console.BLUE)} $message")
}

object SpinalWarning {
  def apply(message: String) = println(s"${SpinalLog.tag("Warning", Console.YELLOW)} $message")
}

class SpinalExit(message: String) extends Exception("\n\n" + (Seq(message)++ GlobalData.get.pendingErrors.map(_.apply())).map(_ + "\n" + SpinalExit.errorsMessagesSeparator + "\n\n").mkString("") + "Design's errors are listed above.\nSpinalHDL compiler exit stack : \n");

object PendingError {
  def apply(error : => String) : Unit = {
    GlobalData.get.pendingErrors += (() => error)
  }
}
object LocatedPendingError {
  def apply(error : => String) = {
    val location = ScalaLocated.long
    GlobalData.get.pendingErrors += (() => error + "\n" + location)
  }
}

object SpinalError {
  private var errCount:Int = 0

  def apply() = {
    SpinalExit()
  }

  def apply(message: String) = {
    errCount += 1
    SpinalExit(message)
  }

  def apply(messages: Seq[String]) = {
    errCount += messages.length
    SpinalExit(messages.reduceLeft(_ + "\n\n" + _))
  }

  def printError(message: String) = println(s"${SpinalLog.tag("Progress", Console.RED)} $message")

  def getErrorCount():Int = {
    val ret = errCount + GlobalData.get.pendingErrors.length
    errCount = 0
    return ret
  }
}



object ifGen {
  def apply[T](cond: Boolean)(block: => T): T = {
    if (cond)
      return block
    else
      return null.asInstanceOf[T]
  }
}

object MaskedLiteral{
  def apply(str : String) : MaskedLiteral = {
    val strCleaned = str.replace("_","")
    for(c <- strCleaned) assert(c == '1' || c == '0' || c == '-', s"""M"$str" is not correctly formated.""")
    val careAbout = strCleaned.map(c => if(c == '-') '0' else '1')
    val value = strCleaned.map(c => if(c == '-') '0' else c)
    new MaskedLiteral(BigInt(value,2),BigInt(careAbout,2),strCleaned.length())
  }
}

class MaskedLiteral(val value: BigInt, val careAbout: BigInt, val width: Int){

  def ===(that: BitVector): Bool = {
    if(that.getWidth != width){
      SpinalError(s"Masked literal width=$width doesn't match the one of $that")
    }
    return (that.asBits & careAbout) === value
  }

  def =/=(that: BitVector): Bool = !(this === that)

  override def toString(): String = {

    def bigInt2ListBoolean(value: BigInt, size: BitCount): List[Boolean] = {
      def bigInt2ListBool(that: BigInt): List[Boolean] = {
        if(that == 0)  Nil
        else List(that.testBit(0)) ::: bigInt2ListBool(that >> 1)
      }

      castListBool(bigInt2ListBool(value), size.value)
    }

    def castListBool(l: List[Boolean], size: Int): List[Boolean] = {
      if (l.length == size)     l
      else if (l.length > size) l.drop( l.length - size)
      else                      l ::: List.fill(size - l.length)(false)
    }

    val valueList = bigInt2ListBoolean(this.value, this.width bits)
    val careList  = bigInt2ListBoolean(this.careAbout, this.width bits)

    val maskStr = careList.zip(valueList).map{ x => x match{
      case (false, x)    => "-"
      case (true, true)  => "1"
      case (true, false) => "0"
    }
    }.reverse.mkString("")

    "M\"" + maskStr + "\""
  }
}


object ArrayManager{
  def setAllocate[T](array : Array[T],idx : Int,value : T,initialSize : Int = 4)(implicit m: ClassTag[T]) : Array[T] = {
    var ret = array
    if(ret == null) ret = new Array[T](initialSize)
    if(ret.length <= idx){
      val cpy = new Array[T](idx << 1)
      ret.copyToArray(cpy)
      ret = cpy
    }
    ret(idx) = value
    ret
  }

  def getElseNull[T](array : Array[T],idx : Int)(implicit m: ClassTag[T]) : T = {
    if(array == null) return null.asInstanceOf[T]
    if(array.length <= idx) return null.asInstanceOf[T]
    return array(idx)
  }
}


object AnnotationUtils{
  def isDontName(f: Field): Boolean = {
    return f.isAnnotationPresent(classOf[dontName])
  }
}
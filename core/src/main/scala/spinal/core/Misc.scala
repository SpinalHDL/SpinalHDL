/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core

import spinal.core.fiber.AsyncThread
import spinal.core.internals._

import java.lang.reflect.Field
import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.reflect.ClassTag
import scala.runtime.Nothing$
import scala.collection.Seq



/**
 * Use to give value by reference to a function
 */
case class Ref[T](var value: T)


/**
 * Give number of bit to encode a given number of states
 */
object log2Up {
  def apply(value: BigInt): Int = {
    if (value < 0) SpinalError(s"No negative value ($value) on ${this.getClass.getSimpleName}")
    (value - 1).bitLength
  }
  def apply(value : Int) : Int = apply(BigInt(value))
}

object Gray {
  /** Encoding binary number in binary gray code */
  def encode(binary: BigInt): BigInt = binary ^ (binary >> 1)

  /** Decode binary gray encoded number to binary */
  def decode(gray: BigInt): BigInt = {
    var binary = BigInt(0)
    var bits = gray
    while (bits > 0) {
      binary ^= bits
      bits >>= 1
    }
    binary
  }
}


/**
 * Check if a number is a power of 2 
 */
object isPow2 {
  def apply(that: BigInt): Boolean = {
    if (that < 0) return false
    that.bitCount == 1
  }
  def apply(that : Int) : Boolean = apply(BigInt(that))
}


/**
 * Round up a BigInt 
 */
object roundUp {
  def apply(that: BigInt, by: BigInt): BigInt = (that / by) * by + (if(that % by != 0) by else 0)

}


/**
 * Return a new data with the same data structure as the given parameter (including bit width) 
 */
object cloneOf {  
  def apply[T <: Data](that: T): T = that.clone().asInstanceOf[T]
  def apply[T <: Data](that: HardType[T]): T = that()
}


/**
 * Return a new data with the same data structure as the given parameter (except bit width)
 */
object weakCloneOf {
  def apply[T <: Data](that: T): T = {
    val ret = that match {
      case that : BitVector => that.weakClone.asInstanceOf[T]
      case _ => cloneOf(that)
    }

    ret.flattenForeach{
      case bv: BitVector => bv.unfixWidth()
      case _             =>
    }
    ret
  }
}


/**
 * Return the number of bit of the given data
 */
object widthOf {
  def apply[T <: Data](that: T): Int = that.getBitsWidth
  def apply(maskedLiteral: MaskedLiteral): Int = maskedLiteral.getWidth()
  def apply[T <: Data](that: HardType[T]): Int = that.getBitsWidth
}



object HardType{
  implicit def implFactory[T <: Data](t : => T): HardType[T] = new HardType(t)
  def apply[T <: Data](t : => T) = new HardType(t)

  def union(elements: Data*): HardType[Bits] = {
    val width = elements.map(widthOf(_)).max
    HardType(Bits(width bits))
  }
}

class HardType[T <: Data](t : => T) extends OverridedEqualsHashCode{
  def apply()   = {
    val id = GlobalData.get.instanceCounter
    val called = t
    called.flattenForeach{
      case w : BitVector if w.isFixedWidth || !w.dlcIsEmpty => w.fixWidth()
      case _ =>
    }
    val ret = if(called.getInstanceCounter < id || called.component != Component.current) cloneOf(called) else called.purify()
    ret match {
      case ret : Bundle => ret.hardtype = this
      case _ =>
    }
    ret
  }
  def craft() = apply()
  def getBitsWidth = t.getBitsWidth
}


object NamedType{
  def apply[T <: Data](gen : => T) = new NamedType(gen)
  def apply[T <: Data](gen : HardType[T]) = new NamedType(gen.craft())
}

class NamedType[T <: Data](gen : => T) extends HardType(gen) with Nameable


object signalCache{
  def apply[T](key: Any)(factory: => T): T = {
    Component.current.userCache.getOrElseUpdate(key, factory).asInstanceOf[T]
  }
  def apply[T](key: Any, subKey: Any)(factory: => T): T = {
    apply((key, subKey))(factory)
  }
}

object globalCache{
  def apply[T](key: Any)(factory: => T): T = {
    GlobalData.get.userDatabase.getOrElseUpdate(key, factory).asInstanceOf[T]
  }
}



/**
 * Concatenate a list of data 
 */
object Cat {
  def apply(data: Data*): Bits = apply(data.toList.reverse)
  def apply[T <: Data](data: Vec[T]): Bits = data.asBits

  def apply[T <: Data](data: Iterable[T]) = {
    if (data.isEmpty) B(0, 0 bit)
    else data.map(_.asBits).reduce((a,b) => b ## a)
  }
}


/**
 * Mux operation 
 */
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


/**
 * Set a data to Analog
 */
object Analog{
  def apply[T <: Data](that: HardType[T]): T = that().setAsAnalog()
}


/**
  * Spinal map
  */
object SpinalMap {
  def apply[K <: BaseType, T <: Data](addr: K, mappings: (Any, T)*): T = list(addr, mappings)

  def list[K <: BaseType, T <: Data](addr: K, mappings: Seq[(Any, T)]): T = {
    val result: T = weakCloneOf(mappings.head._2)

//    if(!mappings.contains(default) && addr.isInstanceOf[BitVector] && BigInt(1) << addr.getBitsWidth == mappings.size){
//      Vec(mappings.map(_._2)).read(addr.asBits.asUInt)
//    }

    switch(addr){
      for ((cond, value) <- mappings) {
        cond match {
          case product: Product => 
            is.list(product.productIterator) {
              result := value
            }        
          case `default` => 
            default {
              result := value
            }
          case _ => 
            is(cond) {
              result := value
            }
        }
      }
    }
    result
  }

  def listDc[K <: BaseType, T <: Data](addr: K, mappings: Seq[(Any, T)]): T = {
    val result: T = cloneOf(mappings.head._2).assignDontCare()

    switch(addr){
      for ((cond, value) <- mappings) {
        cond match {
          case product: Product =>
            is.list(product.productIterator) {
              result := value
            }
          case `default` => ???
          case _ =>
            is(cond) {
              result := value
            }
        }
      }
    }
    result
  }
}


/**
  * Sel operation
  */
@deprecated("Use Select instead", "???")
object Sel{
  @deprecated("Use Select instead", "???")
  def apply[T <: Data](default: T, mappings: (Bool, T)*):T = seq(default,mappings)

  @deprecated("Use Select instead", "???")
  def seq[T <: Data](default: T, mappings: Seq[(Bool, T)]): T = {
    val result = cloneOf(default)
    result := default
    for((cond, value) <- mappings.reverseIterator){
      when(cond){
        result := value
      }
    }
    result
  }
}

//TODO DOC
object Select{
  def apply[T <: Data](default: T, mappings: (Bool, T)*): T = list(default, mappings)

  def apply[T <: Data](mappings: (Any, T)*): T = list(mappings)

  def list[ T <: Data](defaultValue: T, mappings: Seq[(Bool, T)]): T = {
    val result = cloneOf(defaultValue)
    result := defaultValue
    for((cond, value) <- mappings.reverseIterator){
      when(cond){
        result := value
      }
    }
    result
  }

  def list[T <: Data](mappings: Seq[(Any, T)]): T = {
    val defaultValue = mappings.find(_._1 == default)
    if(defaultValue.isEmpty) new Exception("MISSING DEFAULT in Select. Select(default -> xxx, ...)")
    val filterd = mappings.filter(_._1 != default).map(t => (t._1.asInstanceOf[Bool] -> t._2))
    list(defaultValue.get._2, filterd)
  }
}


@deprecated("Use HardType instead", "???")
object wrap{
  def apply[T <: Bundle](that : => T) : T = {
    val ret: T = that
    ret.hardtype = HardType(that)
    ret
  }
}

@deprecated("Use HardType instead", "???")
object cloneable {
  def apply[T <: Bundle](that: => T): T = {
    val ret: T = that
    ret.hardtype = HardType(that)
    ret
  }
}


class NamingScope(val duplicationPostfix : String, parent: NamingScope = null) {
  var lock = false
  val map  = mutable.Set[String]()
  val overlaps  = mutable.Map[String, Int]()

  assert(duplicationPostfix.isEmpty)

  def allocateName(name: String): String = {
    assert(!lock)
    val lowerCase = name.toLowerCase
    if(!map.contains(lowerCase) &&  (parent == null || !parent.map.contains(lowerCase))) {
      map += lowerCase
      return name
    }
    var count = overlaps.getOrElseUpdate(lowerCase, 0)
    while(true){
      count += 1
      val alternative = name + "_" + count
      val alternativeLowCase = alternative.toLowerCase()
      if(!map.contains(alternativeLowCase) && (parent == null || !parent.map.contains(alternativeLowCase))){
        map += alternativeLowCase
        overlaps(lowerCase) = count
        return alternative
      }
    }
    return null
  }

//  def getUnusedName(name: String): String = {
//    allocateName(name)
//  }


  def lockName(name: String): Unit = {
    assert(!lock)
    val lowerCase = name.toLowerCase
//    assert(!map.contains(lowerCase))
    map += lowerCase
  }

  def iWantIt(name: String, errorMessage: => String): Unit = {
    assert(!lock)
    val lowerCase = name.toLowerCase
    if (map.contains(lowerCase) ||  (parent != null && parent.map.contains(lowerCase)))
      PendingError(errorMessage)
    map += (lowerCase)
  }

  def lockScope(): Unit ={
    this.lock = true
  }

  def newChild(duplicationPostfix : String = this.duplicationPostfix) = new NamingScope(duplicationPostfix, this)
}


trait Stackable{
  def postPushEvent() = {}
  def postPopEvent()  = {}
  def prePopEvent()   = {}
}


class SafeStackWithStackable[T <: Stackable] extends SafeStack[T] {
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


/**
  * Safe Stack
  */
class SafeStack[T] {
  val stack = new Stack[T]()

  def push(e: T): Unit = stack.push(e)

  def pop(e: T): Unit = {
    if (stack.head != e)
      throw new Exception("Stack pop fail")
    stack.pop()
  }

  def head() = stack.headOption.getOrElse(null.asInstanceOf[T])

  def oldest() = stack.lastOption.getOrElse(null.asInstanceOf[T])

  def isEmpty: Boolean = stack.isEmpty

  def size() = stack.size

  def reset() = stack.clear()
}


object SpinalExit {
  def apply(message: String = "") = throw new SpinalExit(s"\n $message")

  val errorsMessagesSeparator     = s"${"*" * 80}\n${"*" * 80}"
}


object SpinalLog{
  def tag(name: String, color: String): String =
    if (System.console != null)
      s"[$color$name${Console.RESET}]"
    else
      s"[$name]"
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


class SpinalExit(message: String) extends Exception("\n\n" + (Seq(message)++ GlobalData.get.pendingErrors.map(_.apply())).map(_ + "\n" + SpinalExit.errorsMessagesSeparator + "\n\n").mkString("") + "Design's errors are listed above.\nSpinalHDL compiler exit stack : \n")


object PendingError {
  def apply(error:  String): Unit = {
    GlobalData.get.pendingErrors += (() => error)
  }
}


object LocatedPendingError {
  def apply(error: => String) = {
    val location = ScalaLocated.long
    GlobalData.get.pendingErrors += (() => error + "\n" + location)
  }
}


object SpinalError {
  private var errCount:Int = 0

  def apply() = SpinalExit()

  def apply(message: String) = {
    errCount += 1
    SpinalExit(message)
  }

  def apply(messages: Seq[String]) = {
    errCount += messages.length
    SpinalExit(messages.reduceLeft(_ + "\n\n" + _))
  }

  def printError(message: String) = println(s"${SpinalLog.tag("Progress", Console.RED)} $message")

  def getErrorCount(): Int = {
    val ret = errCount + GlobalData.get.pendingErrors.length
    errCount = 0
    ret
  }
}


object ifGen {
  def apply[T](cond: Boolean)(block: => T): T = if (cond) block else  null.asInstanceOf[T]
}


object ArrayManager{
  
  def setAllocate[T](array: Array[T], idx: Int, value: T, initialSize: Int = 4)(implicit m: ClassTag[T]): Array[T] = {
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

  def getElseNull[T](array: Array[T], idx: Int)(implicit m: ClassTag[T]) : T = {
    if(array == null)       return null.asInstanceOf[T]
    if(array.length <= idx) return null.asInstanceOf[T]
    array(idx)
  }
}


object AnnotationUtils{
  def isDontName(f: Field): Boolean = f.isAnnotationPresent(classOf[dontName])
}


/**
 * Create a new signal, assigned by the given parameter.
 * Useful to provide a "copy" of something that you can then modify with more conditional assignments.
 */
object CombInit {
  def apply[T <: Data](init: T): T = {
    val ret = cloneOf(init)
    ret := init
    ret
  }

  def apply[T <: SpinalEnum](init : SpinalEnumElement[T]) : SpinalEnumCraft[T] = apply(init())
}


trait AllowIoBundle{

}

object LutInputs extends ScopeProperty[Int]{
  override def default: Int = 4
}

object ClassName{
  def apply(that : Any) =  that.getClass.getSimpleName.replace("$","")
}

object ContextSwapper{
  def outsideCondScope[T](that : => T) : T = {
    val t = AsyncThread.current
    t.allowSuspend = false
    val body = Component.current.dslBody  // Get the head of the current component symboles tree (AST in other words)
    val ctx = body.push()                 // Now all access to the SpinalHDL API will be append to it (instead of the current context)
    val swapContext = body.swap()         // Empty the symbole tree (but keep a reference to the old content)
    val ret = that                        // Execute the block of code (will be added to the recently empty body)
    ctx.restore()                         // Restore the original context in which this function was called
    swapContext.appendBack()              // append the original symboles tree to the modified body
    t.allowSuspend = true
    ret                                   // return the value returned by that
  }

  //Allows to (only) to create base type instances on the top of the netlist. Support Fiber suspend
  def outsideCondScopeData[T <: Data](that: => T): T = {
    val dummyBody = new ScopeStatement(null)
    dummyBody.component = Component.current
    val ctx = dummyBody.push() // Now all access to the SpinalHDL API will be append to it (instead of the current context)
    val ret = that // Execute the block of code (will be added to the recently empty body)
    ctx.restore() // Restore the original context in which this function was called

    val topBody = Component.current.dslBody // Get the head of the current component symboles tree (AST in other words)
    val oldHead = topBody.head
    val oldLast = topBody.last
    val addedHead = dummyBody.head
    val addedLast = dummyBody.last

    //Move the AST from dummyBody to the head of topBody
    dummyBody.foreachStatements {
      case cu: ContextUser => cu.parentScope = topBody
      case _ =>
    }
    topBody.head = addedHead
    addedHead.lastScopeStatement = null.asInstanceOf[Statement]
    addedLast.nextScopeStatement = oldHead
    if(oldHead != null) oldHead.lastScopeStatement = addedLast
    if(oldLast != null) oldLast.nextScopeStatement = null.asInstanceOf[Statement]
    topBody.last = oldLast

    ret // return the value returned by that
  }
}



object Pull{
  def driveFromTopInput[T <: Data](that : T) : T = {
    val input = Component.toplevel.rework(in(cloneOf(that))).setCompositeName(that)
    that := input.pull()
    that
  }

  def driveFromTopInput[T <: Data](that : T, name : String) : T = {
    val input = Component.toplevel.rework(in(cloneOf(that))).setName(name, weak = false)
    that := input.pull()
    that
  }

  def toTopOutput[T <: Data](that : T) : T = {
    val top = Component.toplevel
    val io = top.rework {
      val topPulled = that.pull()
      out(CombInit(topPulled)).setCompositeName(that)
    }
    that
  }
}

//      .setPartialName(new Nameable {
//      val chain = Component.current.parents().tail :+ Component.current
//      override type RefOwnerType = this.type
//      override def isNamed = chain.forall(_.isNamed) && that.isNamed
//      override def getName(default : String = "") : String = {
//        if(!isNamed) return default
//        chain.map(_.getName()).mkString("_") + "_" + that.getName()
//      }
//    })
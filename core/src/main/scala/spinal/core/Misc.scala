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


import java.lang.reflect.Field

import scala.collection.mutable
import scala.collection.mutable.Stack
import scala.reflect.ClassTag
import scala.runtime.Nothing$

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
  //Return the number of bit of the given data
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
    val cache = GlobalData.get.componentStack.head().userCache.getOrElseUpdate(key, scala.collection.mutable.Map[Object, Object]())
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


object ScalaUniverse {
  def isCaseClass(o: Any): Boolean = {
    val clazz = o.getClass
    clazz.getInterfaces.find(_ == classOf[scala.Product]).isDefined && clazz.getDeclaredMethods.find(_.getName == "copy").isDefined
  }
}

object Misc {

  def addReflectionExclusion(o: Object) = reflectExclusion += o.getClass

  val reflectExclusion = mutable.Set[Class[_]]()

  addReflectionExclusion(new Bundle())
  addReflectionExclusion(new Vec(Bool, Vector[Bool]()))
  addReflectionExclusion(new Bool)
  addReflectionExclusion(new Bits)
  addReflectionExclusion(new UInt)
  addReflectionExclusion(new SInt)
  addReflectionExclusion(new Generic)
  addReflectionExclusion(new SpinalEnum)
  addReflectionExclusion(new SpinalEnumCraft(null))
  addReflectionExclusion(new Area{})


  def reflect(o: Object, onEach: (String, Object) => Unit, namePrefix: String = ""): Unit = {
    val refs = mutable.Set[Object]()
    explore(o.getClass)
    def explore(c: Class[_]): Unit = {
      if (c == null) return
      if (reflectExclusion.contains(c) || c.getName + "$" == Component.getClass.getName)
        return
      explore(c.getSuperclass)

//      val fields = c.getDeclaredFields
//      def isValDef(m: java.lang.reflect.Method) = fields exists (fd => fd.getName == m.getName && fd.getType == m.getReturnType && ! AnnotationUtils.isDontName(fd)) //  && java.lang.reflect.Modifier.isPublic(fd.getModifiers )     && fd.isAnnotationPresent(Class[spinal.core.refOnly])
//      val methods = c.getDeclaredMethods filter (m => m.getParameterTypes.isEmpty && isValDef(m))
//
//
      val methods = c.getDeclaredMethods
      val fields = c.getDeclaredFields.filter(fd => methods.exists(_.getName == fd.getName) && ! AnnotationUtils.isDontName(fd))

      for (field <- fields) {
        field.setAccessible(true)
        val fieldRef = field.get(o)
        if (fieldRef != null && (!refs.isInstanceOf[Data] || !refs.contains(fieldRef))) {
          val methodName = field.getName
          val firstCharIndex = methodName.lastIndexOf('$')
          val postFix = if(firstCharIndex == -1)
            methodName
          else
            methodName.substring(firstCharIndex+1)
          val name = namePrefix + postFix
          fieldRef match {
            case range : Range =>
            case vec: Vec[_] =>
            case seq: Seq[_] => {
              for ((obj, i) <- seq.zipWithIndex) {
                onEach(name + i, obj.asInstanceOf[Object])
                refs += fieldRef
              }
            }
            case seq: Array[_] => {
              for ((obj, i) <- seq.zipWithIndex) {
                onEach(name + i, obj.asInstanceOf[Object])
                refs += fieldRef
              }
//              for ((obj, i) <- seq.zipWithIndex) {
//                reflect(obj.asInstanceOf[Object], onEach, name  + "_" + i + "_"  )
//              }
            }
//            case zone: Area => {
//              reflect(zone, onEach, name + "_")
//            }
            case _ =>
          }
          onEach(name, fieldRef)
          refs += fieldRef
        }
      }
    }
  }

  def normalizeResize(to: Node, inputId: Integer, width: Int) {
    val input = to.getInput(inputId)
    if (input == null || input.asInstanceOf[WidthProvider].getWidth == width) return;

    input match{
      case bitVector : BitVector => {
        bitVector.getInput(0) match{
          case lit : BitVectorLiteral if (! lit.hasSpecifiedBitCount) =>{
            Component.push(input.component)
            val sizedLit = lit.clone()
            sizedLit.asInstanceOf[Widthable].inferredWidth = width
            to.setInput(inputId,sizedLit)
            Component.pop(input.component)
            Misc.normalizeResize(to, inputId, Math.max(lit.minimalValueBitWidth,width)) //Allow resize on direct literal with unfixed values

          }

          case _ => {
            val that = input.asInstanceOf[BitVector]
            Component.push(that.component)
            val resize = that.resize(width)
            resize.inferredWidth = width
            resize.input.asInstanceOf[Widthable].inferredWidth = width
            to.setInput(inputId,resize)
            Component.pop(that.component)
          }
        }
      }
      case _ =>
    }
  }
}

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

class Scope {
  val map = mutable.Map[String, Int]()


  def allocateName(name: String): String = {
    val lowerCase = name.toLowerCase
    val count = map.get(lowerCase).getOrElse(0)
    map(lowerCase) = count + 1
    if (count == 0) name else name + "_" + count
  }

  def getUnusedName(name: String): String = {
    val lowerCase = name.toLowerCase
    val count = map.get(lowerCase).getOrElse(0)
    if (count == 0) name else name + "_" + count
  }


  def lockName(name: String): Unit = {
    val lowerCase = name.toLowerCase
    val count = map.get(lowerCase).getOrElse(1)
    map(lowerCase) = count
  }

  def iWantIt(name: String): Unit = {
    val lowerCase = name.toLowerCase
    if (map.contains(lowerCase))
      SpinalError(s"Reserved name $name is not free")
    map(lowerCase) = 1
  }

  def copy() : Scope = {
    val cpy = new Scope
    map.foreach{case (n,i) => cpy.map.put(n,i)}
    cpy
  }
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

class SpinalExit(message: String) extends Exception("\n\n" + (Seq(message)++ GlobalData.get.pendingErrors.map(_.apply())).reduceLeft(_ + "\n\n" + _));

object PendingError {
  def apply(error : => String) = GlobalData.get.pendingErrors += (() => error)
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

class MaskedLiteral(val value : BigInt,val careAbout : BigInt,val width : Int){
  def ===(that : BitVector) : Bool = {
    assert(that.getWidth == width)
    return (that.asBits & careAbout) === value
  }
  def =/=(that : BitVector) : Bool = !(this === that)
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
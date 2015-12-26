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
import scala.collection.mutable.Stack

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

object cloneOf {
  //Return a new data with the same data structure than the given parameter (including bit width)
  def apply[T <: Data](that: T): T = that.clone()
}

object widthOf {
  //Return the number of bit of the given data
  def apply[T <: Data](that: T): Int = that.getBitsWidth
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
    else data.map(_.toBits).reduce((a,b) => b ## a)
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
  addReflectionExclusion(new Vec(null, null))
  addReflectionExclusion(new Bool)
  addReflectionExclusion(new Bits)
  addReflectionExclusion(new UInt)
  addReflectionExclusion(new SInt)
  addReflectionExclusion(new Generic)
  addReflectionExclusion(new SpinalEnum)
  addReflectionExclusion(new SpinalEnumCraft(null))


  //TODO find if there is a solution to keep declaration order in every case, then remove fix from component.nameElements
  //  def reflect(o: Object, onEach: (String, Any) => Unit,namePrefix :String = ""): Unit = {
  //    val refs = mutable.Set[Any]()
  //    val ru = scala.reflect.runtime.universe
  //    val runtimeMirror = ru.runtimeMirror(o.getClass.getClassLoader)
  //    val instanceMirror = runtimeMirror.reflect(o)
  //    val symbols = instanceMirror.symbol.typeSignature.members.sorted
  //    for(symbol <- symbols){
  //      if(symbol.isMethod){
  //        val method = symbol.asMethod
  //        if(method.isGetter && method.isPublic){
  //          val fieldRef = instanceMirror.reflectMethod(method.asMethod).apply()
  //          if (fieldRef != null && !refs.contains(fieldRef)) {
  //            val name = namePrefix + method.name
  //            fieldRef match {
  //              case vec: Vec[_] =>
  //              case seq: Seq[_] => {
  //                for ((obj, i) <- seq.zipWithIndex) {
  //                  onEach(name + i, obj.asInstanceOf[Object])
  //                  refs += fieldRef
  //                }
  //              }
  //              case zone : Area => {
  //                reflect(zone,onEach,name + "_")
  //              }
  //              case _ =>
  //            }
  //            onEach(name, fieldRef)
  //            refs += fieldRef
  //          }
  //        }
  //      }
  //    }
  //  }

  def reflect(o: Object, onEach: (String, Object) => Unit, namePrefix: String = ""): Unit = {
    val refs = mutable.Set[Object]()
    explore(o.getClass)
    def explore(c: Class[_]): Unit = {
      if (c == null) return
      if (reflectExclusion.contains(c) || c.getName + "$" == Component.getClass.getName)
        return
      explore(c.getSuperclass)

      val fields = c.getDeclaredFields
      def isValDef(m: java.lang.reflect.Method) = fields exists (fd => fd.getName == m.getName && fd.getType == m.getReturnType)
      val methods = c.getDeclaredMethods filter (m => m.getParameterTypes.isEmpty && isValDef(m))



      for (method <- methods) {
        method.setAccessible(true)
        val fieldRef = method.invoke(o)
        if (fieldRef != null && !refs.contains(fieldRef)) {
          val name = namePrefix + method.getName
          fieldRef match {
            case vec: Vec[_] =>
            case seq: Seq[_] => {
              for ((obj, i) <- seq.zipWithIndex) {
                onEach(name + i, obj.asInstanceOf[Object])
                refs += fieldRef
              }
            }
            case zone: Area => {
              reflect(zone, onEach, name + "_")
            }
            case _ =>
          }
          onEach(name, fieldRef)
          refs += fieldRef
        }
      }
    }
  }

  def normalizeResize(to: Node, inputId: Integer, width: Int) {
    val input = to.inputs(inputId)
    if (input == null || input.getWidth == width || input.isInstanceOf[NoneNode]) return;

    val that = input.asInstanceOf[BitVector]
    Component.push(that.component)
    val resize = that.resize(width)
    resize.inferredWidth = width
    to.inputs(inputId) = resize
    Component.pop(that.component)
//    if (input.isInstanceOf[BaseType] && input.asInstanceOf[BaseType].getLiteral[Literal] == null)
//      println("asd")
//    else
//      SpinalWarning("Automatic resize on " + to.toString)
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

  def lockName(name: String): Unit = {
    val lowerCase = name.toLowerCase
    val count = map.get(lowerCase).getOrElse(1)
    map(lowerCase) = count
  }

  def iWantIt(name: String): Unit = {
    val lowerCase = name.toLowerCase
    if (map.contains(lowerCase)) SpinalError("Reserved name $name is not free")
    map(lowerCase) = 1
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
  def apply(message: String = "Unspecified exit") = {
    throw new SpinalExit("\n" + message)
  }
}
object SpinalLog{
  def tag(name: String, color: String): String =
    s"[${color}${name}${Console.RESET}]"
}

object SpinalInfoPhase {
  def apply(message: String) = println(s"${SpinalLog.tag("Progress", Console.BLUE)} at ${f"${Driver.executionTime}%1.3f"} : $message")
}

object SpinalInfo {
  def apply(message: String) = println(s"${SpinalLog.tag("Info", Console.BLUE)} $message")
}

object SpinalWarning {
  def apply(message: String) = println(s"${SpinalLog.tag("Warning", Console.YELLOW)} $message")
}

class SpinalExit(message: String) extends Exception(message);

object SpinalError {
  def apply() = {
    SpinalExit()
  }

  def apply(message: String) = {
    SpinalExit(message)
  }

  def apply(messages: Seq[String]) = {
    SpinalExit(messages.reduceLeft(_ + "\n" + _))
  }

  def printError(message: String) = println(s"${SpinalLog.tag("Progress", Console.RED)} $message")
}



object ifGen {
  def apply[T](cond: Boolean)(block: => T): T = {
    if (cond)
      return block
    else
      return null.asInstanceOf[T]
  }
}
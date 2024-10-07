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
package spinal.core.internals

import spinal.core._
import scala.collection.mutable


object ScalaUniverse {
  def isCaseClass(o: Any): Boolean = {
    val clazz = o.getClass
    clazz.getInterfaces.find(_ == classOf[scala.Product]).isDefined && clazz.getDeclaredMethods.find(_.getName == "copy").isDefined
  }
}


trait BaseNode extends ScalaLocated{
  var algoInt, algoIncrementale = 0

  private[core] def getClassIdentifier: String = this.getClass.getName.split('.').last.replace("$","")

  def toStringMultiLine(): String = this.toString
}


object TypeBool
object TypeBits
object TypeUInt
object TypeSInt
object TypeEnum
object TypeStruct


trait DoubleLinkedContainerElement[SC  <: DoubleLinkedContainer[SC, SE], SE <: DoubleLinkedContainerElement[SC, SE]]{

  def dlcParent : SC

  var dlceLast, dlceNext : SE = null.asInstanceOf[SE]

  def dlcRemove(): Unit = {
    //super.removeStatement()

    //Remove from BaseType
    if(dlceLast != null){
      dlceLast.dlceNext = dlceNext
    } else {
      dlcParent.dlcHead = dlceNext
    }

    if(dlceNext != null){
      dlceNext.dlceLast = dlceLast
    } else {
      dlcParent.dlcLast = dlceLast
    }

    dlceLast = null.asInstanceOf[SE]
    dlceNext = null.asInstanceOf[SE]
  }
}


trait DoubleLinkedContainer[SC <: DoubleLinkedContainer[SC, SE], SE <: DoubleLinkedContainerElement[SC, SE]]{
  var dlcHead, dlcLast : SE = null.asInstanceOf[SE]

  def dlcHasOnlyOne = dlcHead == dlcLast && dlcHead != null
  def dlcIsEmpty    = dlcHead == null
  //  def sizeIsOne = head != null && head == last

  def dlcPrepend(that: SE): this.type = {
    if(dlcHead != null){
      dlcHead.dlceLast = dlcHead
    } else {
      dlcLast = that
    }

    that.dlceNext = dlcHead
    that.dlceLast = null.asInstanceOf[SE]

    dlcHead = that

    this
  }

  def dlcAppend(that: SE): this.type = {
    that.dlceNext = null.asInstanceOf[SE]
    that.dlceLast = dlcLast

    if(dlcLast != null){
      dlcLast.dlceNext = that
    } else {
      dlcHead = that
    }

    dlcLast = that
    this
  }

  //  def dlcIterable = new Iterable[SE] {
  //    override def iterator: Iterator[SE] = iterator
  //  }
  //
  //  def dlcIterator = new Iterator[SE] {
  //    var ptr = dlcHead
  //    override def hasNext: Boolean = ptr != null
  //
  //    override def next(): SE = {
  //      val ret = ptr
  //      ptr = ret.dlceNext
  //      ret.asInstanceOf[SE]
  //    }
  //  }


  def dlcForeach[T >: SE](func: T => Unit): Unit = {
    var ptr = dlcHead
    while(ptr != null){
      val current = ptr
      ptr = ptr.dlceNext
      func(current)
    }
  }

  def cldCount: Int ={
    var count = 0
    dlcForeach{_ => count += 1}
    count
  }
}



object Misc {

  def addReflectionExclusion(o: Object) = reflectExclusion += o.getClass

  val reflectExclusion = mutable.Set[Class[_]]()

  addReflectionExclusion(classOf[Bundle])
  addReflectionExclusion(classOf[Vec[_]])
  addReflectionExclusion(classOf[Bool])
  addReflectionExclusion(classOf[Bits])
  addReflectionExclusion(classOf[UInt])
  addReflectionExclusion(classOf[SInt])
  addReflectionExclusion(classOf[Generic])
  addReflectionExclusion(classOf[SpinalEnum])
  addReflectionExclusion(classOf[SpinalEnumCraft[_]])
  addReflectionExclusion(classOf[Area])

  def reflect(o: Object, onEach: (String, Object) => Unit, namePrefix: String = ""): Unit = {
    val refs = mutable.Set[Object]()

    def applyNameTo(name: String, fieldRef: Object): Unit ={
      if (fieldRef != null && (!refs.isInstanceOf[Data] || !refs.contains(fieldRef))) {
        fieldRef match {
          case range : Range =>
          case vec: Vec[_]   =>
          case seq: Seq[_]   =>
            for ((obj, i) <- seq.zipWithIndex) {
              applyNameTo(name + "_" + i, obj.asInstanceOf[Object])
              refs += fieldRef
            }
          case seq: Array[_] =>
            for ((obj, i) <- seq.zipWithIndex) {
              applyNameTo(name + "_" + i, obj.asInstanceOf[Object])
              refs += fieldRef
            }
          case _             =>
        }

        onEach(name, fieldRef)
        refs += fieldRef
      }
    }

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
      val fields  = c.getDeclaredFields.filter(fd => methods.exists(_.getName == fd.getName) && ! AnnotationUtils.isDontName(fd))

      for (field <- fields) {
        field.setAccessible(true)

        val fieldRef = field.get(o)

        if (fieldRef != null && (!refs.isInstanceOf[Data] || !refs.contains(fieldRef))) {
          val methodName     = field.getName
          val firstCharIndex = methodName.lastIndexOf('$')

          val postFix = if(firstCharIndex == -1)
            methodName
          else
            methodName.substring(firstCharIndex+1)

          val name = namePrefix + postFix

          applyNameTo(name,fieldRef)
        }
      }
    }
  }

  //  def normalizeResize(to: Node, inputId: Integer, width: Int) {
  //    val input = to.getInput(inputId)
  //    if (input == null || input.asInstanceOf[WidthProvider].getWidth == width) return;
  //
  //    input match{
  //      case bitVector : BitVector => {
  //        bitVector.getInput(0) match{
  //          case lit : BitVectorLiteral if (! lit.hasSpecifiedBitCount) =>{
  //            Component.push(input.component)
  //            val sizedLit = lit.clone()
  //            sizedLit.asInstanceOf[Widthable].inferredWidth = width
  //            to.setInput(inputId,sizedLit)
  //            Component.pop(input.component)
  //            Misc.normalizeResize(to, inputId, Math.max(lit.minimalValueBitWidth,width)) //Allow resize on direct literal with unfixed values
  //
  //          }
  //
  //          case _ => {
  //            val that = input.asInstanceOf[BitVector]
  //            Component.push(that.component)
  //            val resize = that.resize(width)
  //            resize.inferredWidth = width
  //            resize.input.asInstanceOf[Widthable].inferredWidth = width
  //            to.setInput(inputId,resize)
  //            Component.pop(that.component)
  //          }
  //        }
  //      }
  //      case _ =>
  //    }
  //  }
}

object GraphUtils{
  def walkAllComponents(root: Component, func: Component => Unit): Unit = {
    func(root)
    root.children.foreach(walkAllComponents(_, func))
  }
  def countNames(topLevel : Component) ={
    var named, unamed = 0
    GraphUtils.walkAllComponents(topLevel, c => c.dslBody.walkStatements{
      case s : Nameable => if(s.getName().contains("zz"))  unamed += 1 else named += 1
      case _ =>
    })
    println(s"Named=$named Unamed=$unamed")
    named
  }

}


class BooleanPimped(pimped : Boolean){
  def generate[T](block : => T) : T = if(pimped) block else null.asInstanceOf[T]
  def toInt = if(pimped) 1 else 0
  def mux[T](whenTrue: => T, whenFalse: => T): T = if (pimped) whenTrue else whenFalse
  def option[T](value: => T): Option[T] = pimped match {
    case false => None
    case true => Some(value)
  }
}


class IntPimped(pimped : Int){
  def toBoolean = pimped match {
    case 0 => false
    case 1 => true
  }

  def #* (value : Bits) =  Cat(List.fill(pimped)(value))
}
package spinal.core

import scala.reflect.runtime.universe._
import scala.collection.mutable.ArrayBuffer

object IsInterface extends SpinalTag {}

object Interface {
  def mkNewName(name: String, count: Int) = (
    name + (
      if (count == 0) (
        ""
      ) else (
        "_" + count
      )
    )
  )
}

/** system verilog interface
  * 
  * ==Example==
  *{{{
  * case class MyIf(width: Int = 8) extends Interface with IMasterSlave {
  *  val wParam = addGeneric("WIDTH", width, default = "8")
  *  val a = Bits(width bits)
  *  tieGeneric(a, wParam)
  *  val b = Bool()
  *  val c = SInt(8 bits)
  *
  *  override def asMaster = mst
  *  @modport
  *  def mst = {
  *    out(a, b, c)
  *  }
  *
  *  @modport
  *  def slv = {
  *    in(a, b, c)
  *  }
  *
  *}
  *}}}
  *
  */
class Interface extends Bundle {
  var definitionName: String = this.getClass.getSimpleName
  var origDefinitionName: String = null//this.getClass.getSimpleName
  var thisIsNotSVModport = false
  var thisIsNotSVIF = false
  var thisIsSVstruct = false
  var noConvertSVIFvec = false
  /** Set the definition name of the component */
  def setDefinitionName(name: String): this.type = {
    definitionName = name
    this
  }
  val genericElements = ArrayBuffer[(String, Any, String)]()
  def addGeneric(name : String, that : Any, default: String = null): String = {
    that match {
      case bt: BaseType => genericElements += Tuple3(name, bt.addTag(GenericValue(bt.head.source)), default)
      case vv: VerilogValues => genericElements += Tuple3(name, vv, default)
      case s: String    => genericElements += Tuple3(name, s, default)
      case i: Int       => genericElements += Tuple3(name, i, default)
      case i: BigInt if i <= Integer.MAX_VALUE && i >= Integer.MIN_VALUE => genericElements += Tuple3(name, i.toInt, default)
      case d: Double        => genericElements += Tuple3(name, d, default)
      case boolean: Boolean => genericElements += Tuple3(name, boolean, default)
      case t: TimeNumber    => genericElements += Tuple3(name, t, default)
    }
    name
  }
  def addParameter(name: String, that: Any, default: String = null): String = addGeneric(name, that, default)
  val widthGeneric = scala.collection.mutable.LinkedHashMap[BitVector, String]()
  val IFGeneric = scala.collection.mutable.LinkedHashMap[(Data, String), String]()
  def tieGeneric[T <: BitVector](signal: T, generic: String) = {
    widthGeneric += signal -> generic
  }
  def tieParameter[T <: BitVector](signal: T, parameter: String) = tieGeneric(signal, parameter)
  def tieIFParameter[T <: Interface](signal: T, signalParam: String, inputParam: String) = {
    IFGeneric += (signal, signalParam) -> inputParam
  }

  override def valCallbackRec(ref: Any, name: String): Unit = {
    //def checkForBundleWithIntfElem(elem: Data, name: String): Unit = {
    //  elem match {
    //    case intf: Interface => {
    //      LocatedPendingError(s"sv interface is still under develop. by now Interface cannot be contained inside Bundle that is contained inside Interface")
    //      return
    //    }
    //    case bndl: Bundle => {
    //      if (bndl.elementsCache != null) {
    //        for ((name, elem) <- bndl.elementsCache) {
    //          checkForBundleWithIntfElem(elem=elem, name=name)
    //        }
    //      }
    //    }
    //    case vec: Vec[_] => {
    //      for ((elem, idx) <- vec.zipWithIndex) {
    //        checkForBundleWithIntfElem(
    //          elem=elem,
    //          name=s"${elem}_${idx}"
    //        )
    //      }
    //    }
    //    case _ =>
    //  }
    //}
    def checkForErrors(
      elem: Data, name: String, foundStruct: Boolean=false, foundBundle: Boolean=false
    ): Unit = {
      elem match {
        case intf: Interface => {
          //if (foundBundle) {
          //  LocatedPendingError(s"sv interface is still under develop. by now Interface cannot be contained inside Bundle that is contained inside Interface")
          //  return
          //}
          if (!intf.thisIsSVstruct && foundStruct) {
            LocatedPendingError(s"sv interface cannot be contained inside sv struct")
            return
          }
          if (intf.elementsCache != null) {
            for ((name, elem) <- intf.elementsCache) {
              checkForErrors(
                elem=elem,
                name=name,
                foundStruct=intf.thisIsSVstruct,
                foundBundle=foundBundle
              )
            }
          }
        }
        case bndl: Bundle => {
          if (bndl.elementsCache != null) {
            for ((name, elem) <- bndl.elementsCache) {
              checkForErrors(
                elem=elem,
                name=name,
                foundStruct=foundStruct,
                foundBundle=true,
              )
            }
          }
        }
        case vec: Vec[_] => {
          for ((elem, idx) <- vec.zipWithIndex) {
            checkForErrors(
              elem=elem,
              name=s"${elem}_${idx}",
              foundStruct=foundStruct,
              foundBundle=foundBundle,
            )
          }
        }
        case _ => {
        }
      }
    }
    ref match {
      case ref : Data => {
        ref match {
          case ref: BaseType => {
            ref match {
              case _: SpinalEnumCraft[_] => {
                LocatedPendingError(s"sv interface is still under develop. by now SpinalEnum is not allowed")
              }
              case ref => {
                ref.addTag(IsInterface)
              }
            }
            if(elementsCache != null)
              if(elementsCache.find(_._1 == name).isDefined)
                LocatedPendingError(s"name conflict: ${name} has been used")
            super.valCallbackRec(ref, name)
          }
          case intf: Interface => {
            if(elementsCache != null)
              if(elementsCache.find(_._1 == name).isDefined)
                LocatedPendingError(s"name conflict: ${name} has been used")
            super.valCallbackRec(ref, name)
            checkForErrors(
              elem=ref,
              name=name,
              foundStruct=intf.thisIsSVstruct,
              foundBundle=false,
            )
          }
          case b: Bundle => {
            b.flattenForeach(x => x.addTag(IsInterface))
            //b.parent = this
            super.valCallbackRec(ref, name)
            //checkForBundleWithIntfElem(elem=b, name=name)
            checkForErrors(
              elem=b,
              name=name,
              foundStruct=false,
              foundBundle=true,
            )
          }
          case ref: Vec[_] => {
            if(OwnableRef.proposal(ref, this)) ref.setPartialName(name, Nameable.DATAMODEL_WEAK)
            ref.IFvecNamePrefix = name
            ref.zipWithIndex.foreach{case (node, idx) =>
              node.IFvecParent = ref
              valCallbackRec(node, s"${name}_${idx}")
            }
          }
          case _ => {
            LocatedPendingError(s"sv interface is still under develop. by now only BaseType is allowed")
          }
        }
      }
      case ref => {
        if(elementsCache != null)
          if(elementsCache.find(_._1 == name).isDefined)
            LocatedPendingError(s"name conflict: ${name} has been used")
        super.valCallbackRec(ref, name)
      }
    }
  }
  def allModPort: List[String] = {
    this.getClass.getMethods()
      .filter(m => {
        m.getAnnotation(classOf[modport]) != null
      })
      .map(m => m.getName())
      .toList
  }
  def callModPort(s: String): Unit = {
    this.getClass.getMethod(s).invoke(this).asInstanceOf[Unit]
  }
  def checkModport() = {
    allModPort
      .filter(x => checkIODir(x=Some(x)))
  }
  def checkIODir(x: Option[String]): Boolean = {
    val t = this
    val toplevel = globalData.toplevel
    val phase = globalData.phaseContext.topLevel
    globalData.toplevel = null
    globalData.phaseContext.topLevel = null
    val c = new Component {
      val y = t.clone().asInstanceOf[t.type]
      x match {
        case Some(myX) => {
          y.callModPort(myX)
        }
        case None =>
      }
    }
    globalData.toplevel = toplevel
    globalData.phaseContext.topLevel = phase

    c.y.elements.foldLeft(true) {case (dirSame, (name, element)) =>
      val other = this.find(name)
      if (other == null) {
        LocatedPendingError(s"Bundle assignment is not complete. Missing $name")
        false
      } else element match {
        case b: Bundle => b.checkDir(other.asInstanceOf[Bundle]) && dirSame
        case b         => (b.dir == other.dir) && dirSame
      }
    }
  }
  override def clone() = {
    val ret = super.clone().asInstanceOf[this.type]
    ret.setDefinitionName(this.definitionName)
    ret
  }

  def notSVIF(): Unit = {
    this.flattenForeach(x => x.removeTag(IsInterface))
    this.elementsCache.foreach{
      case (name, x: Interface) => x.notSVIF()
      case _ =>
    }
    this.thisIsNotSVIF = true
  }

  def notSVIFthisLevel(): Unit = {
    this.elementsCache.foreach{case (name, x) => x match {
      case s: BaseType => s.removeTag(IsInterface)
      case _ =>
    }}
    this.thisIsNotSVIF = true
  }
  def notSVModport(): this.type = {
    this.elementsCache.foreach{
      case (name, x: Interface) => x.notSVModport()
      case _ =>
    }
    this.thisIsNotSVModport = true
    this
  }

  def notSVModportthisLevel(): this.type = {
    this.thisIsNotSVModport = true
    this
  }
  def dontConvertSVIFvec(): this.type = {
    this.elementsCache.foreach{
      case (name, x: Interface) => x.dontConvertSVIFvec()
      case _ =>
    }
    this.noConvertSVIFvec = true
    this
  }
  def dontConvertSVIFvecThisLevel(): this.type = {
    this.noConvertSVIFvec = true
    this
  }
  def setAsSVstruct(): this.type = {
    this.elementsCache.foreach{
      case (name, x: Interface) => x.setAsSVstruct()
      case _ =>
    }
    this.thisIsSVstruct = true
    this
  }
  def setAsSVstructThisLevel(): this.type = {
    this.thisIsSVstruct = true
    this
  }
}

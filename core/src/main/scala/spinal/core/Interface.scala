package spinal.core

import scala.reflect.runtime.universe._
import scala.collection.mutable.ArrayBuffer

object IsInterface extends SpinalTag {}

/** system verilog interface
  * 
  * ==Example==
  *{{{
  * case class MyIf(width: Int = 8) extends SVIF with IMasterSlave {
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
  var thisIsNotSVIF = false
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
          case _: Interface => {
            if(elementsCache != null)
              if(elementsCache.find(_._1 == name).isDefined)
                LocatedPendingError(s"name conflict: ${name} has been used")
            super.valCallbackRec(ref, name)
          }
          case ref: Vec[_] => {
            if(OwnableRef.proposal(ref, this)) ref.setPartialName(name, Nameable.DATAMODEL_WEAK)
            ref.zipWithIndex.foreach{case (node, idx) =>
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
      .filter(x => {
        val t = this
        val toplevel = globalData.toplevel
        val phase = globalData.phaseContext.topLevel
        globalData.toplevel = null
        globalData.phaseContext.topLevel = null
        val c = new Component {
          val y = t.clone().asInstanceOf[t.type]
          y.callModPort(x)
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
      })
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
}

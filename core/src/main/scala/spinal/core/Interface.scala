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
  def checkDir(self: Bundle, x: Option[String]/*, checkStruct: Boolean=false, rootIODir: IODirection=null*/): Boolean = {
    val t = self
    val toplevel = self.globalData.toplevel
    val phase = self.globalData.phaseContext.topLevel
    self.globalData.toplevel = null
    self.globalData.phaseContext.topLevel = null
    val c = new Component {
      val y = t.clone().asInstanceOf[t.type]
      x match {
        case Some(myX) => {
          y match {
            case yIntf: Interface => {
              yIntf.callModPort(myX)
            }
            case _ =>
          }
        }
        case None =>
      }
    }
    self.globalData.toplevel = toplevel
    self.globalData.phaseContext.topLevel = phase
    def myVecFunc(someVec: Vec[_], otherVec: Vec[_]): Boolean = {
      if (someVec.size == otherVec.size) {
        for (idx <- 0 until someVec.size) {
          val vecElem = someVec(idx)
          val otherVecElem = otherVec(idx)
          vecElem match {
            case intf: Interface if intf.thisIsNotSVmodport => {
              if (!innerCheckDir(self=intf, that=otherVecElem.asInstanceOf[Bundle])) {
                println(
                  s"debug ("
                    + s"innerCheckDir("
                      + s"self=intf, that=otherVecElem.asInstanceOf[Bundle]"
                    + s") == false"
                  + s"): "
                  + s"intf:(${intf.getName()} ${intf.origDefinitionName} ${idx})"
                  + s"someVec:(${someVec.getName()} ${someVec.size})"
                  + s"otherVec:(${otherVec.getName()} ${otherVec.size})"
                )
                return false
              }
              println(
                s"debug ("
                  + s"innerCheckDir("
                    + s"self=intf, that=otherVecElem.asInstanceOf[Bundle]"
                  + s") == true"
                + s"): "
                + s"intf:(${intf.getName()} ${intf.origDefinitionName} ${idx})"
                + s"someVec:(${someVec.getName()} ${someVec.size})"
                + s"otherVec:(${otherVec.getName()} ${otherVec.size})"
              )
              //return true
              //otherVecElem match {
              //  case otherIntf: Interface if !otherIntf.thisIsNotSVmodport => {
              //    return intf.checkDir(x=None)
              //  }
              //  case otherBndl: Bundle => {
              //    return false
              //  }
              //  case _ => {
              //    return false
              //  }
              //}
            }
            case bndl: Bundle => {
              if (!innerCheckDir(self=bndl, that=otherVecElem.asInstanceOf[Bundle])) {
                println(
                  s"debug ("
                    + s"innerCheckDir("
                      + s"self=bndl, that=otherVecElem.asInstanceOf[Bundle]"
                    + s") == false"
                  + s"): "
                  + s"bndl:(${bndl.getName()} ${idx})"
                  + s"someVec:(${someVec.getName()} ${someVec.size})"
                  + s"otherVec:(${otherVec.getName()} ${otherVec.size})"
                )
                return false
              }
              println(
                s"debug ("
                  + s"innerCheckDir("
                    + s"self=bndl, that=otherVecElem.asInstanceOf[Bundle]"
                  + s") == true"
                + s"): "
                + s"bndl:(${bndl.getName()} ${idx})"
                + s"someVec:(${someVec.getName()} ${someVec.size})"
                + s"otherVec:(${otherVec.getName()} ${otherVec.size})"
              )
              //otherVecElem match {
              //  //case otherIntf: Interface => {
              //  //  return false
              //  //}
              //  case otherBndl: Bundle => {
              //    return bndl.asInstanceOf[Interface].checkDir(x=None)
              //  }
              //  case _ => {
              //    return false
              //  }
              //}
            }
            case vec: Vec[_] => {
              otherVecElem match {
                case otherVec: Vec[_] => {
                  if (!myVecFunc(someVec=vec, otherVec=otherVec)) {
                    println(
                      s"debug (myVecFunc(...) == false): "
                      + s"someVec:(${someVec.getName()} ${someVec.size})"
                      + s"vecElem:(${vec.getName()} ${vec.size} ${idx})"
                      + s"otherVecElem:(${otherVec.getName()} ${otherVec.size})"
                    )
                    return false
                  }
                  println(
                    s"debug (myVecFunc(...) == true): "
                    + s"someVec:(${someVec.getName()} ${someVec.size})"
                    + s"vecElem:(${vec.getName()} ${vec.size} ${idx})"
                    + s"otherVecElem:(${otherVec.getName()} ${otherVec.size})"
                  )
                  //return true
                }
                case _ => {
                  println(
                    s"debug (otherVecElem not a `Vec[_]`): "
                    + s"someVec:(${someVec.getName()} ${someVec.size})"
                    + s"vecElem:(${vec.getName()} ${vec.size} ${idx})"
                    + s"otherVec:(${otherVec.getName()} ${otherVec.size})"
                  )
                  return false
                }
              }
            }
            case b: Bool => {
              otherVecElem match {
                case otherB: Bool => {
                  if (b.dir != otherB.dir) {
                    println(
                      s"debug Bool (b.dir != otherB.dir): "
                      + s"someVec:(${someVec.getName()} ${someVec.size}) "
                      + s"otherVec:(${otherVec.getName()} ${otherVec.size}) "
                      + s"b:(${b.getName()} ${b.dir} ${idx}) "
                      + s"otherB:(${otherB.getName()} ${otherB.dir} ${idx})"
                    )
                    return false
                  }
                  println(
                    s"debug Bool (b.dir == otherB.dir): "
                    + s"someVec:(${someVec.getName()} ${someVec.size}) "
                    + s"otherVec:(${otherVec.getName()} ${otherVec.size}) "
                    + s"b:(${b.getName()} ${b.dir} ${idx}) "
                    + s"otherB:(${otherB.getName()} ${otherB.dir} ${idx})"
                  )
                }
                case _ => {
                  println(
                    s"debug Bool (otherVecElem different type): "
                    + s"someVec:(${someVec.getName()} ${someVec.size}) "
                    + s"otherVec:(${otherVec.getName()} ${otherVec.size}) "
                    + s"b:(${b.getName()} ${b.dir} ${idx}) "
                  )
                  return false
                }
              }
            }
            case bv: BitVector => (
              otherVecElem match {
                case otherBv: BitVector => {
                  if (bv.dir != otherBv.dir) {
                    println(
                      s"debug BitVector (bv.dir != otherBv.dir): "
                      + s"someVec:(${someVec.getName()} ${someVec.size}) "
                      + s"otherVec:(${otherVec.getName()} ${otherVec.size}) "
                      + s"bv:(${bv.getName()} ${bv.dir} ${idx}) "
                      + s"otherBv:(${otherBv.getName()} ${otherBv.dir} ${idx})"
                    )
                    return false
                  }
                }
                case _ => {
                  return false
                }
              }
            )
            case bd: Data => {
              println(
                s"debug: other `Data` type: "
                + s"bd:${bd.getName()} someVec:(${someVec.getName()} ${someVec.size}); "
                + s"this element:(index=${idx}) "
                + s"of this Vec: name=${someVec.getName()} size=${someVec.size}"
              )
              return false
            }
            case _ => {
              LocatedPendingError(
                s"unknown type for "
                + s"this element:(index=${idx}) "
                + s"of this Vec: name=${someVec.getName()} size=${someVec.size}"
              )
              return false
            }
          }
        }
        return true
      } else {
        println(
          s"eek! debug (sizes don't match): "
          + s"someVec:(${someVec.getName()} ${someVec.size}) "
          + s"otherVec:(${otherVec.getName()} ${otherVec.size}) "
        )
        return false
      }
    }

    def innerCheckDir(
      self: Bundle, that: Bundle
    ): Boolean = {
      var ret = true
      for ((name, element) <- self.elements) {
        val other = that.find(name)
        if (other == null) {
          LocatedPendingError(s"Bundle assignment is not complete. Missing $name")
        } else {
          element match {
            case i: Interface if i.thisIsNotSVmodport => {
              val temp = (innerCheckDir(self=i, that=other.asInstanceOf[Bundle]) && ret)
              //return ret //true //Interface.checkDir(self=i, x=None) && ret
              println(
                s"debug (innerCheckDir(self=i, that=other.asInstanceOf[Bundle]) == ${temp}): "
                + s"i:(${i.getName()} ${i.origDefinitionName})"
              )
              ret = temp
            }
            case b: Bundle => {
              val temp = innerCheckDir(self=b, that=other.asInstanceOf[Bundle]) && ret
              println(
                s"debug (innerCheckDir(self=b, that=other.asInstanceOf[Bundle]) == ${temp}): "
                + s"b:(${b.getName()})"
              )
              ret = temp
            }
            case v: Vec[_] => {
              other match {
                case otherVec: Vec[_] => {
                  ret = myVecFunc(someVec=v, otherVec=otherVec) && ret
                }
                case _ => {
                  ret = false
                }
              }
            }
            case b  => {
              ret = (b.dir == other.dir) && ret
            }
          }
        }
      }
      ret
    }

    return c.y.elements.foldLeft(true) {case (dirSame, (name, element)) =>
      val other = self.find(name)
      if (other == null) {
        LocatedPendingError(s"Bundle assignment is not complete. Missing $name")
        false
      } else {
        element match {
          case elemBndl: Bundle => (
            (
              other match {
                case that: Bundle => innerCheckDir(self=elemBndl, that=that)
                case _ => false
              }
            ) && dirSame
          )
          case elemVec: Vec[_] => (
            (
              other match {
                case otherVec: Vec[_] => myVecFunc(someVec=elemVec, otherVec=otherVec)
                case _ => false
              }
            ) && dirSame
          )
          case b => ((b.dir == other.dir) && dirSame)
        }
      }
    }
  }
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
  var thisIsNotSVmodport = false
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
    def checkForErrors(
      elem: Data, name: String, foundStruct: Boolean=false, foundBundle: Boolean=false
    ): Unit = {
      elem match {
        case intf: Interface => {
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
            super.valCallbackRec(ref, name)
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
      .filter(x => Interface.checkDir(this, Some(x)))
  }
  override def clone() = {
    val ret = super.clone().asInstanceOf[this.type]
    ret.setDefinitionName(this.definitionName)
    ret.origDefinitionName = this.origDefinitionName
    //ret.thisIsNotSVIF = this.thisIsNotSVIF
    ret.thisIsNotSVmodport = this.thisIsNotSVmodport
    ret.thisIsSVstruct = this.thisIsSVstruct
    ret.noConvertSVIFvec = this.noConvertSVIFvec
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

  def notSVmodport(): this.type = {
    this.elementsCache.foreach{
      case (name, x: Interface) => x.notSVmodport()
      case _ =>
    }
    this.thisIsNotSVmodport = true
    this
  }
  def notSVmodportThisLevel(): this.type = {
    this.thisIsNotSVmodport = true
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
    this.thisIsNotSVmodport = true
    this.thisIsSVstruct = true
    this
  }
  def setAsSVstructThisLevel(): this.type = {
    this.thisIsNotSVmodport = true
    this.thisIsSVstruct = true
    this
  }
}

package spinal.core

import spinal.core.internals.{Suffixable, TypeStruct}
import spinal.idslplugin.{Location, ValCallback}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.Seq

/**
 * Class representing Verilog Struct and VHDL Record data types.
 * @param typeName Underlying structure name to use if not the subclass name.
 */
abstract class SpinalStruct(val typeName: String = null) extends BaseType with Nameable with ValCallbackRec with DataPrimitives[SpinalStruct] with Suffixable {

  def elements: ArrayBuffer[(String, Data)] = elementsCache

  override private[spinal] def _data: SpinalStruct = this

  override def asInput(): this.type = {
    super.asInput()
    elements.foreach(_._2.asInput())
    this
  }

  override def asOutput(): this.type = {
    super.asOutput()
    elements.foreach(_._2.asOutput())
    this
  }

  override def asInOut(): this.type = {
    super.asInOut()
    elements.foreach(_._2.asInOut())
    this
  }

  override def copyDirectionOfImpl(that : Data): this.type = {
    super.copyDirectionOfImpl(that)
    (elements, that.asInstanceOf[MultiData].elements).zipped.foreach{case (t, s) => t._2.copyDirectionOfImpl(s._2)}
    this
  }

  override def setAsDirectionLess(): this.type = {
    super.setAsDirectionLess()
    elements.foreach(_._2.setAsDirectionLess());
    this
  }

  /** Set baseType to reg */
  override def setAsReg(): this.type = {
    super.setAsReg()
    elements.foreach(_._2.setAsReg())
    this
  }

  /** Set baseType to Combinatorial */
  override def setAsComb(): this.type = {
    super.setAsComb()
    elements.foreach(_._2.setAsComb())
    this
  }

  override def freeze(): this.type = {
    elements.foreach(_._2.freeze())
    this
  }

  override def unfreeze(): this.type = {
    elements.foreach(_._2.unfreeze())
    this
  }

  override def flatten: Seq[BaseType] = {
    elements.map(_._2.flatten).foldLeft(List[BaseType]())(_ ++ _)
  }

  def find(name: String): Data = {
    val temp = elements.find((tuple) => tuple._1 == name).getOrElse(null)
    if (temp == null) return null
    temp._2
  }

  override def asBits: Bits = {
    var ret: Bits = null
    for ((eName, e) <- elements) {
      if (ret == null.asInstanceOf[Object]) ret = e.asBits
      else ret = e.asBits ## ret
    }
    if (ret.asInstanceOf[Object] == null) ret = Bits(0 bits)
    ret
  }

  override def getBitsWidth: Int = {
    var accumulateWidth = 0
    for ((_, e) <- elements) {
      val width = e.getBitsWidth
      if (width == -1)
        SpinalError("Can't return bits width")
      accumulateWidth += width
    }
    accumulateWidth
  }

  override def assignFromBits(bits: Bits): Unit = {
    var offset = 0
    for ((_, e) <- elements) {
      val width = e.getBitsWidth
      e.assignFromBits(bits(offset, width bit))
      offset = offset + width
    }
  }

  override def assignFromBits(bits: Bits, hi: Int, lo: Int): Unit = {
    var offset = 0
    var bitsOffset = 0

    for ((_, e) <- elements) {
      val width = e.getBitsWidth

      if (hi >= offset && lo < offset + width) {
        val high = Math.min(hi-offset,width-1)
        val low  = Math.max(lo-offset,0)
        val bitUsage = high - low + 1
        e.assignFromBits(bits(bitsOffset,bitUsage bit), high,low)
        bitsOffset += bitUsage
      }

      offset = offset + width
    }

  }

  /** Assign the bundle with an other bundle by name */
  def assignAllByName(that: Bundle): Unit = {
    for ((name, element) <- elements) {
      val other = that.find(name)
      if (other == null)
        LocatedPendingError(s"Bundle assignment is not complete. Missing $name")
      else element match {
        case b: SpinalStruct => b.assignAllByName(other.asInstanceOf[Bundle])
        case _         => element := other
      }
    }
  }

  /** Assign all possible signal fo the bundle with an other bundle by name */
  def assignSomeByName(that: Bundle): Unit = {
    for ((name, element) <- elements) {
      val other = that.find(name)
      if (other != null) {
        element match {
          case b: SpinalStruct => b.assignSomeByName(other.asInstanceOf[Bundle])
          case _         => element := other
        }
      }
    }
  }

  protected override def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = {
    that match {
      case that: SpinalStruct =>
        if (!this.getClass.isAssignableFrom(that.getClass)) SpinalError("Structs must have the same final class to" +
          " be assigned. Either use assignByName or assignSomeByName at \n" + ScalaLocated.long)
        super.assignFromImpl(that, target, kind)
      case _ => throw new Exception("Undefined assignment")
    }
  }

  private[core] def isEqualTo(that: Any): Bool = {
    that match {
      case that: SpinalStruct => zippedMap(that, _ === _).reduce(_ && _)
      case _               => SpinalError(s"Function isEquals is not implemented between $this and $that")
    }
  }

  private[core] def isNotEqualTo(that: Any): Bool = {
    that match {
      case that: SpinalStruct => zippedMap(that, _ =/= _).reduce(_ || _)
      case _               => SpinalError(s"Function isNotEquals is not implemented between $this and $that")
    }
  }

  private[core] override def autoConnect(that: Data)(implicit loc: Location): Unit = {
    that match {
      case that: SpinalStruct => {
        this autoConnectBaseImpl that
        zippedMap(that, _ autoConnect _)
      }
      case _               => SpinalError(s"Function autoConnect is not implemented between $this and $that")
    }
  }

  def elementsString = this.elements.map(_.toString()).reduce(_ + "\n" + _)

  private[core] def zippedMap[T](that: SpinalStruct, task: (Data, Data) => T): Seq[T] = {
    if (that.elements.length != this.elements.length) SpinalError(s"Can't zip [$this] with [$that]  because they don't have the same number of elements.\nFirst one has :\n${this.elementsString}\nSeconde one has :\n${that.elementsString}\n")
    this.elements.map(x => {
      val (n, e) = x
      val other = that.find(n)
      if (other == null) SpinalError(s"Can't zip [$this] with [$that] because the element named '${n}' is missing in the second one")
      task(e, other)
    })
  }

  private var elementsCache = ArrayBuffer[(String, Data)]()

  override def valCallbackRec(ref: Any, name: String): Unit = ref match {
    case ref : Data => {
      elementsCache += name -> ref
      ref.parent = this
      if(OwnableRef.proposal(ref, this)) ref.setPartialName(name, Nameable.DATAMODEL_STRONG)
    }
    case ref =>
  }

  def getTypeString: String = {
    if (typeName == null)
      getClass.getSimpleName + "_t"
    else
      typeName
  }

  override def getZero: this.type = {
    val ret = cloneOf(this)
    ret.elements.foreach(e => {
      e._2 := e._2.getZero
    })
    ret.asInstanceOf[this.type]
  }

  override private[core] def newMultiplexerExpression() = ???

  override private[core] def newBinaryMultiplexerExpression() = ???

  /** Create a new instance of the same datatype without any configuration (width, direction) */
  override private[core] def weakClone = ???

  override def opName: String = "Struct"

  override def getTypeObject: Any = TypeStruct
}

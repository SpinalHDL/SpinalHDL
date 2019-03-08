package spinal.lib.sim

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable
import scala.language.dynamics

object SimData{
  def apply() = new SimData()
  def copy(hard : Data) = new SimData().load(hard)

  implicit def dataToSimData(data : Data) : SimData = copy(data)
}

class SimData extends Dynamic{
  val values = mutable.LinkedHashMap[String,Any]()

  def apply(name : String) = values(name)
  def update(name : String, value: BigInt) = updateDynamic(name)(value)

  def updateDynamic(name: String)(value: BigInt) = values(name) = value
  def selectDynamic(name: String) = values(name)

  def load(hard : Data): this.type ={
    hard match {
      case bt : BaseType => {
        values("self") = bt.toBigInt
      }
      case md : MultiData => {
        md.elements.foreach{e => e._2 match{
          case bt : BaseType => values(e._1) = bt.toBigInt
          case md : MultiData => values(e._1) = new SimData().load(md)
        }}
      }

    }
    this
  }

  def write(hard : Data): this.type ={
    hard match {
      case bt : BaseType => {
        bt.assignBigInt(values("self").asInstanceOf[BigInt])
      }
      case md : MultiData => {
        md.elements.foreach{e => e._2 match{
          case bt : BaseType => bt.assignBigInt(values(e._1).asInstanceOf[BigInt])
          case md : MultiData =>  values(e._1).asInstanceOf[SimData].write(md)
        }}
      }

    }
    this
  }

  def check(hard : Data): Boolean ={
    hard match {
      case bt : BaseType => {
        if(values("self") != bt.toBigInt) return false
      }
      case md : MultiData => {
        md.elements.foreach{e => e._2 match{
          case bt : BaseType => if(values(e._1) != bt.toBigInt) return false
          case md : MultiData => if(!values(e._1).asInstanceOf[SimData].check(md)) return false
        }}
      }

    }
    return true
  }


  def toString(tab : String) : String = {
    val str = new StringBuilder()
    for((name, value) <- values) value match{
      case value : BigInt => {
        str ++= s"$tab$name : 0x${value.toString(16)}\n"
      }
      case value : SimData => {
        str ++= s"$tab$name\n"
        str ++= value.toString(tab + "- ")
      }
    }
    str.toString
  }

  override def toString = toString("")

  override def equals(o: scala.Any) = o match {
    case o : SimData => values == o.values
    case _ => false
  }
  override def hashCode() = values.hashCode()
}

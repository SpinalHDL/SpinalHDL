package spinal.lib.io

import spinal.core._
import spinal.core.fiber.Engine
import spinal.lib.master

import scala.collection.mutable

object InOutWrapper {
  def apply[T <: Component](c : T) : T = {
    Engine.get.onCompletion += (() => {
      val dataParents = mutable.LinkedHashMap[Data, Int]()

      def add(that: Data): Unit = {
        if (that.parent != null) {
          dataParents(that.parent) = dataParents.getOrElseUpdate(that.parent, 0) + 1
          add(that.parent)
        }
      }

      def propagateTags(tristate: Bundle, pin: Data): Unit = {
        tristate.getTags().filter(_.ioTag).foreach(t => pin.addTag(t))
      }

      for (io <- c.getAllIo) {
        add(io)
      }

      c.rework {
        for ((dataParent, count) <- dataParents) {
          dataParent match {
            case bundle: TriState[_] if bundle.writeEnable.isOutput => {
              val newIo = inout(Analog(bundle.dataType)).setWeakName(bundle.getName())
              bundle.setAsDirectionLess.unsetName().allowDirectionLessIo
              bundle.read.assignFrom(newIo)
              when(bundle.writeEnable) {
                newIo := bundle.write
              }
              propagateTags(bundle, newIo)
            }
            case bundle: TriStateOutput[_] if bundle.isOutput || bundle.isMasterInterface => {
              val newIo = inout(Analog(bundle.dataType)).setWeakName(bundle.getName())
              bundle.setAsDirectionLess.unsetName().allowDirectionLessIo
              when(bundle.writeEnable) {
                newIo := bundle.write
              }
              propagateTags(bundle, newIo)
            }
            case bundle: ReadableOpenDrain[_] if bundle.write.isOutput && bundle.read.isInput => {
              val newIo = inout(Analog(bundle.dataType)).setWeakName(bundle.getName())
              bundle.setAsDirectionLess.unsetName().allowDirectionLessIo
              bundle.read.assignFrom(newIo)
              for ((value, id) <- bundle.write.asBits.asBools.zipWithIndex) {
                when(!value) {
                  newIo.assignFromBits(B"0", id, 1 bits)
                }
              }
              propagateTags(bundle, newIo)
              //            for(bt <- bundle.write.flatten){
              //              for((value, id) <- bt.asBits.asBools.zipWithIndex) {
              //                when(!value){
              //                  bt.assignFromBits("0", id, 1 bits)
              //                }
              //              }
              //            }
            }
            case bundle: TriStateArray if bundle.writeEnable.isOutput => {
              val newIo = inout(Analog(bundle.write)).setWeakName(bundle.getName())
              bundle.setAsDirectionLess.unsetName().allowDirectionLessIo
              bundle.read.assignFrom(newIo)
              for (i <- 0 until bundle.width) {
                when(bundle.writeEnable(i)) {
                  newIo(i) := bundle.write(i)
                }
              }
              propagateTags(bundle, newIo)
            }
            case _ =>
          }
        }
      }
    })
    c
  }

  def main(args: Array[String]): Unit = {
    case class MyTriStateTag() extends SpinalTag {
      override def ioTag = true
    }
    case class D() extends Bundle{
      val x = UInt(2 bits)
      val y = Bool()
    }
    val report = SpinalVhdl(InOutWrapper(new Component{
      def t = D()
      val driver = in(t)
      val sink = out(t)
      val openDrain = master(ReadableOpenDrain(t))
      openDrain.addTag(MyTriStateTag())
      openDrain.write := driver
      sink := openDrain.read
    }))
    report.toplevel.getAllIo.foreach(io => println(s"${io.getName()} => ${io.getTags()}"))
  }
}




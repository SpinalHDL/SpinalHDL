package spinal.lib.io

import spinal.core._

import scala.collection.mutable

object InOutWrapper {
  def apply[T <: Component](c : T) : T = {
    val dataParents = mutable.LinkedHashMap[Data,Int]()
    for(io <- c.getAllIo if io.parent != null){
      dataParents(io.parent) = dataParents.getOrElseUpdate(io.parent,0) + 1
    }

    c.rework {
      for ((dataParent, count) <- dataParents) {
        dataParent match {
          case bundle: TriState[_]  if bundle.isMasterInterface => {
            val newIo = inout(Analog(bundle.dataType)).setWeakName(bundle.getName())
            bundle.asDirectionLess.unsetName().allowDirectionLessIo
            bundle.read.assignFrom(newIo)
            when(bundle.writeEnable){
              newIo := bundle.write
            }
          }
          case bundle: ReadableOpenDrain[_]  if bundle.isMasterInterface => {
            val newIo = inout(Analog(bundle.dataType)).setWeakName(bundle.getName())
            bundle.asDirectionLess.unsetName().allowDirectionLessIo
            bundle.read.assignFrom(newIo)
            newIo := bundle.write
          }
          case bundle: TriStateArray if bundle.isMasterInterface => {
            val newIo = inout(Analog(bundle.write)).setWeakName(bundle.getName())
            bundle.asDirectionLess.unsetName().allowDirectionLessIo
            bundle.read.assignFrom(newIo)
            for(i <- 0 until bundle.width) {
              when(bundle.writeEnable(i)) {
                newIo(i) := bundle.write(i)
              }
            }
          }
          case _ =>
        }
      }
    }
    c
  }
}

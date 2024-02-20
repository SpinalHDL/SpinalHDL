package spinal.lib.io

import spinal.core._
import scala.collection.mutable

/** Can be used in blackbox to have a "Vec.fill(count)(dataType)"
 *  which will appear a multiple  Bits(count * widthOf(dataType.element[x]) bits)
 *  See https://github.com/SpinalHDL/SpinalHDL/issues/1258#issuecomment-1849942630
 */
class InOutVecToBits[T <: Data](dataType: => T, count: Int) extends Area {
  def apply(i: Int) = accesses(i)

  val mapping = mutable.LinkedHashMap[BaseType, Bits]()
  val template = dataType.setName("")
  val accesses = Component.current.parent.rework {
    Vec.fill(count)(dataType.setAsDirectionLess())
  }
  val keys = template.flatten
  // Generate the Bits array from the template
  for (e <- keys) {
    val array = Bits(widthOf(e) * count bits).setCompositeName(this, e.getName())
    e.getDirection match {
      case `in` => in(array)
      case `out` => out(array)
    }
    mapping(e) = array
    e.removeStatement()
  }
  // Generate the parent component access points
  Component.current.parent.rework {
    val accessesKeys = accesses.map(_.flatten)
    for (i <- 0 until keys.size) {
      val key = mapping(keys(i))
      val slices = key.subdivideIn(count slices)
      for (sliceId <- 0 until count) {
        key.getDirection match {
          case `in` => slices(sliceId) := accessesKeys(sliceId)(i).asBits
          case `out` => accessesKeys(sliceId)(i).assignFromBits(slices(sliceId))
        }
      }
    }
  }
}
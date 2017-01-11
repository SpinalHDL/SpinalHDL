package spinal.lib

import spinal.core._

class BoolPimped(pimped : Bool){

  //Warning, there is no overflow protection
  def genEvent : Event = {
    val e = Event
    val reg = RegInit(False)

    reg := (reg | pimped) & !e.ready
    e.valid := pimped | reg

    e
  }
}

/**
 * Endianness enumeration
 */
sealed trait Endianness
/** Little-Endian */
object LITTLE extends Endianness
/** Big-Endian */
object BIG    extends Endianness

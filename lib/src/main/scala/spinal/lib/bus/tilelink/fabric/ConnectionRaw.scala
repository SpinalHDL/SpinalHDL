package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, OffsetTransformer, SizeMapping}
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer

/**
 * provide some software interface to connect 2 NodeBase
 */
class ConnectionRaw(m : NodeUpDown, s : NodeUpDown) extends bus.fabric.MappedConnection(m, s) {
  //Handles used for negociation
  val up, down = new Area{
    val bus = Handle[Bus]()
    val m2s = new Area{
      val parameters = Handle[M2sParameters]()
    }
    val s2m = new Area{
      val parameters = Handle[S2mParameters]()
    }
  }
}


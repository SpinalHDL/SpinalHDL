package spinal.lib.bus.mmslavefactory

sealed trait AccessType

object AccessType {
  case object RO extends AccessType
  case object RW extends AccessType
  case object WO extends AccessType
  case object NA extends AccessType
}


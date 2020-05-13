package spinal.lib.bus.regif

sealed trait DocType

object DocType{
  case object Json     extends DocType
  case object Rst      extends DocType
  case object MarkDown extends DocType
  case object HTML     extends DocType
  case object Docx     extends DocType
}

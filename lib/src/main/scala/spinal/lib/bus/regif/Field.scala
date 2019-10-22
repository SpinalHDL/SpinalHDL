package spinal.lib.bus.regif

import spinal.core._

case class Field(name: String,
                 hardbit: Bits,
                 section: Range,
                 accType: AccessType,
                 resetValue: Bits,
                 readError: Boolean,
                 doc: String)
package spinal.lib.eda.altera.ip



trait BOOLEAN { def value : String }
case object ON   extends BOOLEAN { def value = "on"   }
case object OFF  extends BOOLEAN { def value = "off"  }
case object NONE extends BOOLEAN { def value = "None" }


trait IO_STRANDARD { def value : String }
case object STD_1_2V      extends IO_STRANDARD { def value = "1.2V" }
case object STD_1_2V_HSTL extends IO_STRANDARD { def value = "1.2- V HSTL" }
case object STD_1_2V_HSUL extends IO_STRANDARD { def value = "1.2- V HSUL" }
case object STD_NONE      extends IO_STRANDARD { def value = "None" }
// @TODO declare all others io standard

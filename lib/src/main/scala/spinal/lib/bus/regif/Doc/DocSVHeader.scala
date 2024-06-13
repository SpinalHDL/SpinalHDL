package spinal.lib.bus.regif

import spinal.lib._
import spinal.lib.bus.regif.AccessType._

import scala.math

final case class DocSVHeader(name : String,
                             override val prefix: String = "",
                             withshiftmask: Boolean = true) extends BusIfDoc {
  override val suffix: String = "svh"

  def guardName : String = s"${name}_REGIF_H"

  def body(): String = {
    val maxnamelen = bi.slices.map(_.getName().size).max + prefix.length
    val maxshiftlen = bi.slices.map(t => t.getName().size + t.fdNameLens).max + prefix.length
    val normalRegSlices = bi.regSlicesNotReuse
    val reuseGroupsById = bi.reuseGroupsById
    s"""|/*
        | * ${header}
        | * Reg Interface SV-Header [AUTOGENERATE by SpinalHDL]
        | */
        |
        |`ifndef ${guardName}
        |`define ${guardName}
        |
        |${normalRegSlices.map(_.define(maxnamelen, maxshiftlen)).mkString("\n")}
        |
        |${reuseDeclare(reuseGroupsById)}
        |
        |`endif /* ${guardName} */
        |""".stripMargin
  }

  def reuseDeclare(lst: Map[String, Map[Int, List[RegSlice]]]) = {

    def base(name: String, t: RegSlice, max: Int) = {
      val alignName = s"%-${max}s".format(t.reuseTag.instName)
      val defineName = s"${name}_base_${alignName}".toUpperCase()
      s"`define ${defineName}  0x${t.reuseTag.baseAddr.hexString()}"
    }

    lst.map{ t =>
      val partName = t._1
      val decPart: List[RegSlice]  = t._2.head._2
      val heads : List[RegSlice] = t._2.map(_._2.head).toList.sortBy(_.reuseTag.id)

      val instNameMaxLens = heads.map(_.reuseTag.instName.size).max
      val maxnamelen = decPart.map(_.getName().size).max + prefix.length
      val maxshiftlen = decPart.map(t => t.getName().size + t.fdNameLens).max + prefix.length

      s"""
        |/* part '${partName}' declare --> */
        |${heads.map(t => base(partName, t, instNameMaxLens)).mkString("\n")}
        |/* part '${partName}' defines */
        |${decPart.map(_.baseDefine(maxnamelen, maxshiftlen)).mkString("\n")}
        |/* <--- part '${partName}' declare */
        |""".stripMargin
    }.mkString("")
  }

  def nameDedupliaction(repeat: String, word: String) = word.toUpperCase().replaceAll(repeat.toUpperCase() + "_", "")

  implicit class RegSliceCheadExtend(reg: RegSlice) {
    val deDupRegName = nameDedupliaction(prefix, reg.getName())
    val preFixRegName = (if(prefix.isEmpty) deDupRegName else s"${prefix}_${deDupRegName}").toUpperCase()

    def define(maxreglen: Int, maxshiftlen: Int): String = {
      val _tab = " " * (maxreglen - deDupRegName.size)
      s"""`define ${preFixRegName} ${_tab}0x${reg.getAddr().hexString(16)}${fddefine(maxshiftlen)}""".stripMargin
    }

    def baseDefine(maxreglen: Int, maxshiftlen: Int) = {
      val _tab = " " * (maxreglen - deDupRegName.size)
      s"""`define ${preFixRegName}(base)  ${_tab}base + 0x${(reg.getAddr() - reg.reuseTag.baseAddr).hexString(8)}${fddefine(maxshiftlen)}""".stripMargin
    }

    def fdNameLens = math.max("reserved_0".size, reg.getFields().map(_.getName.size).max)

    def fddefine(maxlen: Int): String = {
      val nmaxlen = maxlen - preFixRegName.size
      if (withshiftmask) {
        val t = reg.getFields().map(t => t.define(preFixRegName, nmaxlen, prefix)).filterNot(_.isEmpty).mkString("\n")
        if (t.isEmpty) "" else "\n" + t
      } else ""
    }
  }

  implicit class FieldCHeadExtend(fd: Field) {
    def define(pre: String, tabn: Int = 0, duplicate: String = ""): String = {
      //add Define  XXX_SHIFT   XXX_MASK  for SW bit operation
      def lsb: Int = fd.getSection().min

      def msb: Int = fd.getSection().max

      def mask = BigInt((1 << fd.getSection().size) - 1) << lsb

      val newfdname = nameDedupliaction(duplicate, fd.getName())
      val _tab = " " * (tabn - newfdname.size)
      fd.getAccessType() match {
        case `NA` => ""
        case `W1S` | `W1C` | `W1T` | `W1P` | `W1CRS` | `W1SRC` | `W1SHS` | `W1CHS` => s"""`define ${pre}_${newfdname}_SHIFT ${_tab}${lsb} /*${fd.getName()} 1bit*/""".stripMargin
        case `W0S` | `W0C` | `W0T` | `W0P` | `W0CRS` | `W0SRC` => s"""`define ${pre}_${newfdname}_SHIFT ${_tab}${lsb} /*${fd.getName()} 1bit*/""".stripMargin
        case _ => {
          if (fd.getSection().size == bi.busDataWidth) "" else if (fd.getName() == "_bm_") "" else
            s"""`define ${pre}_${newfdname}_SHIFT ${_tab}${lsb}
               |`define ${pre}_${newfdname}_MASK  ${_tab}0x${mask.hexString(32)} /* ${fd.getName()}, ${fd.getAccessType()}, ${fd.getWidth()} bit */""".stripMargin
        }
      }
    }
  }
}
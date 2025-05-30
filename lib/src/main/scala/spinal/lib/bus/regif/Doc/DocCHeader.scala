package spinal.lib.bus.regif

import spinal.lib._

import scala.math
import spinal.lib.bus.regif.AccessType._

final case class DocCHeader(name : String,
                            override val prefix: String = "",
                            regType : String = "u32", //unsigned int
                            withshiftmask: Boolean = true) extends BusIfDoc {
  override val suffix: String = "h"

  def guardName : String = s"__${name.toUpperCase()}_REGIF_H__"

  def body(): String = {
    val maxnamelen = bi.slices.map(_.getName().size).max + prefix.length
    val maxshiftlen = bi.slices.map(t => t.getName().size + t.fdNameLens).max + prefix.length
    val normalRegSlices = bi.regSlicesNotReuse
    val reuseGroupsById = bi.reuseGroupsById
    s"""|/*
        | * ${header}
        | * Reg Interface C-Header [AUTOGENERATE by SpinalHDL]
        | */
        |
        |#ifndef ${guardName}
        |#define ${guardName}
        |
        |${normalRegSlices.map(_.define(maxnamelen, maxshiftlen)).mkString("\n")}
        |
        |${reuseDeclare(reuseGroupsById)}
        |
        |${normalRegSlices.map(_.union).mkString("\n")}
        |
        |${reuseStruct(reuseGroupsById)}
        |#endif /* ${guardName} */
        |""".stripMargin
  }

  def reuseDeclare(lst: Map[String, Map[Int, List[RegSlice]]]) = {

    def base(name: String, t: RegSlice, max: Int) = {
      val alignName = s"%-${max}s".format(t.reuseTag.instName)
      val defineName = s"${name}_base_${alignName}".toUpperCase()
      s"#define ${defineName}  0x${t.reuseTag.baseAddr.hexString()}"
    }

    lst.map{ t =>
      val partName = t._1
//      val decPart: List[RegSlice]  = t._2.head._2
      val decPart: List[RegSlice]  = t._2.toList.sortBy(_._1).head._2
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

  def reuseStruct(lst: Map[String, Map[Int, List[RegSlice]]]) = {
    lst.map { t =>
      val partName = t._1
      val decPart: List[RegSlice] = t._2.toList.sortBy(_._2.head.addr).head._2
      s"""/*part '${partName}' start --> */
         |${decPart.map(_.union).mkString("\n")}
         |/*<-- part '${partName}' end*/
         |""".stripMargin
    }.mkString("\n")
  }

  def nameDedupliaction(repeat: String, word: String) = word.toUpperCase().replaceAll(repeat.toUpperCase() + "_", "")

  implicit class RegSliceCheadExtend(reg: RegSlice) {
    val deDupRegName = nameDedupliaction(prefix, reg.getName())
    val preFixRegName = (if(prefix.isEmpty) deDupRegName else s"${prefix}_${deDupRegName}").toUpperCase()

    def define(maxreglen: Int, maxshiftlen: Int): String = {
      val _tab = " " * (maxreglen - deDupRegName.size)
      s"""#define ${preFixRegName} ${_tab}0x${reg.getAddr().hexString(16)}${fddefine(maxshiftlen)}""".stripMargin
    }

    def baseDefine(maxreglen: Int, maxshiftlen: Int) = {
      val _tab = " " * (maxreglen - deDupRegName.size)
      s"""#define ${preFixRegName}(base)  ${_tab}base + 0x${(reg.getAddr() - reg.reuseTag.baseAddr).hexString(8)}${fddefine(maxshiftlen)}""".stripMargin
    }

    def union: String = {
      s"""/**
         |  * @union       ${preFixRegName.toLowerCase()}_t
         |  * @address     0x${reg.getAddr().hexString(16)}
         |  * @brief       ${reg.getDoc().replace("\n", "\\n")}
         |  */
         |typedef union {
         |    ${regType} val;
         |    struct {
         |        ${fdUnion(" " * 8)}
         |    } reg;
         |} ${preFixRegName.toLowerCase()}_t;""".stripMargin
    }

    def fdNameLens = math.max("reserved_0".size, reg.getFields().map(_.getName.size).max)

    def fdUnion(tab: String): String = {
      var i, j = -1
      val fields = reg.getFields()
      val maxlen = fdNameLens
      fields.map(fd => {
        val name = fd.getAccessType match {
          case AccessType.NA => i += 1; s"reserved_${i}"
          case AccessType.ROV => j += 1; s"rov_${j}"
          case _ => fd.getName()
        }
        val _tab = " " * (maxlen - name.size)
        val reset = fd.getAccessType match {
          case AccessType.NA => ""
          case _ => ", reset: 0x" + fd.getResetValue().hexString(fd.getWidth())
        }
        f"""$regType ${name.toLowerCase()}${_tab} : ${fd.getWidth()}%2d; /* ${fd.getAccessType()}${reset}, ${fd.getDoc().replace("\n", "\\n")} */""".stripMargin
      }).mkString("\n" + tab)
    }

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
        case `NA`  | `ROV` => ""
        case `W1S` | `W1C` | `W1T` | `W1P` | `W1CRS` | `W1SRC` | `W1SHS` | `W1CHS` | `W1I` => s"""#define ${pre}_${newfdname}_SHIFT ${_tab}${lsb} /* ${fd.getName()}, ${fd.getAccessType()}, 1 bit */""".stripMargin
        case `W0S` | `W0C` | `W0T` | `W0P` | `W0CRS` | `W0SRC` => s"""#define ${pre}_${newfdname}_SHIFT ${_tab}${lsb} /* ${fd.getName()}, ${fd.getAccessType()}, 1 bit */""".stripMargin
        case _ => {
          if (fd.getSection().size == bi.busDataWidth) "" else if (fd.getName() == "_bm_") "" else
            s"""#define ${pre}_${newfdname}_SHIFT ${_tab}${lsb}
               |#define ${pre}_${newfdname}_MASK  ${_tab}0x${mask.hexString(32)} /* ${fd.getName()}, ${fd.getAccessType()}, ${fd.getWidth()} bit */""".stripMargin
        }
      }
    }
  }
}
package spinal.lib.bus.regif

import spinal.lib.BigIntRicher

final case class DocPlay(name : String) extends BusIfDoc {
  override val suffix: String = "log"

  override def body(): String = {
    case class Part(name: String, slices: List[RegSlice], bases: List[(BigInt, Int)]){
      def heads: String = {
        bases.map(t => s"""# define ${name}_base_${t._2}  ${t._1}""").mkString("\n      ")
      }
      def defines: String = {
        slices.map(_.define).mkString("\n      ")
      }
    }

    val fundefins = bi.slices.filter(_.reuseTag.id != 0)
      .groupBy(_.reuseTag.partName)
      .map{ t =>
        val ret = t._2.groupBy(_.reuseTag.id)
        val bases = t._2.map(t => t.reuseTag.baseAddr -> t.reuseTag.id)
        Part(t._1, ret.head._2, bases)
      }

    s"""{
      |  "system"     : "${name}",
      |  "busport"    : "${bi.busName}",
      |  "datawidth"  : "${bi.busDataWidth}",
      |  "addrwidth"  : "${bi.busAddrWidth}",
      |  "byteMask"   : "${bi.withStrb}",
      |  "date"       : "${java.time.LocalDate.now}",
      |  "ToolVersion": "${bi.getVersion}",
      |  "slices"     : [
      |     ${bi.slices.map(_.toJson).mkString(",\n     ")}
      |  ],
      |  "defines" : [
      |     ${bi.repeatGroupsHead.map(_._2.map(_.define).mkString(",\n     ")).mkString(",\n     ")}
      |     ${bi.repeatGroupsBase.map(t => t._2.map(v => s"#define ${t._1}_base_${v.reuseTag.instName}  0x${v.reuseTag.baseAddr.hexString()}").mkString(",\n     ")).mkString(",\n     ")}
      |  ]
      |}""".stripMargin
  }

  implicit class RegSliceExtend(reg: RegSlice) {
    def toJson: String = {
      s"""|{"tag": "${reg.reuseTag}", "addr": ${reg.getAddr()}, "name": "${reg.getName().toUpperCase()}", }""".stripMargin
    }

    def define: String = {
      s"""# define ${reg.getName().toUpperCase}(base)  base + ${reg.getAddr() - reg.reuseTag.baseAddr}""".stripMargin
    }
  }

  implicit class FieldDescrExtend(field: Field) {
    def toJson: String = {
      s""" --- """.stripMargin
    }
  }
}

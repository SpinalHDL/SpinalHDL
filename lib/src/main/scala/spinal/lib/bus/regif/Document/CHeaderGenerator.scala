package spinal.lib.bus.regif

import spinal.lib._
import spinal.core.{GlobalData, SpinalError}
import scala.collection.mutable
import java.io.PrintWriter
import scala.math
import spinal.lib.bus.regif.AccessType._

final case class CHeaderGenerator(
    fileName : String,
    prefix : String,
    regType : String = "u32",
    headers : List[String] = Nil,
    withshiftmask: Boolean = true ) extends BusIfVisitor {
    val words = "\\w*".r
    prefix match{
        case words(_*) => null
        case _ => SpinalError(s"${prefix} should be Valid naming : '[A-Za-z0-9_]+'")
    }
        
    case class Reg(name : String, addr : Long)
    case class Field(name : String, width : Long, accessType : AccessType)
    case class Type(name : String, var fields : List[FieldDescr])

    val guardName : String = s"${prefix}_REGIF_H"
    val regs : mutable.ListBuffer[RegDescr] = mutable.ListBuffer[RegDescr]()
    val types : mutable.ListBuffer[Type] = mutable.ListBuffer[Type]()
    var regLength : Int = 0

    def begin(busDataWidth : Int) : Unit = {

    }

    def visit(descr : BaseDescriptor) : Unit = {
        descr match {
            case descr: RegDescr => regDescrVisit(descr)
            case _ => ???
        }
    }

    private def regDescrVisit(descr: RegDescr): Unit = {
        def nameLen = descr.getName.length()

        if(nameLen > regLength)
            regLength = nameLen

        regs += descr
        types += Type(descr.getName, descr.getFieldDescrs)
    }

    def end() : Unit = {
        val pc = GlobalData.get.phaseContext
        val targetPath = s"${pc.config.targetDirectory}/${fileName}.h"
        val pw = new PrintWriter(targetPath)

        implicit class RegDescrCheadExtend(reg: RegDescr) {
            def define(maxreglen: Int, maxshiftlen: Int): String  = {
                val _tab = " " * (maxreglen - reg.getName().size)
                s"""#define ${reg.getName().toUpperCase()} ${_tab}0x${reg.getAddr().hexString(16)}${fddefine(maxshiftlen)}""".stripMargin
            }

            def union: String = {
                s"""/**
                   |  * @union       ${reg.getName().toLowerCase()}_t
                   |  * @address     0x${reg.getAddr().hexString(16)}
                   |  * @brief       ${reg.getDoc().replace("\n","\\n")}
                   |  */
                   |typedef union {
                   |    u32 val;
                   |    struct {
                   |        ${fdUnion(" " * 8)}
                   |    } reg;
                   |} ${reg.getName().toLowerCase()}_t;""".stripMargin
            }

            def fdNameLens = math.max("reserved_0".size, reg.getFieldDescrs().map(_.getName.size).max)

            def fdUnion(tab: String): String = {
                var i, j = -1
                val fields = reg.getFieldDescrs()
                val maxlen = fdNameLens
                fields.map(fd => {
                    val name = fd.getAccessType match {
                        case AccessType.NA  => i += 1; s"reserved_${i}"
                        case AccessType.ROV => j += 1; s"rov_${j}"
                        case _ => fd.getName()
                    }
                    val _tab = " " * (maxlen - name.size)
                    val reset = fd.getAccessType match {
                        case AccessType.NA => ""
                        case _ =>  ", reset: 0x" + fd.getResetValue().hexString(fd.getWidth())
                    }
                    f"""$regType ${name.toLowerCase()}${_tab} : ${fd.getWidth()}%2d; //${fd.getAccessType()}${reset}, ${fd.getDoc().replace("\n","\\n")}""".stripMargin
                }).mkString("\n" + tab)
            }

            def fddefine(maxlen: Int): String = {
                val nmaxlen = maxlen - reg.getName().size
                if(withshiftmask){
                    val t = reg.getFieldDescrs().map(t => t.define(reg.getName().toUpperCase(), nmaxlen)).filterNot(_.isEmpty).mkString("\n")
                    if(t.isEmpty) "" else "\n" + t
                } else ""
            }
        }

        implicit class FieldDescrCHeadExtend(fd: FieldDescr) {
            def define(pre: String, tabn: Int = 0): String = {
                //add Define  XXX_SHIFT   XXX_MASK  for SW bit operation
                def lsb: Int = fd.getSection().min
                def msb: Int = fd.getSection().max
                def mask = BigInt((1 << fd.getSection().size) - 1) << lsb
                val _tab = " " * (tabn - fd.getName().size)
                fd.getAccessType() match {
                    case `NA` |`RO` |`ROV`                       => ""
                    case `W1S`|`W1C`|`W1T`|`W1P`|`W1CRS`|`W1SRC` => ""
                    case `W0S`|`W0C`|`W0T`|`W0P`|`W0CRS`|`W0SRC` => ""
                    case _ => {
                        s"""#define ${pre}_${fd.getName().toUpperCase()}_SHIFT ${_tab}${lsb}
                           |#define ${pre}_${fd.getName().toUpperCase()}_MASK  ${_tab}0x${mask.hexString(32)} //${fd.getAccessType()}, ${fd.getWidth()} bit""".stripMargin
                    }
                }
            }
        }

        def body() = {
            val maxnamelen = regs.map(_.getName().size).max
            val maxshiftlen = regs.map(t => t.getName().size + t.fdNameLens).max
            def header: String = headers.mkString("\n * ")
            s"""|/*
                | * Reg Interface C-Header [AUTOGENERATE by SpinalHDL]
                | * ${header}
                | */
                |
                |#ifndef ${guardName}
                |#define ${guardName}
                |
                |${regs.map(_.define(maxnamelen, maxshiftlen)).mkString("\n")}
                |
                |${regs.map(_.union).mkString("\n\n")}
                |
                |#endif /* ${guardName} */
                |""".stripMargin
        }
        pw.write(body())
        pw.close()

        def unionregs = {
            //todo: all regbank as union struct for mem copy usage
            """
              |/** Example
              |  * @union      xxx_sys_regbank_block
              |  * @brief
              |  */
              |typedef union {
              |    struct {
              |        m_agl_common_t agl_common    ; // Register description
              |        m_agl_inimg_t  agl_inimg     ; // Register description
              |        u32            reserved0[100]; // reserved 100 reg
              |        m_agl_hscale_t agl_hscale    ; // Register description
              |        u32            reserved1[1]  ; // reserved 1 reg
              |        m_agl_hscale_t agl_hscale    ; // Register description
              |        u32            reserved2[94] ; // reserved 1 reg
              |        m_agl_xxx_t    agl_xxx       ; // Register description
              |    };
              |    u32  reg[200];  //total size 200
              |} xxx_sys_regbank_block_t;
              |""".stripMargin
        }
    }
}
package spinal.lib.bus.regif

import spinal.core.GlobalData
import spinal.lib.bus.regif._
import java.io.PrintWriter

final case class JsonGenerator(fileName : String) extends BusIfVisitor {
    val sb : StringBuilder = new StringBuilder
    var prefix = ""

    def clean(str : String) : String = {
        str.replace("\n","\\n").replace("\r","\\r")
    }
    
    def begin(busDataWidth : Int) : Unit = {
        sb ++= "["
    }

    def visit(descr : BaseDescriptor) : Unit = {
        descr match {
            case descr: RegDescr => regDescrVisit(descr)
            case _ => ???
        }
    }

    private def regDescrVisit(descr: RegDescr) = {
        sb ++= prefix

        sb ++=
          s"""|{
              |   "addr"   : ${descr.getAddr()},
              |   "name"   : "${descr.getName()}",
              |   "doc"    : "${clean(descr.getDoc())}",
              |   "fields" :[\n""".stripMargin

        var fieldPrefix = ""
        descr.getFieldDescrs().foreach(f =>{
            sb ++= fieldPrefix
            sb ++=
              s"""|       {
                  |           "accType" : "${f.getAccessType()}",
                  |           "name"    : "${f.getName()}",
                  |           "width"   : ${f.getWidth()},
                  |           "reset"   : ${f.getResetValue()},
                  |           "doc"     : "${clean(f.getDoc())}"
                  |       }""".stripMargin
            fieldPrefix = ",\n"
        })
        sb ++= "\n"

        sb ++=
          s"""|   ]
              |}""".stripMargin

        prefix = ",\n"
    }
    
    def end() : Unit = {
        val pc = GlobalData.get.phaseContext
        val targetPath = s"${pc.config.targetDirectory}/${fileName}.json"
        val pw = new PrintWriter(targetPath)

        sb ++= "]\n"
        pw.write(sb.toString())

        pw.close()
    }
}
package spinal.lib.bus.mmslavefactory.generators

import spinal.core.GlobalData
import spinal.lib.bus.mmslavefactory._
import java.io.PrintWriter

final case class JsonGenerator(fileName : String) extends MMSlaveFactoryVisitor {
    val sb : StringBuilder = new StringBuilder
    var prefix = ""

    def clean(str : String) : String = {
        str.replace("\n","\\n").replace("\r","\\r")
    }
    
    def begin(busDataWidth : Int) : Unit = {
        sb ++= "["
    }

    def visit(descr : FifoDescr)  : Unit = {

    }

    def visit(descr : RegDescr) : Unit = {
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
        val targetPath = s"${pc.config.targetDirectory}/${fileName}"
        val pw = new PrintWriter(targetPath)

        sb ++= "]\n"
        pw.write(sb.toString())

        pw.close()
    }
}
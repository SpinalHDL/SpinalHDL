package spinal.lib.experimental.hdl
import java.awt.Toolkit
import java.awt.datatransfer.DataFlavor


object VerilogToSpinal extends App{
  val data = Toolkit.getDefaultToolkit().getSystemClipboard().getData(DataFlavor.stringFlavor).toString

  val lineExp = raw"(?<reg>reg)?\s*(?<dir>input|output)\s*(?<range>[\[]([0-9]*):0[\]])?\s*(?<name>[_a-zA-Z0-9]*)\s*[;\n\r,]".r
  println(data)
  println("=====>")
  val matches = lineExp.findAllMatchIn(data)
  for(m <- matches){
    val dir = m.subgroups(1)
    val size = m.subgroups(3)
    val name = m.subgroups(4)
    val dirScala = if(dir == "input") "in" else "out"
    val typeScala = size match {
      case null => "Bool()"
      case _ => s"Bits(1+$size bits)"
    }
    println(s"val $name = $dirScala $typeScala")


  }

  println("DONE")
}

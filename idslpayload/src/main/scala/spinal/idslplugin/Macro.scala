package spinal.idslplugin

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context



class Location(val file : String, val line: Int)

object Location {
  implicit def capture: Location = macro locationMacro

  def locationMacro(x: Context): x.Expr[Location] = {
    import x.universe._

//    val className = Option(x.enclosingClass).map(_.symbol.toString).getOrElse("")
//    val methodName = Option(x.enclosingMethod).map(_.symbol.toString).getOrElse("")
    val pos = x.enclosingPosition
    val line =  pos.line
    val file =  pos.source.toString().replace(".scala", "")
//    val where = s"${line}"
    reify(new Location(x.literal(file).splice, x.literal(line).splice))
  }
}
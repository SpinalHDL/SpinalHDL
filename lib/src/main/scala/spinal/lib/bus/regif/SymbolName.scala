package spinal.lib.bus.regif

import language.experimental.macros
import reflect.macros.blackbox.Context

case class SymbolName(name: String)

object SymbolName{
  implicit def genWhenNeed: SymbolName = macro Macros.symbolNameImpl
//  implicit def stringToSymbolName(s: String) = new SymbolName(s)
//  def apply()(implicit sn:SymbolName):String = sn.name
}

object Macros{
  def symbolNameImpl(c: Context): c.Expr[SymbolName] = {
    import c.universe._
    val symbolName = c.internal.enclosingOwner.name.decodedName.toString.trim
    c.Expr[SymbolName](q"""${c.prefix}($symbolName)""")
  }
}

package spinal.lib.bus.regif

import language.experimental.macros
import reflect.macros.blackbox.Context

case class SymbolName(name: String)
case class ClassName(name: String)

object SymbolName{
  implicit def genWhenNeed: SymbolName = macro Macros.symbolNameImpl
//  implicit def stringToSymbolName(s: String) = new SymbolName(s)
//  def apply()(implicit sn:SymbolName):String = sn.name
}
object ClassName {
  implicit def genWhenNeed: ClassName = macro Macros.classNameImpl
}

object Macros{
  def symbolNameImpl(c: Context): c.Expr[SymbolName] = {
    import c.universe._
    val symbolName = c.internal.enclosingOwner.name.decodedName.toString.trim
    c.Expr[SymbolName](q"""${c.prefix}($symbolName)""")
  }

  def classNameImpl(c: Context): c.Expr[ClassName] = {
    import c.universe._
    def enclosingClass(c: Context)  = {
      def nearestEnclosingClass(owner: c.Symbol): c.Symbol =
        if(owner.isClass) owner.asClass
        else nearestEnclosingClass(owner.owner)
      nearestEnclosingClass(c.internal.enclosingOwner).asClass.toString.trim.split(' ').last
    }
    val className = enclosingClass(c)
    c.Expr[ClassName](q"""${c.prefix}($className)""")
  }
}

package spinal.lib.bus.regif

import spinal.core.Bool

import language.experimental.macros
import reflect.macros.blackbox.Context
//import scala.annotation.StaticAnnotation
//import scala.annotation.compileTimeOnly

case class SymbolName(name: String)
case class ClassName(name: String)

object SymbolName{
  implicit def genWhenNeed: SymbolName = macro Macros.symbolNameImpl
}

object ClassName {
  implicit def genWhenNeed: ClassName = macro Macros.classNameImpl
}

//@compileTimeOnly("enable macro paradise to expand macro annotations")
//class AutoInterrupt extends StaticAnnotation {
//  def macroTransform(annottees: Any*): Bool = macro Macros.autoInterruptImpl
//}

object Macros{
  def symbolNameImpl(c: Context)= {
    import c.universe._
    val symbolName = c.internal.enclosingOwner.name.decodedName.toString.trim
    q"""${c.prefix}($symbolName)"""
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

  def interruptFactoryImpl(c:Context)(busif: c.Tree, regNamePre: c.Tree, triggers: c.Tree*) = {
    import c.universe._
    val creatREG = q"""
        val ENS    = $busif.newReg("Interrupt Enable Reigsiter")(SymbolName($regNamePre+"_INT_ENABLE"))
        val MASKS  = $busif.newReg("Interrupt Mask   Reigsiter")(SymbolName($regNamePre+"_INT_MASK"))
        val STATUS = $busif.newReg("Interrupt status Reigsiter")(SymbolName($regNamePre+"_INT_STATUS"))
        """
    val creatField = triggers.collect {
      case q"$name" =>
        val endName =  name.toString().split('.').last
        val tn_en   = TermName(endName + "_en")
        val tn_mask = TermName(endName + "_mask")
        val tn_state = TermName(endName + "_state")
        val tn_intWithMask = TermName(endName + "intWithMask")
        List(
          q"""val $tn_en = ENS.field(1 bits,AccessType.RW,doc=$endName+" int enable")(SymbolName($endName+"_en"))(0)""",
          q"""val $tn_mask = MASKS.field(1 bits, AccessType.RW, doc=$endName+" int mask")(SymbolName($endName+"_mask"))(0)""",
          q"""val $tn_state = STATUS.field(1 bits, AccessType.RC, doc= $endName+" int status")(SymbolName($endName+"_state"))(0)""",
          q"""val $tn_intWithMask = $tn_mask && $tn_state """,
          q"""when($name && $tn_en) {$tn_state.set()}"""
        )
    }.flatten

    val intMasks = triggers.collect{case q"$name" => val tn = TermName(name.toString().split('.').last+"intWithMask"); q"$tn"}
    val mergeInt = q"""val interrupt = Vec(..$intMasks).asBits.orR; interrupt"""

    q"""{
       ..$creatREG
       ..$creatField
       ..$mergeInt
       }
     """
    }
//
//  def autoInterruptImpl(c:Context)(annottees: c.Tree*) = {
//    import c.universe._
//    annottees match {
//      case q"$mods def $fname($regNamePre,..$triggers):$tpt = {..$expr}" => {
//        val creatREG = q"""
//        val ENS    = newReg("Interrupt Enable Reigsiter")(SymbolName($regNamePre+"_INT_ENABLES"))
//        val MASKS  = newReg("Interrupt Mask   Reigsiter")(SymbolName($regNamePre+"_INT_MASK"))
//        val STATUS = newReg("Interrupt status Reigsiter")(SymbolName($regNamePre+"_INT_STATUS"))
//        """
//        val creatField = triggers.collect {
//          case q"$name" =>
//            val endName =  name.toString().split('.').last
//            val tn_en   = TermName(endName + "_en")
//            val tn_mask = TermName(endName + "_mask")
//            val tn_state = TermName(endName + "_stat")
//            val tn_intWithMask = TermName(endName + "intWithMask")
//            List(
//              q"""val $tn_en = ENS.field(1 bits,AccessType.RW,doc=$endName+" int enable")(SymbolName($endName+"_en"))(0)""",
//              q"""val $tn_mask = MASKS.field(1 bits, AccessType.RW, doc=$endName+" int mask")(SymbolName($endName+"_mask"))(0)""",
//              q"""val $tn_state = STATUS.field(1 bits, AccessType.RC, doc= $endName+" int status")(SymbolName($endName+"_stat"))(0)""",
//              q"""val $tn_intWithMask = $tn_mask && $tn_state """,
//              q"""when($name && $tn_en) {$tn_state.set()}"""
//            )
//        }.flatten
//
//        val intMasks = triggers.collect{case q"$name" => val tn = TermName(name.toString().split('.').last+"intWithMask"); q"$tn"}
//        val mergeInt = q"""val interrupt = Vec(..$intMasks).asBits.orR; interrupt"""
//        q"""$mods def $fname($regNamePre, ..$triggers): $tpt = {
//                ..$expr
//                $creatREG
//                ..$creatField
//                $mergeInt
//             }
//           """
//      }
//      case _ => throw new Exception("Macro annotation Error!")
//    }
//    q"""SpinalError("Macro annotation Error!")"""
//  }

}

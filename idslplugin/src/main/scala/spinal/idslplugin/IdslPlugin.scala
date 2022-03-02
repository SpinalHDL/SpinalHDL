package spinal.idslplugin

import dotty.tools.dotc.report
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Names.PreName
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.unpickleScala2.Scala2Flags
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.{PickleQuotes, Staging}

class IdslPlugin extends StandardPlugin {
  override val name: String = "IdslPlugin"
  override val description: String = "IDSL plugin"

  def init(options: List[String]): List[PluginPhase] = List(new PostInitPhase, new ValCallbackPhase)
}


class ValCallbackPhase extends PluginPhase {
  import tpd.*

  val phaseName = "valCallback phase"

  override val runsAfter = Set(Staging.name)
  override val runsBefore = Set(PickleQuotes.name)

  def symbolHasTrait(s: ClassSymbol, name: String)(implicit ctx: Context): Boolean = {
    if(s.fullName.toString == name) return true
    s.parentSyms.exists {
      case p : ClassSymbol => symbolHasTrait(p, name)
    }
  }

  def typeHasTrait(s: Type, name: String)(implicit ctx: Context): Boolean = {
    s.parentSymbols(_ => true).exists {
      case p : ClassSymbol => (p.fullName.toString == name) || symbolHasTrait(p, name)
    }
  }

  override def transformTemplate(tree: tpd.Template)(implicit ctx: Context): Tree = {
    val cs = ctx.owner.asClass

    //No io bundle without component trait compilation time check
    tree.body.foreach {
      case vd: ValDef if vd.name.toString == "io" && vd.rhs != null && typeHasTrait(vd.rhs.tpe, "spinal.core.Bundle") => {
        if (!symbolHasTrait(cs, "spinal.core.Component") && !symbolHasTrait(cs, "spinal.core.Area") && !symbolHasTrait(cs, "spinal.core.Data") && !symbolHasTrait(cs, "spinal.core.AllowIoBundle")) {
          report.error(s"MISSING EXTENDS COMPONENT\nclass with 'val io = new Bundle{...}' should extends spinal.core.Component", vd.sourcePos)
        }
      }
      case _ =>
    }


    if (symbolHasTrait(cs, "spinal.idslplugin.ValCallback")) {
      val func : TermSymbol = cs.requiredMethod("valCallback")
      tpd.cpy.Template(tree)(body = tree.body.map{
        case vd: ValDef if !vd.symbol.annotations.exists(_.symbol.name.toString == "DontName") && !vd.rhs.isEmpty => { //TODO also, test dontname
          val nameStr = vd.name.toString
          val const = Constant(nameStr)
          val lit = Literal(const)
          val thiz = This(cs)
          val sel = Select(thiz, func.name)
          val typeApply = TypeApply(sel, List(vd.tpt))
          val appl = Apply(typeApply, List(vd.rhs, lit))
          val ret = cpy.ValDef(vd)(rhs = appl)
          ret
        }
        case x => x
      })
    } else {
      tree
    }
  }
}

class PostInitPhase extends PluginPhase {
  import tpd.*

  val phaseName = "postInitPhase"

  override val runsAfter = Set(Staging.name)
  override val runsBefore = Set(PickleQuotes.name)

  def symbolHasTrait(s: ClassSymbol, name: String)(implicit ctx: Context): Boolean = {
    s.parentSyms.exists {
      case p : ClassSymbol => (p.fullName.toString == name) || symbolHasTrait(p, name)
    }
  }

  override def transformApply(a: Apply)(implicit ctx: Context): Tree = {
    var ret: Tree = a
    if(a.fun.symbol.isConstructor){
      val sym = a.fun.symbol.enclosingClass.asInstanceOf[ClassSymbol]
      val tpe = sym.thisType
      sym.parentSyms
      if (symbolHasTrait(sym, "spinal.idslplugin.PostInitCallback")) {
        val avoidIt = a match {
          case Apply(Select(Super(_, _), _), _) => true
          case Apply(Select(This(_), _), _) => true
          case _ => false
        }
        if (!avoidIt) {

          val func = sym.requiredMethod("postInitCallback")
          val sel = Select(a, func.name)
          val appl = Apply(sel, Nil)
          ret = appl
        }
      }
    }
    ret
  }
}
package spinal.idslplugin.components

import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.Trees
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform

class MainTransformer(val global: Global) extends PluginComponent with Transform {

  override val phaseName: String = "idslHelper"

  override val runsAfter: List[String] = List("typer")
  override val runsRightAfter: Option[String] = Some("typer")

  override protected def newTransformer(unit: global.CompilationUnit): global.Transformer = ToStringMaskerTransformer

  import global._

  object ToStringMaskerTransformer extends Transformer {

    def symbolHasAnnotation(s: Symbol, name: String): Boolean = {
      if (s.annotations.exists(_.symbol.name.toString() == name)) return true
      s.parentSymbols.exists(symbolHasAnnotation(_, name))
    }

    override def transform(tree: global.Tree): global.Tree = {
      val transformedTree = super.transform(tree)
      //      println(transformedTree.getClass.toString + " => \n" + transformedTree.toString)
      transformedTree match {
        case cd: ClassDef => {
          var ret: Tree = cd
          if (symbolHasAnnotation(cd.symbol, "valCallback")) {
            val clazz = cd.impl.symbol.owner
            val func = clazz.tpe.members.find(_.name.toString == "valCallback").get
            def call(name: TermName): Tree = {
              val thiz = This(clazz)
              val sel = Select(thiz, func)
              val const = Constant(name.toString)
              val lit = Literal(const)
              val ident = Select(thiz, name)
              val appl = Apply(sel, List(ident, lit))
              ident.tpe = definitions.AnyTpe
              thiz.tpe = clazz.tpe
              sel.tpe = MethodType(List(definitions.AnyTpe.termSymbol, definitions.StringTpe.termSymbol), definitions.UnitTpe)
              appl.tpe = definitions.UnitTpe
              lit.setType(definitions.StringTpe)
              appl
            }

            val body = ArrayBuffer[Tree]()
            cd.impl.body.foreach {
              case vd: ValDef =>
                body += vd; body += call(vd.name)
              case e => body += e
            }
            val impl = treeCopy.Template(cd.impl, cd.impl.parents, cd.impl.self, body.toList)
            val cdNew = treeCopy.ClassDef(cd, cd.mods, cd.name, cd.tparams, impl) //)mods0, name0, tparams0, impl0

            ret = cdNew
          }

          ret
        }
        case a: Apply => {
          var ret: Tree = a
          if (a.fun.symbol.isPrimaryConstructor) {
            val sym = a.fun.symbol.enclClass
            val tpe = sym.typeOfThis
            if (symbolHasAnnotation(sym, "postInitCallback")) {
              val isSuper = a match {
                case Apply(Select(Super(_, _), _), _) => true
                case _ => false
              }
              if (!isSuper) {
                val func = tpe.members.find(_.name.toString == "postInitCallback").get
                val sel = Select(a, func.name)
                val appl = Apply(sel, Nil)
                sel.tpe = MethodType(Nil, a.tpe)
                appl.tpe = a.tpe
                ret = appl
              }
            }
          }
          ret
        }
        case oth => {
          transformedTree
        }
      }
    }
  }
}

package spinal.idslplugin.components

import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.Trees
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform

class MainTransformer(val global: Global) extends PluginComponent with Transform {

  override val phaseName: String = "idsl-plugin"

  override val runsAfter: List[String] = List("uncurry")
  override val runsRightAfter: Option[String] = Some("uncurry")

  override protected def newTransformer(unit: global.CompilationUnit): global.Transformer = ToStringMaskerTransformer

  import global._

  var counter = 0

  object ToStringMaskerTransformer extends Transformer {

    def symbolHasAnnotation(s: Symbol, name: String): Boolean = {
      if (s.annotations.exists(_.symbol.name.toString() == name)) return true
      s.parentSymbols.exists(symbolHasAnnotation(_, name))
    }

    def symbolHasTrait(s: Symbol, name: String): Boolean = {
      s.parentSymbols.exists { p =>
        (p.name.toString == name && p.enclosingPackage.name.toString == "idslplugin") || symbolHasTrait(p, name)
      }
    }

    override def transform(tree: global.Tree): global.Tree = {
      val transformedTree = super.transform(tree)
      //      println(transformedTree.getClass.toString + " => \n" + transformedTree.toString)
      transformedTree match {
        case cd: ClassDef => {
          var ret: Tree = cd
          if (symbolHasTrait(cd.symbol, "ValCallback")) {
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
            if (symbolHasTrait(sym, "PostInitCallback")) {
              val avoidIt = a match {
                case Apply(Select(Super(_, _), _), _) => true
                case Apply(Select(This(_), _), _) => true
                case _ => false
              }
              if (!avoidIt) {
                val func = tpe.members.find(_.name.toString == "postInitCallback").get
                val sel = Select(a, func.name)
                val appl = Apply(sel, Nil)
                sel.tpe = MethodType(Nil, a.tpe)
                appl.tpe = a.tpe
                counter += 1
                ret = appl

//                if(sym.name.toString == "RGB2"){
//                  Thread.sleep(1000)
//                  println()
//                  println()
//                  println()
//                  println()
//                  println()
//                  println()
//                  println("HIT")
//                  tpe.foreach(println(_))
//                  println("=>1")
//                  a.foreach(println(_))
//                  println("=>2")
//                  ret.foreach(println(_))
//                }

                //                if(!a.toString().contains("StreamArbiter")) {
//
//                } else {
//                  println("*************** 1")
//                  println(a)
//                  println("*************** 2")
//                  println(appl)
//                  println("*************** 3")
//                }
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

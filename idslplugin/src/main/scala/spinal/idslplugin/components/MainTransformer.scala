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


  object ToStringMaskerTransformer extends Transformer {

    def symbolHasAnnotation(s: Symbol, name: String): Boolean = {
      if (s.annotations.exists(_.symbol.name.toString() == name)) return true
      s.parentSymbols.exists(symbolHasAnnotation(_, name))
    }

    def symbolHasTrait(s: Symbol, name: String): Boolean = {
      s.parentSymbols.exists { p =>
        (p.fullName == name) || symbolHasTrait(p, name)
      }
    }

    def typeHasTrait(s: Type, name: String): Boolean = {
      s.parents.exists { p =>
        p.toString().toString == name  || typeHasTrait(p, name)
      }
    }


    override def transform(tree: global.Tree): global.Tree = {
      val transformedTree = super.transform(tree)
      //      println(transformedTree.getClass.toString + " => \n" + transformedTree.toString)
      transformedTree match {
        case cd: ClassDef => {
          var ret: Tree = cd

          //No io bundle without component trait compilation time check
          val withIoBundle = cd.impl.body.exists{
            case vd : ValDef if vd.name.toString == "io " && vd.rhs != null && typeHasTrait(vd.rhs.tpe, "spinal.core.Bundle") => true
            case _ => false
          }
          if(withIoBundle && !symbolHasTrait(cd.symbol, "spinal.core.Component") && !symbolHasTrait(cd.symbol, "spinal.core.Area" ) && !symbolHasTrait(cd.symbol, "spinal.core.Data" ) && !symbolHasTrait(cd.symbol, "spinal.core.AllowIoBundle" )){
            global.globalError(cd.symbol.pos, s"MISSING EXTENDS COMPONENT\nclass with 'val io = new Bundle{...}' should extends spinal.core.Component")
          }


          //ValCallback management for class def
          if (symbolHasTrait(cd.symbol, "spinal.idslplugin.ValCallback")) {
            val clazz = cd.impl.symbol.owner
            val func = clazz.tpe.members.find(_.name.toString == "valCallback").get
            val body = cd.impl.body.map {
              case vd: ValDef if !vd.mods.isParamAccessor  && !vd.symbol.annotations.exists(_.symbol.name.toString == "DontName") && vd.rhs.nonEmpty =>
                val nameStr = vd.getterName.toString
                val const = Constant(nameStr)
                val lit = Literal(const)
                val thiz = This(clazz)
                val sel = Select(thiz, func)
                val appl = Apply(sel, List(vd.rhs, lit))

                thiz.tpe = clazz.tpe
                sel.tpe = func.tpe
                appl.tpe = definitions.UnitTpe
                lit.setType(definitions.StringTpe)
                treeCopy.ValDef(vd, vd.mods, vd.name, vd.tpt, appl)
              case e => e
            }
            val impl = treeCopy.Template(cd.impl, cd.impl.parents, cd.impl.self, body)
            val cdNew = treeCopy.ClassDef(cd, cd.mods, cd.name, cd.tparams, impl) //)mods0, name0, tparams0, impl0

            ret = cdNew
          }

          ret
        }
        case a: Apply => {
          var ret: Tree = a

          if (a.fun.symbol.isConstructor) {
            val sym = a.fun.symbol.enclClass
            val tpe = sym.typeOfThis
            if (symbolHasTrait(sym, "spinal.idslplugin.PostInitCallback")) {
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
                ret = appl
              }
            }
          }
          ret
        }

        case cd : ModuleDef => {
          var ret: Tree = cd

          //ValCallback management for objects def, 99
          if (symbolHasTrait(cd.symbol, "spinal.idslplugin.ValCallback")) {
            val clazz = cd.impl.symbol.owner
            val func = clazz.tpe.members.find(_.name.toString == "valCallback").get
            val body = cd.impl.body.map {
              case vd: ValDef if !vd.mods.isParamAccessor && !vd.symbol.annotations.exists(_.symbol.name.toString == "DontName") && vd.rhs.nonEmpty =>
                val nameStr = vd.getterName.toString
                val const = Constant(nameStr)
                val lit = Literal(const)
                val thiz = This(clazz)
                val sel = Select(thiz, func)
                val appl = Apply(sel, List(vd.rhs, lit))

                thiz.tpe = clazz.tpe
                sel.tpe = func.tpe
                appl.tpe = definitions.UnitTpe
                lit.setType(definitions.StringTpe)
                treeCopy.ValDef(vd, vd.mods, vd.name, vd.tpt, appl)
              case e => e
            }
            val impl = treeCopy.Template(cd.impl, cd.impl.parents, cd.impl.self, body)
            val cdNew = treeCopy.ModuleDef(cd, cd.mods, cd.name, impl) //)mods0, name0, tparams0, impl0

            ret = cdNew
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

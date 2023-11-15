package spinal.idslplugin.components

import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.Trees
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform

/**
 * MainTransformer is a plugin component for the Scala compiler.
 * It extends the Transform trait to provide custom AST transformations.
 * 
 * @param global Provides access to the abstract syntax trees (ASTs) and other compiler internals.
 */
class MainTransformer(val global: Global) extends PluginComponent with Transform {

  // The name of this phase in the compiler pipeline.
  override val phaseName: String = "idsl-plugin"

  // Specifies the compiler phases after which this plugin should run.
  override val runsAfter: List[String] = List("uncurry")
  override val runsRightAfter: Option[String] = Some("uncurry")

  // Method to create a new transformer for each compilation unit.
  override protected def newTransformer(unit: global.CompilationUnit): global.Transformer = ToStringMaskerTransformer

  import global._

  /**
   * Transformer object to modify the AST.
   * This object defines custom transformations to be applied to the AST during the compilation.
   */
  object ToStringMaskerTransformer extends Transformer {

    /**
    * Helper method to check if a symbol has a specific annotation.
    */
    def symbolHasAnnotation(s: Symbol, name: String): Boolean = {
      if (s.annotations.exists(_.symbol.name.toString() == name)) return true
      s.parentSymbols.exists(symbolHasAnnotation(_, name))
    }

    /**
    * Helper method to check if a symbol extends a trait with a specific name.
    */
    def symbolHasTrait(s: Symbol, name: String): Boolean = {
      s.parentSymbols.exists { p =>
        (p.fullName == name) || symbolHasTrait(p, name)
      }
    }

    /**
    * Helper method to check if a type extends a trait with a specific name.
    */
    def typeHasTrait(s: Type, name: String): Boolean = {
      s.parents.exists { p =>
        p.toString().toString == name  || typeHasTrait(p, name)
      }
    }

    /**
     * Overrides the transform method to apply custom transformations to the AST.
     * 
     * @param tree The AST node to transform.
     * @return The transformed AST node.
     */
    override def transform(tree: global.Tree): global.Tree = {
      val transformedTree = super.transform(tree)
      
      transformedTree match {
        case cd: ClassDef => { // Perform checks and transformations specific to ClassDef nodes.
          var ret: Tree = cd

          //1. Check that we have no io bundle without component trait (compilation time check)

          // True if body of this class define a value "io" assigned with a non-null Bundle .
          val withIoBundle = cd.impl.body.exists{
            case vd : ValDef if vd.name.toString == "io " && vd.rhs != null && typeHasTrait(vd.rhs.tpe, "spinal.core.Bundle") => true
            case _ => false
          }

          // Issue error if we have a io bundle, but we are NOT a Component, an Area, a Data nor with implement the AllowIOBundle trait.
          if(withIoBundle && !symbolHasTrait(cd.symbol, "spinal.core.Component") && !symbolHasTrait(cd.symbol, "spinal.core.Area" ) && !symbolHasTrait(cd.symbol, "spinal.core.Data" ) && !symbolHasTrait(cd.symbol, "spinal.core.AllowIoBundle" )){
            global.globalError(cd.symbol.pos, s"MISSING EXTENDS COMPONENT\nclass with 'val io = new Bundle{...}' should extends spinal.core.Component")
          }


          //2. ValCallback management
          // ValCallback is a special trait that allow to define a callback that is called for each value
          // It allows Bundle to "know" its values for example (Bundle.elements is the list of Data value defined inside the Bundle)
          if (symbolHasTrait(cd.symbol, "spinal.idslplugin.ValCallback")) {
             // Get the class or trait that owns the current class definition.
            val clazz = cd.impl.symbol.owner

            // Find the 'valCallback' method within the members of the class or trait.
            val func = clazz.tpe.members.find(_.name.toString == "valCallback").get

            // Modify the body of the class definition.
            val body = cd.impl.body.map {
              // Process each ValDef (variable definition) that is not a parameter accessor,
              // doesn't have the 'DontName' annotation, and has a non-empty right-hand side.
              case vd: ValDef if !vd.mods.isParamAccessor  && !vd.symbol.annotations.exists(_.symbol.name.toString == "DontName") && vd.rhs.nonEmpty =>
                // Get the name of the variable.
                val nameStr = vd.getterName.toString
                // Create a constant with the variable's name.
                val const = Constant(nameStr)
                // Create a literal from the constant.
                val lit = Literal(const)
                // Create a 'This' reference to the current class or trait.
                val thiz = This(clazz)
                // Select the 'valCallback' method from the current context.
                val sel = Select(thiz, func)
                // Apply the 'valCallback' method with the original right-hand side and the variable's name.
                val appl = Apply(sel, List(vd.rhs, lit))

                // Set types for new nodes to maintain the AST's integrity.
                thiz.tpe = clazz.tpe
                sel.tpe = func.tpe
                appl.tpe = definitions.UnitTpe
                lit.setType(definitions.StringTpe)

                // Return a modified ValDef with the new application replacing the original right-hand side.
                treeCopy.ValDef(vd, vd.mods, vd.name, vd.tpt, appl)
              // For other elements in the body, keep them as is.
              case e => e
            }
            // Create a new template (class body) with the modified body.
            val impl = treeCopy.Template(cd.impl, cd.impl.parents, cd.impl.self, body)
            // Create a new ClassDef with the modified template.
            val cdNew = treeCopy.ClassDef(cd, cd.mods, cd.name, cd.tparams, impl)
            // Set the modified ClassDef as the return value.
            ret = cdNew
          }

          ret
        }
        case a: Apply => { // Perform checks and transformations specific to Apply nodes.

          var ret: Tree = a

          // Check if the function call (Apply node) is a constructor.
          if (a.fun.symbol.isConstructor) {
            // Get the class symbol of the constructor.
            val sym = a.fun.symbol.enclClass
            // Get the type of the class.
            val tpe = sym.typeOfThis

            // Check if the class has the 'PostInitCallback' trait.
            // Allow to insert a callback after the constructor
            if (symbolHasTrait(sym, "spinal.idslplugin.PostInitCallback")) {
              // Determine if the constructor call should be avoided for transformation.
              // Avoid transformation for constructor calls within super and this references.
              val avoidIt = a match {
                case Apply(Select(Super(_, _), _), _) => true
                case Apply(Select(This(_), _), _) => true
                case _ => false
              }

              // If the constructor call is not to be avoided.
              if (!avoidIt) {
                // Find the 'postInitCallback' method in the class members.
                val func = tpe.members.find(_.name.toString == "postInitCallback").get
                // Select the 'postInitCallback' method from the current Apply node.
                val sel = Select(a, func.name)
                // Apply the 'postInitCallback' method without any arguments.
                val appl = Apply(sel, Nil)

                // Set the type information for the new nodes.
                sel.tpe = MethodType(Nil, a.tpe)
                appl.tpe = a.tpe

                // Set the new Apply node as the return value.
                ret = appl
              }
            }
          }

          // Return the transformed or original Apply node.
          ret
        }
        case oth => {
          // For all other kinds of nodes, return the transformed tree as is.
          transformedTree
        }
      }
    }
  }
}

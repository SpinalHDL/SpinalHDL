package spinal.core
//
//
//import spinal.core.Component
//
import scala.language.experimental.macros

object MacroTest {

//  type H2Db(connString: String) = macro implaa
//  def implaa(c: scala.reflect.macros.whitebox.Context) = {
//    import c.universe._
//
//    q"asd"
//  }
//  def createEnumImpl(c: scala.reflect.macros.whitebox.Context)(ename: c.Expr[String]): c.universe.ModuleDef = {
//    import c.universe._
//
//    val Literal(Constant(s_ename: String)) = ename.tree
//    val oname = TermName(s_ename)
//
//    val barLine = q"val bar: Int = 5"
//    q"object $oname { $barLine }"
//  }
//
//  def createEnum(ename: String): Unit = macro createEnumImpl



}
//  def mkObject(param: Any*) = macro mkObjectImpl
//  def mkObjectImpl(c: Context)(param: c.Expr[Any]*): c.Expr[Any] = {
//    import c.universe._
//    reify { object Toto{
//      def a = println("asd")
//    } }
//
//  }
//  def IfHasImpl[XThenY: c.WeakTypeTag](c: Context): c.Tree = {
//    import c.universe._
//
//    null
//
//  }
//  def impl(c: Context)(url: c.Expr[String]) = {
//    null
//  }
//  type H2Db(url: String) = macro impl
//
//
//  def impl(c: Context)(url: c.Expr[String]): c.Tree = {
//    null
//  }
//
//  // Returns the tree of `a` after the typer, printed as source code.
//  def desugar(a: Any): String = macro desugarImpl
//
//  def desugarImpl(c: Context)(a: c.Expr[Any]) = {
//    import c.universe._
//
//    val s = show(a.tree)
//    c.Expr(
//      Literal(Constant(s))
//    )
//  }
//}
//
////
////import language.experimental.macros
////import scala.reflect.macros.Context
////
////// TODO Error handling (i.e. when parsing fail)
//////
////object scalax {
////  trait Enumerable {
////    type Value <: scalax.Value
////    def values: Seq[Value]
////  }
////
////  trait Value {
////    def name: String
////    override def toString: String = name
////  }
////
////  type Enum(values: _*) = macro Macros.enum
////  type EnumOf[T <: Value](values: _*) = macro Macros.enumOf[T]
////
////  type TypeEnum[TC[_]](instances: _*) = macro Macros.typeEnum[TC]
////
////  case class EnumDef(id: String, name: String)
////
////  object Macros {
////
////    def enum(c: Context)(values: c.Tree*): c.Tree = {
////      import c.universe._
////      import Flag._
////
////      val enumDefs = parseValues(c)(values.toList)
////
////      val Expr(Block(List(valueSealedTrait), _)) = reify {
////        sealed trait Val extends scalax.Value
////      }
////
////      val valueType = TypeDef(Modifiers(OVERRIDE), TypeName("Value"), List(), Ident(TypeName("Val")))
////      val valueTypeTree = Select(This(TypeName(c.enclosingImpl.name.toString)), TypeName("Val"))
////
////      template(c)(valueSealedTrait :: valueType :: valuesList(c)(valueTypeTree, enumDefs) :: valueObjects(c)(valueTypeTree, enumDefs))
////    }
////
////    def enumOf[T : c.WeakTypeTag](c: Context)(values: c.Tree*): c.Tree = {
////      import c.universe._
////      import Flag._
////
////      implicit val context = c
////
////      val tpe = c.weakTypeOf[T]
////      val enumDefs = parseValues(c)(values.toList)
////
////      val valueTypeTree = Ident(tpe.typeSymbol)
////      val valueType = TypeDef(Modifiers(OVERRIDE), TypeName("Value"), List(), Ident(tpe.typeSymbol))
////
////      val generatedCode = valueType ::
////        valuesList(c)(valueTypeTree, enumDefs) ::
////        valueObjects(c)(valueTypeTree, enumDefs)
////
////      template(c)(valueType :: valuesList(c)(valueTypeTree, enumDefs) :: valueObjects(c)(valueTypeTree, enumDefs))
////    }
////
////    def typeEnum[TC[_]](c: Context)(instances: c.Tree*)(implicit tag: c.WeakTypeTag[TC[_]]) = {//: c.Tree = {
////      import c.universe._
////      import Flag._
////
////      val tpe = c.weakTypeOf[TC[_]]
////
////      val generatedCode = instances.collect {
////        // TODO Support package prefix for types
////        case Apply(Ident(TermName(typeName)), List(Block(Tuple2(defs, _)))) => typeName -> defs
////      } map {
////        case (typeName, defs) =>
////          ModuleDef(
////            Modifiers(IMPLICIT),
////            TermName(tpe.typeSymbol.name + typeName),
////            Template(
////              List(AppliedTypeTree(Ident(tpe.typeSymbol), List(Ident(TypeName(typeName))))),
////              emptyValDef,
////              List(
////                DefDef(
////                  Modifiers(),
////                  nme.CONSTRUCTOR,
////                  List(),
////                  List(List()),
////                  TypeTree(),
////                  Block(
////                    List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())),
////                    Literal(Constant(()))
////                  )
////                )
////              ) ++ defs
////            )
////          )
////      }
////
////      val Expr(Block(List(ClassDef(_, _, _, Template(parents, _, body))), _)) = reify {
////        class CONTAINER
////      }
////
////      val Template(_, _, _ :: existingCode) = c.enclosingTemplate
////      Template(parents, emptyValDef, body ++ generatedCode ++ existingCode)
////    }
////
////    private def parseValues(c: Context)(xs: List[c.Tree]): List[(EnumDef, List[c.Tree])] = {
////      import c.universe._
////      xs.collect {
////        case Ident(TermName(id)) => EnumDef(id, id) -> Nil
////        case Apply(Ident(TermName(id)), List(Literal(Constant(name)))) => EnumDef(id, name.toString) -> Nil
////        case Apply(Ident(TermName(id)), args) => EnumDef(id, id) -> args
////      }
////    }
////
////    private def template(c: Context)(generatedCode: List[c.Tree]) = {
////      import c.universe._
////
////      val Expr(Block(List(ClassDef(_, _, _, Template(parents, _, body))), _)) = reify {
////        class CONTAINER extends Enumerable
////      }
////
////      val Template(_, _, _ :: existingCode) = c.enclosingTemplate
////      Template(parents, emptyValDef, body ++ generatedCode ++ existingCode)
////    }
////
////    private def valueObjects(c: Context)(typeTree: c.Tree, enumDefs: List[(EnumDef, List[c.Tree])]): List[c.Tree] = enumDefs.map {
////      case (enumDef, args) =>
////        import c.universe._
////        ModuleDef(
////          Modifiers(),
////          TermName(enumDef.id),
////          Template(
////            List(typeTree),
////            emptyValDef,
////            List(
////              DefDef(
////                Modifiers(),
////                nme.CONSTRUCTOR,
////                List(),
////                List(List()),
////                TypeTree(),
////                Block(
////                  List(
////                    Apply(
////                      Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR),
////                      args
////                    )
////                  ),
////                  Literal(Constant(()))
////                )
////              ),
////              DefDef(Modifiers(), TermName("name"), List(), List(), TypeTree(), Literal(Constant(enumDef.name)))
////            )
////          )
////        )
////    }
////
////    private def valuesList(c: Context)(typeTree: c.Tree, enumDefs: List[(EnumDef, List[c.Tree])]): c.Tree = {
////      import c.universe._
////      ValDef(
////        Modifiers(),
////        TermName("values"),
////        AppliedTypeTree(Ident(TypeName("Seq")), List(typeTree)),
////        Apply(
////          Select(
////            Select(Select(Select(Ident(TermName("scala")), TermName("collection")), TermName("immutable")), TermName("List")),
////            TermName("apply")
////          ),
////          enumDefs.map(_._1).map(enumDef => Ident(TermName(enumDef.id))).toList
////        )
////      )
////    }
////  }
////}
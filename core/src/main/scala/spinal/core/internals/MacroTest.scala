/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
//package spinal.core
////
////
////import spinal.core.Component
////
//import scala.language.experimental.macros
//
//object MacroTest {
//
//  def enum_impl(c: scala.reflect.macros.whitebox.Context)(param: c.Expr[Symbol]*) = {
//    import c.universe._
//    val states = param.toList map { sym =>       //println(showRaw(sym.tree))
//      sym.tree match {
//        case Apply(_, List(Literal(Constant(name: String)))) => TermName(name)
//        case _ =>
//          throw new Exception("Expected list of symbols")
//      }
//    }
//
//    val enumName = TypeName(c.freshName)
//    val objectName = TermName(c.freshName)
//    val cleanObjectName = TermName(c.freshName)
//    val tree = q"""
//      object $objectName extends SpinalEnum{
//        ..${states.map(s => q"val $s = newElement()")}
//      }
//
//      object $cleanObjectName{
//        def enumType = $objectName
//        def apply() = enumType()
//        ..${states.map(s => q"def $s = $objectName.$s")}
//      }
//      $cleanObjectName
//    """
//
//    c.Expr(tree)
//  }
//
//
////    val tree2 = q"""
////      class $enumName extends SpinalEnum{
////        ..${states.map(s => q"val $s = ordered")}
////      }
////      object $cleanObjectName{
////        def enumType = new $enumName()
////        def apply() = enumType.craft()
////        ..${states.map(s => q"def $s = enumType.$s")}
////      }
////      $cleanObjectName
////    """
//
//    def createEnumImpl(c: scala.reflect.macros.blackbox.Context)(ename: c.Expr[String]) = {
//      import c.universe._
//
//      val Literal(Constant(s_ename: String)) = ename.tree
//      val oname = TermName(s_ename)
//
//      val barLine = q"val bar: Int = 5"
//      //q"object $oname { $barLine }"
//      barLine
//    }
//
//    def createEnum(ename: String): Unit = macro createEnumImpl
//
//
//
////  type H2Db(connString: String) = macro implaa
////  def implaa(c: scala.reflect.macros.whitebox.Context) = {
////    import c.universe._
////
////    q"asd"
////  }
////  def createEnumImpl(c: scala.reflect.macros.whitebox.Context)(ename: c.Expr[String]): c.universe.ModuleDef = {
////    import c.universe._
////
////    val Literal(Constant(s_ename: String)) = ename.tree
////    val oname = TermName(s_ename)
////
////    val barLine = q"val bar: Int = 5"
////    q"object $oname { $barLine }"
////  }
////
////  def createEnum(ename: String): Unit = macro createEnumImpl
//
//
//
////  def mkObject(param: Any*) = macro mkObjectImpl
////  def mkObjectImpl(c: scala.reflect.macros.blackbox.Context)(param: c.Expr[Any]*) = {
////    import c.universe._
//////    reify { object Toto{
//////      def a = println("asd")
//////    } }
////    //q"val bar: Int = 5"
////    c.Expr(q"object yolo{}")
////  }
//
//  import scala.language.experimental.macros
//  import scala.reflect.macros.Context
//
//  /* Make an instance of a structural type with the named member. */
//  def bar(name: String*): Any = macro bar_impl
//
//  def bar_impl(c: scala.reflect.macros.whitebox.Context)(name: c.Expr[String]*) = {
//    import c.universe._
//    val anon = TypeName(c.freshName)
//    // next week, val q"${s: String}" = name.tree
//    val Literal(Constant(s: String)) = name(0).tree
//    val A    = TermName(s)
//    val dmmy = TermName(c.freshName)
//    val tree = q"""
//      class $anon {
//        def $A(i: Int): Int = 2 * i
//        def asd = 2
//      }
//      val $dmmy = 0
//      new $anon
//    """
//    // other ploys
//    //(new $anon).asInstanceOf[{ def $A(i: Int): Int }]
//    // reference the member
//    //val res = new $anon
//    //val $dmmy = res.$A _
//    //res
//    // the canonical ploy
//    //new $anon { }  // braces required
//    c.Expr(tree)
//  }
//
//      /* Make an instance of a structural type with the named member. */
////  def bar2[a]()= macro bar2_impl[a]()
////
////  def bar2_impl[ a : c.WeakTypeTag ](c: scala.reflect.macros.whitebox.Context)() = {
////    import c.universe._
////
////      import Flag._
//////    val tree = c.Expr[Unit](
//////      q"val a = 2"
//////    )
//////    tree
////      //val cc = reflect.runtime.universe.reify(for (i <- 1 to 10) yield i * 2).tree
////      import scala.reflect.runtime.{universe => ru}
////      ru.showRaw{ru.reify{val x = 5}}
////      c.Expr[Unit](
////        ValDef(
////          Modifiers(IMPLICIT),
////          newTermName("derivedShowableInstance"),
////          TypeTree(),
////          c.universe.reify(2).tree
////        )
////      )
////      ru.reify{val x = 5}
////
////  }
//
//
//
//  /* Make an instance of a structural type with the named member. */
//  def MyEnum(): Any = macro MyEnum_impl
//
//  def MyEnum_impl(c: scala.reflect.macros.whitebox.Context)() = {
//    import c.universe._
//    val anon = TypeName(c.freshName)
//    // next week, val q"${s: String}" = name.tree
//
//    val dmmy = TermName(c.freshName)
//    val tree = q"""
//      class $anon extends SpinalEnum{
//        val s0,s1,s2 = ordered
//      }
//      val $dmmy = 0
//      new $anon
//    """
//    // other ploys
//    //(new $anon).asInstanceOf[{ def $A(i: Int): Int }]
//    // reference the member
//    //val res = new $anon
//    //val $dmmy = res.$A _
//    //res
//    // the canonical ploy
//    //new $anon { }  // braces required
//    c.Expr(tree)
//  }
//
//  /*def macroFile: Any = macro macroFile_impl
//
//  def macroFile_impl(c: scala.reflect.macros.whitebox.Context) = {
//    import c.universe._
//    val str = "toto"
//    c.Expr(q"""
//
//      object $str{
//
//      }
//      $str
//    """)
//  }  */
//
//    /* Make an instance of a structural type with the named member. */
//  //def enum(param: Symbol*): Any = macro enum_impl
//
//
//
//  def enum2(param: Symbol*): Any = macro enum2_impl
//
//  def enum2_impl(c: scala.reflect.macros.whitebox.Context)(param: c.Expr[Symbol]*) = {
//    import c.universe._
//    val symboles = param.toList map { sym =>       //println(showRaw(sym.tree))
//      sym.tree match {
//        case Apply(_, List(Literal(Constant(name: String)))) => TermName(name)
//        case _ =>
//          throw new Exception("Expected list of symbols")
//      }
//    }
//    val objectName = symboles.head
//    val states = symboles.tail
//
//    val enumName = TypeName(c.freshName)
//  //  val objectName = TermName(c.freshName)
//    val cleanObjectName = TermName(c.freshName)
//    val tree = q"""
//      object $objectName extends SpinalEnum{
//        ..${states.map(s => q"val $s = ordered")}
//      }
//      object $cleanObjectName{
//        def enumType = $objectName
//        def apply() = enumType()
//        ..${states.map(s => q"def $s = $objectName.$s")}
//      }
//      $cleanObjectName
//    """
//    c.Expr(tree)
//  }
//
//}
////  def mkObject(param: Any*) = macro mkObjectImpl
////  def mkObjectImpl(c: Context)(param: c.Expr[Any]*): c.Expr[Any] = {
////    import c.universe._
////    reify { object Toto{
////      def a = println("asd")
////    } }
////
////  }
////  def IfHasImpl[XThenY: c.WeakTypeTag](c: Context): c.Tree = {
////    import c.universe._
////
////    null
////
////  }
////  def impl(c: Context)(url: c.Expr[String]) = {
////    null
////  }
////  type H2Db(url: String) = macro impl
////
////
////  def impl(c: Context)(url: c.Expr[String]): c.Tree = {
////    null
////  }
////
////  // Returns the tree of `a` after the typer, printed as source code.
////  def desugar(a: Any): String = macro desugarImpl
////
////  def desugarImpl(c: Context)(a: c.Expr[Any]) = {
////    import c.universe._
////
////    val s = show(a.tree)
////    c.Expr(
////      Literal(Constant(s))
////    )
////  }
////}
////
//////
//////import language.experimental.macros
//////import scala.reflect.macros.Context
//////
//
////////
//////object scalax {
//////  trait Enumerable {
//////    type Value <: scalax.Value
//////    def values: Seq[Value]
//////  }
//////
//////  trait Value {
//////    def name: String
//////    override def toString: String = name
//////  }
//////
//////  type Enum(values: _*) = macro Macros.enum
//////  type EnumOf[T <: Value](values: _*) = macro Macros.enumOf[T]
//////
//////  type TypeEnum[TC[_]](instances: _*) = macro Macros.typeEnum[TC]
//////
//////  case class EnumDef(id: String, name: String)
//////
//////  object Macros {
//////
//////    def enum(c: Context)(values: c.Tree*): c.Tree = {
//////      import c.universe._
//////      import Flag._
//////
//////      val enumDefs = parseValues(c)(values.toList)
//////
//////      val Expr(Block(List(valueSealedTrait), _)) = reify {
//////        sealed trait Val extends scalax.Value
//////      }
//////
//////      val valueType = TypeDef(Modifiers(OVERRIDE), TypeName("Value"), List(), Ident(TypeName("Val")))
//////      val valueTypeTree = Select(This(TypeName(c.enclosingImpl.name.toString)), TypeName("Val"))
//////
//////      template(c)(valueSealedTrait :: valueType :: valuesList(c)(valueTypeTree, enumDefs) :: valueObjects(c)(valueTypeTree, enumDefs))
//////    }
//////
//////    def enumOf[T : c.WeakTypeTag](c: Context)(values: c.Tree*): c.Tree = {
//////      import c.universe._
//////      import Flag._
//////
//////      implicit val context = c
//////
//////      val tpe = c.weakTypeOf[T]
//////      val enumDefs = parseValues(c)(values.toList)
//////
//////      val valueTypeTree = Ident(tpe.typeSymbol)
//////      val valueType = TypeDef(Modifiers(OVERRIDE), TypeName("Value"), List(), Ident(tpe.typeSymbol))
//////
//////      val generatedCode = valueType ::
//////        valuesList(c)(valueTypeTree, enumDefs) ::
//////        valueObjects(c)(valueTypeTree, enumDefs)
//////
//////      template(c)(valueType :: valuesList(c)(valueTypeTree, enumDefs) :: valueObjects(c)(valueTypeTree, enumDefs))
//////    }
//////
//////    def typeEnum[TC[_]](c: Context)(instances: c.Tree*)(implicit tag: c.WeakTypeTag[TC[_]]) = {//: c.Tree = {
//////      import c.universe._
//////      import Flag._
//////
//////      val tpe = c.weakTypeOf[TC[_]]
//////
//////      val generatedCode = instances.collect {
//////        //
//////        case Apply(Ident(TermName(typeName)), List(Block(Tuple2(defs, _)))) => typeName -> defs
//////      } map {
//////        case (typeName, defs) =>
//////          ModuleDef(
//////            Modifiers(IMPLICIT),
//////            TermName(tpe.typeSymbol.name + typeName),
//////            Template(
//////              List(AppliedTypeTree(Ident(tpe.typeSymbol), List(Ident(TypeName(typeName))))),
//////              emptyValDef,
//////              List(
//////                DefDef(
//////                  Modifiers(),
//////                  nme.CONSTRUCTOR,
//////                  List(),
//////                  List(List()),
//////                  TypeTree(),
//////                  Block(
//////                    List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())),
//////                    Literal(Constant(()))
//////                  )
//////                )
//////              ) ++ defs
//////            )
//////          )
//////      }
//////
//////      val Expr(Block(List(ClassDef(_, _, _, Template(parents, _, body))), _)) = reify {
//////        class CONTAINER
//////      }
//////
//////      val Template(_, _, _ :: existingCode) = c.enclosingTemplate
//////      Template(parents, emptyValDef, body ++ generatedCode ++ existingCode)
//////    }
//////
//////    private def parseValues(c: Context)(xs: List[c.Tree]): List[(EnumDef, List[c.Tree])] = {
//////      import c.universe._
//////      xs.collect {
//////        case Ident(TermName(id)) => EnumDef(id, id) -> Nil
//////        case Apply(Ident(TermName(id)), List(Literal(Constant(name)))) => EnumDef(id, name.toString) -> Nil
//////        case Apply(Ident(TermName(id)), args) => EnumDef(id, id) -> args
//////      }
//////    }
//////
//////    private def template(c: Context)(generatedCode: List[c.Tree]) = {
//////      import c.universe._
//////
//////      val Expr(Block(List(ClassDef(_, _, _, Template(parents, _, body))), _)) = reify {
//////        class CONTAINER extends Enumerable
//////      }
//////
//////      val Template(_, _, _ :: existingCode) = c.enclosingTemplate
//////      Template(parents, emptyValDef, body ++ generatedCode ++ existingCode)
//////    }
//////
//////    private def valueObjects(c: Context)(typeTree: c.Tree, enumDefs: List[(EnumDef, List[c.Tree])]): List[c.Tree] = enumDefs.map {
//////      case (enumDef, args) =>
//////        import c.universe._
//////        ModuleDef(
//////          Modifiers(),
//////          TermName(enumDef.id),
//////          Template(
//////            List(typeTree),
//////            emptyValDef,
//////            List(
//////              DefDef(
//////                Modifiers(),
//////                nme.CONSTRUCTOR,
//////                List(),
//////                List(List()),
//////                TypeTree(),
//////                Block(
//////                  List(
//////                    Apply(
//////                      Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR),
//////                      args
//////                    )
//////                  ),
//////                  Literal(Constant(()))
//////                )
//////              ),
//////              DefDef(Modifiers(), TermName("name"), List(), List(), TypeTree(), Literal(Constant(enumDef.name)))
//////            )
//////          )
//////        )
//////    }
//////
//////    private def valuesList(c: Context)(typeTree: c.Tree, enumDefs: List[(EnumDef, List[c.Tree])]): c.Tree = {
//////      import c.universe._
//////      ValDef(
//////        Modifiers(),
//////        TermName("values"),
//////        AppliedTypeTree(Ident(TypeName("Seq")), List(typeTree)),
//////        Apply(
//////          Select(
//////            Select(Select(Select(Ident(TermName("scala")), TermName("collection")), TermName("immutable")), TermName("List")),
//////            TermName("apply")
//////          ),
//////          enumDefs.map(_._1).map(enumDef => Ident(TermName(enumDef.id))).toList
//////        )
//////      )
//////    }
//////  }
//////}
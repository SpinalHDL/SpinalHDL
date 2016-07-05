package spinal.lib.experimental

import scala.language.experimental.macros

class MacrosClass{
  var toto = 2
  def doit(ename: String) : (Int) => Int = macro Macros.doit
  def doit2(func : (Int) => Int) : (Int) => Int = macro Macros.doit2
  def doit3(func : (Int) => Int) : (Int) => Int = macro Macros.doit3
}

object Macros {
  
    def doit(c: scala.reflect.macros.blackbox.Context)(ename: c.Expr[String]) = {
      import c.universe._

      val Literal(Constant(s_ename: String)) = ename.tree
      val oname = TermName(s_ename)

      val barLine = q"(x : Int) => 3 + x"
      //q"object $oname { $barLine }"
      barLine
    }
    
    def doit2(c: scala.reflect.macros.blackbox.Context)(func: c.Expr[(Int) => Int]) = {
      import c.universe._

  

      val barLine = q"(x : Int) => 3 + x"
      //q"object $oname { $barLine }"
      barLine
    }
    
    def doit3(c: scala.reflect.macros.blackbox.Context)(func: c.Expr[(Int) => Int]) = {
      import c.universe._

  

     // val barLine = q"val aa = 2; (x : Int) => 3 + x + aa;"
      val barLine = q"val aa = 2; $func"
      //q"object $oname { $barLine }"
      barLine
    }
    
    
    
    
//  def forM(header: _)(body: _) : Unit = macro __forM
//  
//  def __forM(c: Context)(header: c.Tree)(body: c.Tree): c.Tree = {
//    import c.universe._
//    header match {
//      case Block(
//        List(
//          Assign(Ident(TermName(name)), Literal(Constant(start))),
//          Apply(Select(Ident(TermName(name2)), TermName(comparison)), List(Literal(Constant(end))))
//        ),
//        Apply(Select(Ident(TermName(name3)), TermName(incrementation)), List(Literal(Constant(inc))))
//      ) =>
//  
//      // here one can generate the behavior of the loop
//      // but omit full implementation for clarity now ...
//  
//    }
//  }
}


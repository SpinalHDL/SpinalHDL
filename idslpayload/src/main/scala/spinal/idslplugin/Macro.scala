package spinal.idslplugin

import scala.quoted._

//trait LocationPlatform {
//  implicit inline def instance: Location =
//    ${LocationPlatform.Location_impl}
//}
//
//object LocationPlatform {
//  def Location_impl(using ctx: Quotes): Expr[Location] = {
//    val rootPosition = ctx.reflect.Position.ofMacroExpansion
//    val file = Expr(rootPosition.sourceFile.name.replace(".scala", ""))
//    val line = Expr(rootPosition.startLine + 1)
//    '{Location($file, $line)}
//  }
//}
//
//final case class Location(file: String, line: Int) {
//  override def toString =
//    s"$file:$line"
//}
//
//object Location extends LocationPlatform

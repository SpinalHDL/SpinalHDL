package spinal.idslplugin

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

/**
 * Represents a source code location.
 *
 * This class is used to capture and represent the location of a piece of code in the source files.
 * It includes information about the file name, line number, and column number.
 *
 * @param file The name of the source file.
 * @param line The line number in the source file.
 * @param col  The column number in the source file.
 */
class Location(val file: String, val line: Int, val col: Int)

object Location {
  /**
   * Captures the current source code location.
   *
   * This method uses a Scala macro to capture the location in the source code where it is called.
   * It is useful for debugging, logging, or providing more informative error messages.
   *
   * @return A Location instance representing the current position in the source code.
   */
  implicit def capture: Location = macro locationMacro

  /**
   * Macro implementation to capture the source code location.
   *
   * @param x The macro context used by the Scala compiler.
   * @return An expression that evaluates to a Location instance.
   */
  def locationMacro(x: Context): x.Expr[Location] = {
    import x.universe._

    // Capture the position in the source code where the macro is called.
    val pos = x.enclosingPosition
    val line = pos.line
    val col = pos.column

    // Extract the source file name, removing the ".scala" extension.
    val file = pos.source.toString().replace(".scala", "")

    // Create and return an expression that evaluates to a new Location instance.
    reify(new Location(x.literal(file).splice, x.literal(line).splice, x.literal(col).splice))
  }
}

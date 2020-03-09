package spinal.idslplugin

import spinal.idslplugin.components.MainTransformer

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{ Plugin, PluginComponent }

class IdslPlugin(val global: Global) extends Plugin {
  override val name: String = "idsl-plugin"
  override val description: String = "IDSL plugin"
  override val components: List[PluginComponent] = List(new MainTransformer(global))
}

package spinal.idslplugin

import spinal.idslplugin.components.MainTransformer

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{ Plugin, PluginComponent }

class Idslcp(val global: Global) extends Plugin {

  override val name: String = "idslcp"
  override val description: String = "StringMask compiler plugin"
  override val components: List[PluginComponent] = List(new MainTransformer(global))
}

package spinal.lib.eda.fusesoc

import spinal.core._
import com.fasterxml.jackson.core.{JsonFactory, JsonParser}
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.{YAMLFactory, YAMLGenerator}
import com.fasterxml.jackson.module.scala._

abstract class FuseSocGeneratorBuilder[P, C<:Component](componentName: String, defaultPram: P) {
  private val yamlMapper = new ObjectMapper(new YAMLFactory().disable(YAMLGenerator.Feature.WRITE_DOC_START_MARKER))
  yamlMapper.registerModule(DefaultScalaModule)
  private val jsonMapper = new ObjectMapper(new JsonFactory().configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true).configure(JsonParser.Feature.INCLUDE_SOURCE_IN_LOCATION, true))
  jsonMapper.registerModule(DefaultScalaModule)


  private case class P2SParam(spinal_parameter: P,
                              output: Object = Map("files"->List(Map(componentName+".v"->new Object{val file_type: String = "verilogSource"}))),
                              target_directory: String = "./generate",
                              entry_function: String = "",
                              copy_core: Boolean=false,
                              spinal_project_path: String = ".")


  private val CoreFile = new Object {
    val name: String = s"::$componentName:0.0.0"
    val filesets: Object = new Object{
      val base: Map[String, List[String]] = Map("depend"->List("chenbosoft:utils:generators:0.0.0"))
    }
    val generate: Map[String, Object] = Map(componentName+"_gen"->new Object{
      val generator = "spinalhdl"
      val parameters: P2SParam = P2SParam(defaultPram)
    })
  }

  def buildScript: String = {
    "CAPI=2:\n\n"+yamlMapper.writeValueAsString(CoreFile)+
      s"""
         |targets:
         |  lint:
         |    default_tool : verilator
         |    generate: [${componentName+"_gen"}]
         |    filesets: [base]
         |    tools:
         |      verilator:
         |        mode : lint-only
         |    toplevel : $componentName
         |
         |""".stripMargin
  }

  def buildComponent(parameter: P): C

  def run(args: Array[String]): Unit = {
    val p = getParameter(args)
    val config = SpinalConfig(targetDirectory = p.target_directory)
    SpinalVerilog(config)(buildComponent(p.spinal_parameter))
  }

  private def getParameter(args: Array[String]): P2SParam = {
    val builder = scopt.OParser.builder[P2SParam]
    val parser = scopt.OParser.sequence(
      builder.opt[String]("spinal_parameter").required().action((x,c)=>c.copy(spinal_parameter=jsonMapper.readValue(x, defaultPram.getClass))),
      builder.opt[String]("target_directory").required().action((x,c)=>c.copy(target_directory = x))
    )
    scopt.OParser.parse(parser, args, P2SParam(defaultPram)).getOrElse(throw new Exception(""))
  }
}


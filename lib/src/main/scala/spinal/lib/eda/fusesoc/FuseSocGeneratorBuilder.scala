package spinal.lib.eda.fusesoc
import spinal.core._
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.{YAMLFactory, YAMLGenerator}
import com.fasterxml.jackson.module.scala._

import scala.collection.immutable.Map.Map1
import scala.collection.mutable
import scala.reflect.ClassTag

class FusesocGeneratorOutput() {
  val parameters = mutable.Map[String, String]()
  // List(file_name -> {file_type -> verilogSource})
  val files = mutable.ArrayBuffer[Map1[String, Map[String, String]]]()
}
case class FusesocGeneratorParameters[P](
    // for python only
    entry_function: String = "",
    copy_core: Boolean = false,
    spinal_project_path: String = ".",
    // for scala only
    target_directory: String = "./generate",
    output: FusesocGeneratorOutput,
    spinal_parameter: P
)

abstract class FusesocRunner[C <: Component, P: ClassTag] {
  private val yamlMapper = new ObjectMapper(new YAMLFactory().disable(YAMLGenerator.Feature.WRITE_DOC_START_MARKER))
    .registerModule(DefaultScalaModule) :: ClassTagExtensions

  def run(
      args: Array[String],
      defaultConfigForClockDomains: ClockDomainConfig = ClockDomainConfig()
  ): SpinalReport[C] = {
    val corePath = parseCli(args)
    val file = new java.io.FileReader(corePath)

    val p = parseCore(file)
    val sourceOutput =
      p.output.files.filter(_.head._2.exists(attr => attr._1 == "file_type" && FusesocUtil.checkAvailFileType(attr._2)))
    assert(sourceOutput.size == 1, "only support 1 output source file")
    assert(p.target_directory.startsWith("/"), s"target_directory: ${p.target_directory} why is not absolute?")

    val targetDirectory = p.target_directory
    val mode = sourceOutput.head.head._2.head._2 match {
      case "verilogSource"       => spinal.core.Verilog
      case "systemVerilogSource" => spinal.core.SystemVerilog
      case "vhdlSource"          => spinal.core.VHDL
      case _ => throw new Exception(s"file_type: ${p.output.files.head.head._2.head._2} not supported filetype")
    }
    val netlistFileName = sourceOutput.head.head._1

    val config = SpinalConfig(
      mode = mode,
      targetDirectory = targetDirectory,
      netlistFileName = netlistFileName,
      defaultConfigForClockDomains = defaultConfigForClockDomains
    )
    config.generate(buildComponent(p.spinal_parameter))
  }

  def buildComponent(parameter: P): C

  private def parseCore(reader: java.io.Reader): FusesocGeneratorParameters[P] =
    yamlMapper.readValue[FusesocGeneratorParameters[P]](reader)

  private def parseCli(args: Array[String]): String = {
    val builder = scopt.OParser.builder[String]
    val parser = scopt.OParser.sequence(
      builder.opt[String]("core_file_path").required().action((x, _) => x)
    )
    scopt.OParser.parse(parser, args, null).getOrElse(throw new Exception(""))
  }

}

class GeneratorBuilder[C <: Component, P: ClassTag](
    componentName: String,
    defaultPram: P,
    descriptions: String = "",
    version: String = "0.0.0",
    generator: String = "chenbosoft:utils:generators:0.0.1"
) {
  private val yamlMapper = new ObjectMapper(new YAMLFactory().disable(YAMLGenerator.Feature.WRITE_DOC_START_MARKER))
  yamlMapper.registerModule(DefaultScalaModule)

  def addParameters(k: String, v: String): this.type = {
    output.parameters.update(k, v)
    this
  }

  def addFiles(file_name: String, file_type: String): this.type = {
    output.files.append(new Map1(file_name, Map("file_type" -> file_type)))
    this
  }
  private val output = new FusesocGeneratorOutput

  // default add a source file(which will be generated)
  addFiles(componentName + ".v", "verilogSource")

  private val CoreFile = new Object {
    val name: String = s"::$componentName:$version"
    val description: String = descriptions
    val filesets = new Object {
      val base: Map[String, List[String]] = Map("depend" -> List(generator))
    }
    val generate = Map((componentName + "_gen") -> new Object {
      val generator = "spinalhdl"
      val parameters: FusesocGeneratorParameters[P] =
        FusesocGeneratorParameters(spinal_parameter = defaultPram, output = output)
    })
  }

  def buildScript: String = {
    "CAPI=2:\n\n" + yamlMapper.writeValueAsString(CoreFile) +
      s"""
         |targets:
         |  lint:
         |    default_tool : verilator
         |    generate: [${componentName + "_gen"}]
         |    filesets: [base]
         |    tools:
         |      verilator:
         |        mode : lint-only
         |    toplevel : $componentName
         |
         |""".stripMargin +
      "# get generator: `fusesoc library add spinal_generator https://github.com/chenbo-again/spinalhdl_fusesoc_ generator`"
  }
}

object FusesocUtil {
  def checkAvailFileType(file_type: String): Boolean = {
    file_type == "verilogSource" || file_type == "systemVerilogSource" || file_type == "vhdlSource"
  }
}

package spinal.lib.eda.fusesoc
import spinal.core._
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.{YAMLFactory, YAMLGenerator}
import com.fasterxml.jackson.module.scala._
import scala.collection.mutable
import scala.reflect.ClassTag

class FusesocOutput() {
  val parameters = mutable.Map[String, String]()
  val files = mutable.ArrayBuffer[Map[String, Map[String, String]]]()

  def addParameters(k: String, v: String): Unit = {
    parameters.update(k, v)
  }

  def addFiles(file_name: String, file_type: String): Unit = {

    files.append(Map(file_name -> Map("file_type" -> file_type)))
  }

}
case class P2SParam[P: ClassTag](
    // for python only
    entry_function: String = "",
    copy_core: Boolean = false,
    spinal_project_path: String = ".",
    // for scala only
    target_directory: String = "./generate",
    output: FusesocOutput,
    spinal_parameter: P
)

abstract class FusesocRunner[C <: Component, P: ClassTag] {
  private val yamlMapper = new ObjectMapper(new YAMLFactory().disable(YAMLGenerator.Feature.WRITE_DOC_START_MARKER))
    .registerModule(DefaultScalaModule) :: ClassTagExtensions

  def run(args: Array[String]): Unit = {
    val corePath = parseCli(args)
    val file = new java.io.FileReader(corePath)

    val p = parseCore(file)
    assert(p.output.files.size == 1, "only support 1 output file only")
    assert(p.target_directory.startsWith("/"), s"target_directory: ${p.target_directory} why is not absolute?")

    val targetDirectory = p.target_directory
    val mode = p.output.files.head.head._2.head._2 match {
      case "verilogSource"       => spinal.core.Verilog
      case "systemVerilogSource" => spinal.core.SystemVerilog
      case "vhdlSource"          => spinal.core.VHDL
      case _ => throw new Exception(s"file_type: ${p.output.files.head.head._2.head._2} not supported filetype")
    }
    val config = SpinalConfig(mode = mode, targetDirectory = targetDirectory)
    config.generate(buildComponent(p.spinal_parameter))
  }

  def buildComponent(parameter: P): C

  private def parseCore(reader: java.io.Reader): P2SParam[P] =
    yamlMapper.readValue[P2SParam[P]](reader)

  private def parseCli(args: Array[String]): String = {
    val builder = scopt.OParser.builder[String]
    val parser = scopt.OParser.sequence(
      builder.opt[String]("core_file_path").required().action((x, _) => x)
    )
    scopt.OParser.parse(parser, args, null).getOrElse(throw new Exception(""))
  }

}

class FusesocGeneratorBuilder[C <: Component, P: ClassTag](
    componentName: String,
    defaultPram: P,
    descriptions: String = "",
    version: String = "0.0.0"
) {
  private val yamlMapper = new ObjectMapper(new YAMLFactory().disable(YAMLGenerator.Feature.WRITE_DOC_START_MARKER))
  yamlMapper.registerModule(DefaultScalaModule)

  private val output = new FusesocOutput
  output.addFiles(componentName + ".v", "verilogSource")

  private val CoreFile = new Object {
    val name: String = s"::$componentName:$version"
    val description: String = descriptions
    val filesets = new Object {
      val base: Map[String, List[String]] = Map("depend" -> List("chenbosoft:utils:generators:0.0.0"))
    }
    val generate = Map((componentName + "_gen") -> new Object {
      val generator = "spinalhdl"
      val parameters: P2SParam[P] = P2SParam(spinal_parameter = defaultPram, output = output)
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

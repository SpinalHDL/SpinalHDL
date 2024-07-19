package spinal.tester.code

import spinal.core._
import spinal.core.internals.{MemBlackboxOf, PhaseContext, PhaseNetlist}
import spinal.lib._

import java.io.{File, FileWriter}

class Toplevel96637 extends Component {
  val xy = new Component {
    val rom = Mem.fill(256)(Bits(32 bits))
    rom.initBigInt((255 downto 0).map(v => BigInt(v)))
    val read = slave(rom.readSyncPort())
  }
  val read = xy.read.toIo
}

class CaseRomPhase extends PhaseNetlist{
  override def impl(pc: PhaseContext): Unit = {
    pc.walkComponents{
      case c : Rom_1rs => {
        c.setDefinitionName(c.getRtlPath("_")) // provide a unique name
        val rom = c.getTag(classOf[MemBlackboxOf]).get.mem
        val content = rom.initialContent

        c.genericElements.clear()
        val addressWidth = log2Up(c.wordCount)
        val lines = for((data, addr) <- content.zipWithIndex) yield {
          f"        $addressWidth'h${addr}%x: data <= ${c.wordWidth}%d'h${data}%x"
        }
        val verilog =
          s"""module ${c.definitionName} (
             |  input  wire clk,
             |  input  wire en,
             |  output wire [${addressWidth-1}:0] addr,
             |  input  wire [${c.wordWidth-1}:0] data
             |);
             |  always @(posedge clk) begin
             |    if(en) begin
             |      case (addr)
             |${lines.mkString("\n")}
             |      endcase
             |    end
             |  end
             |endmodule
             |
             |""".stripMargin

        val fileWriter = new FileWriter(new File(c.definitionName + ".v"))
        fileWriter.write(verilog)
        fileWriter.close()
      }
      case _ =>
    }
  }
}

object Toplevel96637Gen extends App{
  val config = SpinalConfig()
  config.addStandardMemBlackboxing(blackboxAll)
  config.memBlackBoxers += new CaseRomPhase
  config.generateVerilog(new Toplevel96637)
}

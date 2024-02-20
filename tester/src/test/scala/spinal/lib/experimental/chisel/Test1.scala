package spinal.lib.experimental.chisel


object Test1 {
  class TopLevel extends Module{
    val io = new Bundle{
      val a,b,c = new Bool().asInput()
      val result = new Bool().asOutput()
    }
    io.result := io.a || io.b || io.c
  }
  def main(args: Array[String]) {
    spinal.core.SpinalVerilog(new TopLevel)
  }
}

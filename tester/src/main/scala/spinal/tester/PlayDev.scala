package spinal.tester

import spinal.core._
import spinal.lib._


object PlayDevMem{
  class TopLevel extends Component {
    val mem = Mem(Bits(32 bits), 64)
    val p0 = new Bundle{
      val address = in(mem.addressType)
      val data = in(mem.wordType)
      val mask = in(Bits(4 bits))
      val enable = in Bool()
    }

    mem.write(p0.address, p0.data,mask = p0.mask)


    val p1 = new Bundle{
      val address = in(mem.addressType)
      val data = out(mem.wordType)
    }
    p1.data := mem.readSync(p1.address, True)


    val p2 = new Bundle{
      val address = in(mem.addressType)
      val data = out(mem.wordType)
    }
    p2.data := mem.readAsync(p2.address)
    val xx = RegNext(True)

    println(LatencyAnalysis(p0.address, p2.data))

    val p3 = new Bundle{
      val address = in(mem.addressType)
      val data = in(mem.wordType)
      val mask = in(Bits(4 bits))
      val enable = in Bool()
      val wr = in Bool()
    }

    mem.readWriteSync(
      address = p3.address  ,
      data = p3.data  ,
      enable = p3.enable  ,
      write = p3.wr  ,
      mask = p3.mask
    )

  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
  }
}



object PlayDevLatency{
  class TopLevel extends Component {
    val a,b = in Bool()
    val tmp = Reg(Bool)
    val tmp2 = Bool
    val tmp3 = Reg(Bool)


   // tmp := a
    when(RegNext(!a)) {
      tmp2 := RegNext(tmp)
    }
    tmp3 := Delay(tmp2,5)

    println(LatencyAnalysis(a, tmp3))
    System.exit(0)
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
  }
}


object PlayDevPullCkock{
  class SubSub extends Component{
    val x = RegNext(True) init(False)
  }
  class Sub extends Component{
    val subsub = new SubSub
  }
  class TopLevel extends Component {
    val cd = ClockDomain(Bool, Bool)
    val sub = cd(new Sub)
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
  }
}

object PlayDevMaskedCase{

  class TopLevel extends Component {
    val sel = in Bits(4 bits)
    val result = out Bits(8 bits)

    switch(sel){
      is(M"---1"){
        result := 0
      }
      is(M"--1-"){
        result := 1
      }
      is(M"-1--"){
        result := 2
      }
      default{
        result := 3
      }
    }
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
  }
}


object PlayDevMemReadWrite{

  class TopLevel extends Component {
    val mem = Mem(Bits(8 bits), 64)
    val address = UInt(6 bits)
    val wrData, rdData = Bits(8 bits)
    val en, wr = Bool
    rdData := mem.readWriteSync(address,wrData,en,wr)
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
  }
}

object PlayDevFillSwitch{

  class TopLevel extends Component {
    val sel = UInt(2 bits)
    val result = Bits(8 bits)

    result := 3
    switch(sel){
      is(0){
        result := 0
      }
//      is(1){
//        result := 1
//      }
//      is(2){
//        result := 2
//      }
      is(3){
//        result := 3
      }
      default{
        result := 1
      }
    }
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
    SpinalVerilog(new TopLevel())
  }
}

object PlayDevStackTrace{

  def main(args: Array[String]) {
    for(i <- 0 to 1000000) {
      val startAt = System.nanoTime()
      val x = List.fill(1000000)(new Throwable)
      val endAt = System.nanoTime()
      println((endAt - startAt) * 1e-6)
    }
  }
}


object PlayDevCombLoop{

  class TopLevel extends Component {
    val result,result2  = UInt(8 bits)

    def miaou = {
      val x, y, z  = UInt(8 bits)
      x := y
//      y := x
      result := x
      z := z
    }


    val xx, yy, zz  = UInt(8 bits)
    xx := yy
    yy := 0
    when(True) {
      yy := zz
    }
    zz := xx
    result2 := xx


    val xxx = UInt(8 bits)
    xxx := xxx
    miaou

  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
    SpinalVerilog(new TopLevel())
  }
}



object PlayDevCrossClock{

  class TopLevel extends Component {
    val clockA = in Bool
    val clockB = in Bool

    val areaA = new ClockingArea(ClockDomain(clockA)){
      val reg = Reg(Bool)
      reg := in(Bool)
      val wire = Bool()
      wire := reg
      val wire2 = Bool()
      wire2 := wire
    }

    val areaB = new ClockingArea(ClockDomain(clockB)){
      val reg = Reg(Bool)
      reg := areaA.wire2
      val output = out Bool()
      output := reg
    }
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
    SpinalVerilog(new TopLevel())
  }
}

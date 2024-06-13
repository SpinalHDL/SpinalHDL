package spinal.core

import spinal.lib._
import org.scalatest.funsuite.AnyFunSuite

case class ColorVec(channelWidth: Int) extends Interface {
  val width = addGeneric("WIDTH", channelWidth)// or addParameter
  val v0, v1, v2, v3 = Color(channelWidth)
  tieIFParameter(v0, "WIDTH", "WIDTH")
  tieIFParameter(v1, "WIDTH", "WIDTH")
  tieIFParameter(v2, "WIDTH", "WIDTH")
  tieIFParameter(v3, "WIDTH", "WIDTH")

  @modport
  def mst = out(v0, v1, v2, v3)

  @modport
  def slv = in(v0, v1, v2, v3)
}

case class Color(channelWidth: Int) extends Interface {
  val width = addGeneric("WIDTH", channelWidth)// or addParameter
  val r, g, b = UInt(channelWidth bits)
  tieGeneric(r, width)// or tieParameter
  tieGeneric(g, width)
  tieGeneric(b, width)

  @modport
  def mst = in(r, g, b)

  @modport
  def slv = out(r, g, b)
}

case class ColorHandShake(Width: Int) extends Interface with IMasterSlave {
  val w = addGeneric("W", Width, default = "8")
  val valid = Bool()
  val payload = ColorVec(Width)
  val ready = Bool()
  tieIFParameter(payload, "WIDTH", "W")

  override def asMaster = {
    out(valid, payload)
    in(ready)
  }

  @modport
  def mst = asMaster

  @modport
  def slv = asSlave
}

case class MyInt(width: Int) extends Interface with IMasterSlave {
  val wParam = addGeneric("WIDTH", width)
  val sParam = addParameter("SW", 8, default = "8")
  val a = Bits(width bits)
  //tieGeneric(a, wParam)
  tieParameter(a, wParam)
  val b = Bool()
  val c = SInt(8 bits)
  tieGeneric(c, sParam)

  override def asMaster = {
    out(a, b, c)
  }//mst
  @modport
  def mst = asMaster

  @modport
  def slv = asSlave

}

case class StreamIF[T <: Data](payloadType :  HardType[T]) extends Interface with IMasterSlave with DataCarrier[T] {
  val valid = Bool()
  val payload = payloadType()
  val ready = Bool()

  override def asMaster = mst
  @modport
  def mst = {
    out(valid, payload)
    in(ready)
  }

  @modport
  def slv = {
    in(valid, payload)
    out(ready)
  }

  def map[U <: Data](f: T => U): StreamIF[U] = {
    val new_payload = f(this.payload)
    val ht: HardType[U] = HardType(new_payload)
    val that: StreamIF[U] = StreamIF(ht)
    that.setName(s"${this.getName()}_map")
    that.valid := this.valid
    that.payload := new_payload
    this.ready := that.ready
    that
  }

  def <<(that: StreamIF[T]) = {
    this.payload := that.payload
    this.valid := that.valid
    that.ready := this.ready
  }

  def >>(that: StreamIF[T]) = {
    that << this
  }

  def fire = valid && ready
  def freeRun() = {
    ready := True
    this
  }

}

class TestInterface extends Component {
  val io_axi_s_valid = True
  val i3_io_axi_s = True
  val io = new Bundle {
    val x = slave(MyInt(8))
    val y = master(MyInt(9))
    val axi_s = slave(StreamIF(UInt(8 bits)))
    val axi_m = master(StreamIF(UInt(8 bits)))
    //val my_stream_s = slave(StreamIF(Vec(Bits(8 bits), 8)))
    //val my_stream_m = master(StreamIF(Vec(Bits(8 bits), 8)))
    val my_stream_s = slave (StreamIF(MyInt(8)))
    val my_stream_m = master(StreamIF(MyInt(8)))
    val t = out Bits(1 bits)
    val color = slave(ColorHandShake(8))
  }
  //io.color.notSVIFthisLevel()

  //io.color.ready := True

  io.my_stream_s.addGeneric("W", 8, "8")
  io.my_stream_s.tieIFParameter(io.my_stream_s.payload, "WIDTH", "W")

  //val io_axi_s = True

  val i1 = new TestInterfaceSub
  i1.io.i := io.x
  i1.io.ccc <> io.color
  io.t := B(i1.io.t)
  val i2 = new TestInterfaceSub
  i2.io.i.a := io.x.a
  i2.io.i.b := !io.x.b
  i2.io.i.c := io.x.c + 1
  i2.io.ccc.valid := io.color.valid
  i2.io.ccc.payload := io.color.payload
  val useless = Bool()
  useless := io.color.valid
  val i3 = new TestAxi
  val i4 = new TestAxi
  i3.io.axi_s << io.axi_s//.map(_ + 3)
  i4.io.axi_s << i3.io.axi_m
  io.axi_m << i4.io.axi_m.map(x => x + 1)

  io.my_stream_m << io.my_stream_s

  val r = Reg(MyInt(9))

  r.a := io.x.a.resized
  r.b := !io.x.b
  r.c := io.x.c + 2

  io.y := r

}

class TestInterfaceSub extends Component {
  val io = new Bundle {
    val i = in(MyInt(8))
    val ccc = slave(ColorHandShake(8))
    val t = out Bool()
  }

  io.ccc.ready := True
  val a = new ISub
  a.io.x := io.i
  val useless = Bool()
  useless := a.io.y && True
  io.t := a.io.z

}

class TestAxi extends Component {
  val io = new Bundle {
    val axi_s = slave(StreamIF(UInt(8 bits)))
    val axi_m = master(StreamIF(UInt(8 bits)))
  }

  io.axi_s >> io.axi_m
  //io.axi_s.where()

}

class ISub extends BlackBox {
  val io = new Bundle {
    val x = in(MyInt(8))
    val y = out Bool()
    val z = out Bool()
    val p = out Bool()
  }

  //io.y := io.x

}

class InterfaceTesterCocotbBoot extends AnyFunSuite {
  SpinalConfig().generateSystemVerilog(new TestInterface)
  SpinalConfig(svInterface = true).generateSystemVerilog((new TestInterface).setDefinitionName("TestInterfaceActive"))
  SpinalConfig().generateVerilog(new TestInterface)
}

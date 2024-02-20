package spinal.lib.io

import spinal.core._
import spinal.core.fiber.Engine
import spinal.lib.io._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.postfixOps

object InOutWrapper {
  def InferredDriver(i: Bool, o: Bool, io: Bool, we: Bool, name: String): Unit = {
    when(we) {
      io := i
    }
    o := io
  }
  def XilinxSeries7IOBuf(i: Bool, o: Bool, io: Bool, we: Bool, name: String): Unit = {
    import spinal.lib.blackbox.xilinx.s7.IOBUF
    val buffer = IOBUF()
    buffer.T := !we
    buffer.I := i
    o := buffer.O
    io := buffer.IO
    buffer.setName("IOBUF_" + name)
  }
  def LatticeIce40SB_IO(i: Bool, o: Bool, io: Bool, we: Bool, name: String): Unit = {
    import spinal.lib.blackbox.lattice.ice40.SB_IO
    val buffer = SB_IO("101001")
    buffer.OUTPUT_ENABLE := we
    buffer.D_OUT_0 := i
    o := buffer.D_IN_0
    io := buffer.PACKAGE_PIN
    buffer.setName("SB_IO_" + name)
  }

  def apply[T <: Component](c: T, makeDriver: (Bool, Bool, Bool, Bool, String) => Unit = InferredDriver): T = {
    Engine.get.onCompletion += (() => {
      val dataParents = mutable.LinkedHashMap[Data, Int]()
      @tailrec
      def add(that: Data): Unit = {
        if (that.parent != null) {
          dataParents(that.parent) = dataParents.getOrElseUpdate(that.parent, 0) + 1
          add(that.parent)
        }
      }

      def propagateTags(tristate: Bundle, pin: Data): Unit = {
        tristate.getTags().filter(_.ioTag).foreach(t => pin.addTag(t))
      }

      for (io <- c.getAllIo) {
        add(io)
      }

      def flattenedName(bundle: Data, signal: Data, marker: String) =
        bundle.getName() + signal
          .getName()
          .substring(bundle.getName().length + marker.length)

      def makeBuffers(width: Int, name: String) = {
        val we = Bits(width bit)
        val i = Bits(width bit)
        val o = Bits(width bit)
        val io = inout(Analog(Bits(width bit))).setWeakName(name)
        (0 until width) foreach { idx => makeDriver(i(idx), o(idx), io(idx), we(idx), name + "_" + idx) }
        (i, o, we, io)
      }

      c.rework {
        for ((dataParent, count) <- dataParents) {
          dataParent match {
            case bundle: TriState[_] if bundle.writeEnable.isOutput =>
              (bundle.write.flatten zip bundle.read.flatten).foreach {
                case (dw: Data, dr: Data) =>
                  val name = flattenedName(bundle, dw, "_write")
                  val (i, o, we, newIo) = makeBuffers(widthOf(dw), name)
                  we.setAllTo(bundle.writeEnable)
                  i := dw.asBits
                  dr.assignFromBits(o)
                  propagateTags(bundle, newIo)
              }
              bundle.setAsDirectionLess().unsetName().allowDirectionLessIo()
            case bundle: TriStateOutput[_] if bundle.isOutput =>
              bundle.write.flatten.foreach {
                dw: Data =>
                  val name = flattenedName(bundle, dw, "_write")
                  val (i, _, we, newIo) = makeBuffers(widthOf(dw), name)
                  we.setAllTo(bundle.writeEnable)
                  i := dw.asBits
                  propagateTags(bundle, newIo)
              }
              bundle.setAsDirectionLess().unsetName().allowDirectionLessIo()
            case bundle: ReadableOpenDrain[_] if bundle.isMasterInterface =>
              (bundle.write.flatten zip bundle.read.flatten).foreach {
                case (dw: Data, dr: Data) =>
                  val name = flattenedName(bundle, dw, "_write")
                  val (i, o, we, newIo) = makeBuffers(widthOf(dw), name)
                  we := ~dw.asBits
                  i.clearAll()
                  dr.assignFromBits(o)
                  propagateTags(bundle, newIo)
              }
              bundle.setAsDirectionLess().unsetName().allowDirectionLessIo()
            case bundle: TriStateArray if bundle.writeEnable.isOutput =>
              val name = bundle.getName()
              val (i, o, we, newIo) = makeBuffers(bundle.width, name)
              we := bundle.writeEnable
              i := bundle.write
              bundle.read := o
              bundle.setAsDirectionLess().unsetName().allowDirectionLessIo()
              propagateTags(bundle, newIo)
            case _ =>
          }
        }
      }
    })
    c
  }
}

object InOutWrapperPlayground extends App {
  import spinal.lib._

  case class MyTriStateTag() extends SpinalTag {
    override def ioTag = true
  }

  case class D() extends Bundle{
    val x = UInt(2 bits)
    val y = Bool()
  }

  val report = SpinalVhdl(InOutWrapper(new Component{
    def t = D()
    val driver = in(t)
    val sink = out(t)
    val openDrain = master(ReadableOpenDrain(t))
    openDrain.addTag(MyTriStateTag())
    openDrain.write := driver
    sink := openDrain.read
  }))
  report.toplevel.getAllIo.foreach(io => println(s"${io.getName()} => ${io.getTags()}"))
}




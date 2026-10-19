package spinal.core

import spinal.tester.SpinalAnyFunSuite

// Checks how the emitter binds sub-component inputs (ComponentEmitter.isSubComponentInputBound):
// an input driven by a parent signal, or by a literal nobody else reads, goes straight into
// the port map. A literal-driven input that the parent also reads gets its own wire.
object SubComponentInputBindingTester {
  class Child extends Component {
    val io = new Bundle {
      val a, b, c, d = in Bits(8 bits)
      val o = out Bits(8 bits)
    }
    io.o := io.a ^ io.b ^ io.c ^ io.d
  }

  class Parent extends Component {
    val io = new Bundle {
      val x = in Bits(8 bits)
      val o, readC, readD = out Bits(8 bits)
    }
    val first = new Child
    first.io.a := io.x       // parent signal
    first.io.b := B"x5A"     // literal, not read by the parent
    first.io.c := B"x3C"     // literal, read by the parent as a whole source
    first.io.d := B"x0F"     // literal, read by the parent inside an expression
    io.readC := first.io.c
    io.readD := first.io.d & io.x

    // Same parent, same port names, but nothing reads these literals.
    val second = new Child
    second.io.a := io.x
    second.io.b := B"x11"
    second.io.c := B"x22"
    second.io.d := B"x33"

    io.o := first.io.o | second.io.o
  }
}

class SubComponentInputBindingTester extends SpinalAnyFunSuite {
  import SubComponentInputBindingTester._

  def portMap(rtl: String, inst: String): String = {
    val start = rtl.indexOf(inst)
    assert(start >= 0, s"instance $inst not found")
    rtl.substring(start, rtl.indexOf(");", start))
  }

  test("verilog") {
    val rtl = SpinalVerilog(new Parent).getRtlString
    val first = portMap(rtl, "Child first (")
    val second = portMap(rtl, "Child second (")

    assert(first.matches("(?s).*\\.io_a\\s*\\(io_x[\\[\\]0-9:]*\\s*\\).*"), first)
    assert(first.matches("(?s).*\\.io_b\\s*\\(8'h5a\\s*\\).*"), first)
    assert(first.matches("(?s).*\\.io_c\\s*\\(first_io_c[\\[\\]0-9:]*\\s*\\).*"), first)
    assert(first.matches("(?s).*\\.io_d\\s*\\(first_io_d[\\[\\]0-9:]*\\s*\\).*"), first)
    assert(rtl.contains("assign first_io_c = 8'h3c;"))
    assert(rtl.contains("assign first_io_d = 8'h0f;"))

    assert(second.matches("(?s).*\\.io_b\\s*\\(8'h11\\s*\\).*"), second)
    assert(second.matches("(?s).*\\.io_c\\s*\\(8'h22\\s*\\).*"), second)
    assert(second.matches("(?s).*\\.io_d\\s*\\(8'h33\\s*\\).*"), second)
    assert(!rtl.contains("second_io_c") && !rtl.contains("second_io_d"))
  }

  test("vhdl") {
    val rtl = SpinalVhdl(new Parent).getRtlString
    val first = portMap(rtl, "first : entity")
    val second = portMap(rtl, "second : entity")

    assert(first.matches("(?s).*io_a\\s*=>\\s*io_x\\b.*"), first)
    assert(first.matches("(?s).*io_b\\s*=>\\s*pkg_stdLogicVector\\(\"01011010\"\\).*"), first)
    assert(first.matches("(?s).*io_c\\s*=>\\s*first_io_c\\b.*"), first)
    assert(first.matches("(?s).*io_d\\s*=>\\s*first_io_d\\b.*"), first)

    assert(second.matches("(?s).*io_c\\s*=>\\s*pkg_stdLogicVector\\(\"00100010\"\\).*"), second)
    assert(second.matches("(?s).*io_d\\s*=>\\s*pkg_stdLogicVector\\(\"00110011\"\\).*"), second)
    assert(!rtl.contains("second_io_c") && !rtl.contains("second_io_d"))
  }
}

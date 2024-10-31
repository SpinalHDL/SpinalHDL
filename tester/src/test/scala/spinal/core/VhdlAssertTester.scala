package spinal.core

import spinal.core.sim._
import spinal.tester.scalatest.SpinalAnyFunSuite
import java.io.OutputStream

class VhdlAssertTester extends SpinalAnyFunSuite {
  test("VHDL assert message formatting") {
    var references: Seq[String] = null
    val simOutput = new StringBuilder()
    val realOut = Console.out

    Console.withOut(new OutputStream {
      override def write(i: Int) = {
        realOut.write(i)
        simOutput.+=(i.toChar)
      }
    }) {
      SimConfig
        .withGhdl()
        .compile(new Component {
          setDefinitionName("VhdlAssertTester")
          object TestEnum extends SpinalEnum {
            val First, Second, Third = newElement()
          }
          case class TestBundle() extends Bundle {}
          val testValue = TestBundle()
          val messages = Seq[(String, Seq[Any], String)](
            ("REPORT_TIME", L"""$REPORT_TIME""", "210 fs"),
            ("QUOTATION MARK", L""""""", """""""),
            ("NEWLINE", Seq("First Line\nSecond Line\nThird Line"), "First Line\nSecond Line\nThird Line"),
            ("BOOL", L"$True $False", "'1' '0'"),
            ("BITS", L"${B(0x123, 14 bits)}", """x"0123""""),
            ("UINT", L"${U(123, 14 bits)}", "123"),
            ("SINT", L"${S(-123, 14 bits)}", "-123"),
            ("SINT", L"${S(-128, 8 bits)}", "-128"),
            ("ENUM", L"${TestEnum.First()} ${TestEnum.Third()}", "first third"),
            ("EXPRESSION", L"$testValue", "<Unknown Datatype `toplevel/testValue : TestBundle`>")
          )
          references = for ((name, _, ref) <- messages) yield f"$name: $ref"

          val counter = RegInit(U(0, 32 bits))
          counter := counter + 1
          for (((name, msg, _), i) <- messages.zipWithIndex) spinal.core.assert(counter =/= i, f"$name: " +: msg, NOTE)
        })
        .doSim { dut =>
          val clk = dut.clockDomain
          clk.forkStimulus(10, 0, 20)
          clk.waitSampling(10)
        }
    }

    val outputLines = simOutput.mkString.linesIterator
    for {
      message <- references
      line <- message.linesIterator
    }
      while (!outputLines.next().endsWith(line))
        assert(outputLines.hasNext, f"Message not found in simulation output:\n $message")
  }
}

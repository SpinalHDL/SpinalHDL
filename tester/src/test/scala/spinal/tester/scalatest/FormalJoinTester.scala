package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

class StreamJoin[T1 <: Data, T2 <: Data](dataType_0: T1, dataType_1: T2) extends Component {
  val io = new Bundle {
    val inputs_0 = slave(Stream (dataType_0))
    val inputs_1 = slave(Stream (dataType_1))
    val output = master(Stream(TupleBundle2(
        dataType_0,
        dataType_1
    )))
  }
  val logic = StreamJoin(io.inputs_0,io.inputs_1)
  io.output << logic
}

class FormalJoinTester extends SpinalFormalFunSuite {
  test("StreamJoinTester-verify") {
    FormalConfig
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType_0 = Bits(8 bits)
        val dataType_1 = Bits(16 bits)
        val dut = FormalDut(new StreamJoin(dataType_0, dataType_1))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val inputs_0 = slave Stream (dataType_0)
        val inputs_1 = slave Stream (dataType_1)
        val output = master(
          Stream(
            TupleBundle2(
              dataType_0,
              dataType_1
            )
          )
        )

        inputs_0 >> dut.io.inputs_0
        inputs_1 >> dut.io.inputs_1
        output << dut.io.output

        when(reset || past(reset)) {
          assume(inputs_0.valid === False)
          assume(inputs_1.valid === False)
        }

        inputs_0.formalAssumesSlave()
        inputs_1.formalAssumesSlave()
        output.formalAssertsMaster()
        output.formalCovers(3)

        assert(inputs_0.fire === inputs_1.fire)
        assert(output.fire === inputs_0.fire)
        assert(output.payload._1 === inputs_0.payload)
        assert(output.payload._2 === inputs_1.payload)
      })
  }
}

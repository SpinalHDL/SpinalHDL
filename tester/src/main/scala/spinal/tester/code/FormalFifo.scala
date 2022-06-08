package spinal.tester.code

import spinal.core.formal._
import spinal.core._
import spinal.lib.{Stream, StreamFifo, Timeout, master, slave, CountOne}

object FormalFifo extends App {
  val Error = new AreaRoot {
    val DROP_PUSH = new Nameable {}
    val DROP_POP = new Nameable {}
    val DUPLICATE = new Nameable {}
    val CORRUPT = new Nameable {}
    val LOCK_PUSH = new Nameable {}
    val LOCK_POP = new Nameable {}
    def all = List(DROP_PUSH, DROP_POP, DUPLICATE, CORRUPT, LOCK_PUSH, LOCK_POP)
  }
  val Trigger = new AreaRoot {
    val D1 = new Nameable {}
    val D2 = new Nameable {}
    val ANY = new Nameable {}
    def all = List(D1, D2, ANY)
  }

  class StreamFifoWrapper(error: Any, trigger: Any) extends Component {
    val io = new Bundle {
      val push = slave Stream (UInt(7 bits))
      val pop = master Stream (UInt(7 bits))
      val d1, d2 = in UInt (7 bits)
    }

    val dut = StreamFifo(UInt(7 bits), 4)

    val condPush = trigger match {
      case Trigger.D1  => io.push.payload === io.d1
      case Trigger.D2  => io.push.payload === io.d2
      case Trigger.ANY => allseq(Bool())
    }
    val condPop = trigger match {
      case Trigger.D1  => io.pop.payload === io.d1
      case Trigger.D2  => io.pop.payload === io.d2
      case Trigger.ANY => allseq(Bool())
    }
    error match {
      case Error.DROP_PUSH => {
        dut.io.push << io.push.throwWhen(condPush)
        dut.io.pop >> io.pop
      }
      case Error.DROP_POP => {
        dut.io.push << io.push
        dut.io.pop.throwWhen(condPop) >> io.pop
      }
      case Error.DUPLICATE => {
        dut.io.push << io.push.s2mPipe().forkSerial(condPush)
        dut.io.pop >> io.pop
      }
      case Error.CORRUPT => {
        dut.io.push << io.push.translateWith(io.push.payload ^ condPush.asUInt(7 bits))
        assume(io.d1 =/= (io.d2 ^ 1))
        dut.io.pop >> io.pop
      }
      case Error.LOCK_PUSH => {
        dut.io.push << io.push.haltWhen(condPush)
        dut.io.pop >> io.pop
      }
      case Error.LOCK_POP => {
        dut.io.push << io.push
        dut.io.pop.haltWhen(condPop) >> io.pop
      }
      case null => {
        dut.io.push << io.push
        dut.io.pop >> io.pop
      }
    }
  }

  def fifoBmcTest(error: Any, trigger: Any, depth: Int = 12) = {
    FormalConfig
      .withBMC(depth)
      .doVerify(new Component {
        val dut = new StreamFifoWrapper(error, trigger)
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        anyseq(dut.io.push.payload)
        anyseq(dut.io.push.valid)
        anyseq(dut.io.pop.ready)

        when(past(dut.io.push.isStall) init (False)) {
          assume(dut.io.push.valid)
          assume(stable(dut.io.push.payload))
        }

        def hit(stream: Stream[UInt], ref: UInt) = RegInit(False) setWhen (stream.fire && ref === stream.payload)

        val d1, d2 = anyconst(UInt(7 bits))
        assume(d1 =/= d2)
        dut.io.d1 := d1
        dut.io.d2 := d2

        val d1_in = hit(dut.io.push, d1)
        val d2_in = hit(dut.io.push, d2)
        when(dut.io.push.valid) {
          assume(!(dut.io.push.payload === d1 && d1_in)) // Enforce only one d1
          assume(!(dut.io.push.payload === d2 && (!d1_in || d2_in))) // Enforce only one d2, and after d1
        }

        val d1_out = hit(dut.io.pop, d1)
        val d2_out = hit(dut.io.pop, d2)
        assert(!(dut.io.pop.valid && dut.io.pop.payload === d1 && d1_out)) // Check no duplication
        assert(!(d2_out && !d1_out)) // Check ordering

        // Check looks
        val timeout = Timeout(3)
        when(!dut.io.push.isStall || dut.io.pop.valid) {
          timeout.clear()
        }
        assert(!timeout.state)
      })
  }

  def fifoTest(error: Any, trigger: Any) = {
    FormalConfig
      .withBMC(10)
      .withProve(10)
      .withCover(10)
      .addEngin(SmtBmc(stbv = true, solver=SmtBmcSolver.Z3))
      .withDebug
      .doVerify(new Component {
        val dut = FormalDut(new StreamFifoWrapper(error, trigger))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        anyseq(dut.io.push.payload)
        anyseq(dut.io.pop.ready)

        val inValid = anyseq(dut.io.push.valid)
        // assume no valid while reset and one clock later.
        when(reset || past(reset)) {
          assume(inValid === False)
        }

        dut.io.push.withAssumes()
        dut.io.pop.withAsserts()
        dut.dut.withAssumes()

        val d1 = anyconst(UInt(7 bits))
        val d2 = anyconst(UInt(7 bits))
        dut.io.d1 := d1
        dut.io.d2 := d2

        val (d1_in, d2_in) = dut.io.push.withOrderAssumes(d1, d2)
        val (d1_out, d2_out) = dut.io.pop.withOrderAsserts(d1, d2)

        when(!d1_in) { assume(!dut.dut.formalContains(d1)) }
        when(d1_in && !d1_out) { assert(dut.dut.formalCount(d1) === 1) }

        when(!d2_in) { assume(!dut.dut.formalContains(d2)) }
        when(d2_in && !d2_out) { assert(dut.dut.formalCount(d2) === 1) }

        when(d1_in && d2_in && !d1_out) { assert(!d2_out) }
      })
  }

  def shouldFail(body: => Unit) = assert(try {
    body
    false
  } catch {
    case e : Throwable => println(e); true
  })

  for (
    error <- Error.all;
    trigger <- Trigger.all
  ) {
    println("Processing " +s"fifo-$error-$trigger "+"verification.")
    shouldFail(fifoTest(error, trigger))
  }
  println("Processing right FIFO verification.")
  fifoTest(null, Trigger.ANY)

}

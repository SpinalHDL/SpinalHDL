package spinal.core

import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import spinal.tester.{SpinalAnyFunSuite, SpinalSimTester}

import scala.util.Random

// Definition of the types used in the tagged union
case class TypeA() extends Bundle {
  val x, y = UInt(8 bits)
}

case class TypeB() extends Bundle {
  val l = UInt(10 bits)
  val v = Bool()
}

case class TypeC() extends Bundle {
  val m = UInt(16 bits)
}

case class InUnion() extends TaggedUnion {
  val a1 = TypeA()
  val a2 = TypeA()
  val b = TypeB()
}

case class OutUnion() extends TaggedUnion {
  val b = TypeB()
  val c = TypeC()
}

// The TaggedUnionTester component
class TaggedUnionTester() extends Component {
  val io = new Bundle {
    val i = in(InUnion())
    val o = out(OutUnion())
  }

  io.o.assignDontCare()

  io.i {
    case b: TypeB => { // input is variant b
      io.o.update {
        bOut: TypeB => {
          bOut := b
        }
      }
    }
    case (io.i.a1, a: TypeA) => {
      io.o.update {
        cOut: TypeC => {
          cOut.m := a.x.resized
        }
      }
    }
    case (io.i.a2, a: TypeA) => {
      io.o.update(io.o.c) { // explicit c variant chosen
        cOut: TypeC => {
          cOut.m := a.y.resized
        }
      }
    }
  }
}

class TaggedUnionTesterSim extends SpinalAnyFunSuite {
  SpinalSimTester { env =>
    import env._

    var compiled: SimCompiled[TaggedUnionTester] = null

    test(prefix + "compile") {
      compiled = SimConfig
        .allOptimisation
        .compile(new TaggedUnionTester())
    }

    test(prefix + "TaggedUnionTest") {
      compiled.doSim { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        for (_ <- 0 until 1000) {
          // Randomly select a type
          val selectedType = Random.nextInt(3)
          dut.io.i.assignDontCare()

          selectedType match {
            case 0 => // TypeA with a1 variant
              val x = Random.nextInt(256)
              val y = Random.nextInt(256)
              dut.io.i.update(dut.io.i.a1) {
                a: TypeA => {
                    a.x #= x
                    a.y #= y
                }
              }

            case 1 => // TypeA with a2 variant
              val x = Random.nextInt(256)
              val y = Random.nextInt(256)
              dut.io.i.update(dut.io.i.a2) {
                a: TypeA => {
                    a.x #= x
                    a.y #= y
                }
              }

            case 2 => // TypeB
              val l = Random.nextInt(1024)
              val v = Random.nextBoolean()

              dut.io.i.update {
                b: TypeB => {
                    b.l #= l
                    b.v #= v
                }
              }
          }

          dut.clockDomain.waitSampling()


          // Validate the outputs based on the input type
          selectedType match {
            case 0 => // in TypeA with a1 variant => out TypeC
                // assert(otagAsUInt.toInt == 1)
                assert(dut.io.o.tag.asBits === B"1")

            case 1 => // in TypeA with a2 variant => out TypeC
                assert(dut.io.o.tag.asBits === B"1")

            case 2 => // in/out TypeB
                assert(dut.io.o.tag.asBits === B"0")
          }

            
        }

        println("Simulation done")
      } 
    }
  }
}

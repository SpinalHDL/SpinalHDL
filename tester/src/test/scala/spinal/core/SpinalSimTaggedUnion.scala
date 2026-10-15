package tester

import spinal.lib._
import spinal.sim._
import spinal.core._
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
        println("Starting TaggedUnion simulation")
        dut.clockDomain.forkStimulus(period = 10)

        for (_ <- 0 until 1000) {
          // Randomly select a type
          val selectedType = Random.nextInt(3)

          dut.io.i.tag #= dut.io.i.tagEnum.elements(selectedType)
          val x = Random.nextInt(256)
          val y = Random.nextInt(256)
          val l = Random.nextInt(1024)
          // val v = Random.nextBoolean()
          val v = Random.nextInt(2)

          selectedType match {
            case 0 => // TypeA with a1 variant
              dut.io.i.unionPayload #= (y << 8) | x

            case 1 => // TypeA with a2 variant
              dut.io.i.unionPayload #= (y << 8) | x

            case 2 => // TypeB
              dut.io.i.unionPayload #= (v << 10) | l
          }

          dut.clockDomain.waitSampling()

          // println(s"x=$x, y=$y, l=$l, v=$v")
          // println(s"In ${dut.io.i.tag.toEnum} = ${dut.io.i.unionPayload.toInt}")
          // println(s"Out ${dut.io.o.tag.toEnum} = ${dut.io.o.unionPayload.toInt}")
          
      

          // Validate the outputs depending on the tag
          selectedType match {
            case 0 => // in TypeA with a1 variant => out TypeC
                assert(dut.io.o.tag.toEnum == dut.io.o.tagEnum.elements(1))
                assert((dut.io.o.unionPayload.toInt & 0xFF) == x)
                

            case 1 => // in TypeA with a2 variant => out TypeC
                 assert(dut.io.o.tag.toEnum == dut.io.o.tagEnum.elements(1))
                 assert((dut.io.o.unionPayload.toInt & 0xFF) == y)

            case 2 => // in/out TypeB
                 assert(dut.io.o.tag.toEnum == dut.io.o.tagEnum.elements(0))
                //  Most significant bits are "dont care".
                 assert((dut.io.o.unionPayload.toInt & 0x7FF) == ((v << 10) | l))
          }
        }

        println("Simulation TaggedUnion done")
      } 
    }
  }
}


object TaggedUnionIndependantTester {
  SimConfig.withWave.compile(new TaggedUnionTester()).doSim{ dut =>
      println("Starting TaggedUnion simulation")
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
                a1: TypeA => {
                    a1.x #= x
                    a1.y #= y
                }
              }

            case 1 => // TypeA with a2 variant
              val x = Random.nextInt(256)
              val y = Random.nextInt(256)
              dut.io.i.update(dut.io.i.a2) {
                a2: TypeA => {
                    a2.x #= x
                    a2.y #= y
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

          println("Enum signature")
          println(dut.io.o.tagEnum)
          // dut.clockDomain.waitSampling()
          sleep(1)
      

          // Validate the outputs based on the input type
          // selectedType match {
            // case 0 => // in TypeA with a1 variant => out TypeC
                // assert(dut.io.o.tag.toEnum == dut.io.o.tagEnum.a1)
                // println("Enum signature")
                // println(dut.io.o.tagEnum)
                // assert(dut.io.o.tag.toInt == 0)

            // case 1 => // in TypeA with a2 variant => out TypeC
            //     assert(dut.io.o.tag.asBits == B"1".asBits)

            // case 2 => // in/out TypeB
            //     assert(dut.io.o.tag.asBits == B"0".asBits)
          // }

            
        }

        println("Simulation TaggedUnion done")
    }
}
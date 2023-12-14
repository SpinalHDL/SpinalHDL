package spinal.core

import spinal.lib._
import spinal.tester.SpinalTesterCocotbBase


case class TypeA() extends Bundle {
    val x,y = UInt(8 bits)
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

class TaggedUnionTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "TaggedUnionTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/TaggedUnionTester"
  override def createToplevel: Component = new TaggedUnionTester
}
package spinal.core

import spinal.lib._
import spinal.tester.SpinalTesterCocotbBase

case class ReadRequest() extends Bundle {
    val address = UInt(32 bits)
}

case class WriteRequest() extends Bundle {
    val address = UInt(32 bits)
    val value = Bits(32 bits)
}

case class ReadWriteRequest() extends TaggedUnion {
    val read = ReadRequest()
    val write = WriteRequest()
}

class TaggedUnionTester() extends Component {
    val io = new Bundle {
        val rw = in Bool()
        val doReq = in Bool()

        val req = master(Stream(ReadWriteRequest()))

        val ans = slave(Flow(ReadWriteRequest()))
        val ansAdd = out(UInt(32 bits))
    }

    io.req.valid := False
    io.req.payload.assignDontCare()

    when(io.doReq) {
        io.req.valid := True

        when(io.rw) { // write
            io.req.payload.choose {
                case w: WriteRequest => {
                    w.address := 1
                    w.value := 0
                }
            }
        }
        .otherwise {
            io.req.payload.chooseVariant(io.req.payload.read) {
                r: ReadRequest => {
                    r.address := 2
                }
            }
        }
    }

    io.ansAdd.assignDontCare()
    when(io.ans.valid) {
        io.ans.payload.among {
            case r: ReadRequest => {
                io.ansAdd := r.address
            }
            case w: WriteRequest => {
                io.ansAdd := w.address
            }
        }
    }
}

class TaggedUnionTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "TaggedUnionTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/TaggedUnionTester"
  override def createToplevel: Component = new TaggedUnionTester
}
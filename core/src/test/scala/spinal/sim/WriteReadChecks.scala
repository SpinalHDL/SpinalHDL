
package spinal.sim


//import spinal.sim._
import scala.util.continuations.suspendable


object ReadWriteChecks {
  object Rtl {
    import spinal.core._

    class Dut extends Component {
      val io = new Bundle {
        val bool = in Bool
        val u1  = in UInt (1 bits)
        val u8  = in UInt (8 bits)
        val u16 = in UInt (16 bits)
        val u32 = in UInt (32 bits)
        val u63 = in UInt (63 bits)
        val s1  = in SInt (1 bits)
        val s8  = in SInt (8 bits)
        val s16 = in SInt (16 bits)
        val s32 = in SInt (32 bits)
        val s64 = in SInt (64 bits)
//        val u128 = in UInt (128 bits)
      }
    }

  }

  import SpinalSimManagedApi._
  import spinal.sim._
  def main(args: Array[String]): Unit = {
//    val (sim, dut) = SpinalSimVerilator(new Dut)
//    val manager = new SimManager(sim)
//    manager.run{
//      sleep(10)
//      sleep(10)
//      println(peak(dut.io.u8_o))
//    }

    SpinalSimManagedVerilator(new Rtl.Dut) { dut =>
      def checkB(value : Boolean, that : Bool): Unit@suspendable ={
        that := value
        sleep(1)
        assert(that.toBoolean == value, that.getName() + " " + value)
      }

      def check(value : Long, that : BitVector): Unit@suspendable ={
        that := value
        sleep(1)
        assert(that.toLong == value, that.getName() + " " + value)
      }

      List(false, true).foreachSim(value => checkB(value, dut.io.bool))
      
      List(0,1).foreachSim(value => check(value, dut.io.u1 ))
      List(0,1,127, 255).foreachSim(value => check(value, dut.io.u8 ))
      List(0,1,0xFFFF).foreachSim(value => check(value, dut.io.u16 ))
      List(0,1,0x7FFFFFFF).foreachSim(value => check(value, dut.io.u32 ))
      List(0l,1l,0x7FFFFFFFFFFFFFFFl).foreachSim(value => check(value, dut.io.u63 ))
      
      List(0,-1).foreachSim(value => check(value, dut.io.s1 ))
      List(0,1,-1,127, -128).foreachSim(value => check(value, dut.io.s8 ))
      List(0,1,-1, Short.MaxValue, Short.MinValue).foreachSim(value => check(value, dut.io.s16 ))
      List(0,1,-1,0xFFFFFFFF,-1, Int.MaxValue, Int.MinValue).foreachSim(value => check(value, dut.io.s32 ))
      List(0l,1l,0xFFFFFFFFFFFFFFFFl, -1l, Long.MaxValue, Long.MinValue).foreachSim(value => check(value, dut.io.s64 ))

//      List(0l,1l,0x7FFFFFFFFFFFFFFFl).foreachSim(value => check(value, dut.io.s128 ))
    }
  }
}

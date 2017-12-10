
package spinal.sim



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
        val u31 = in UInt (31 bits)
        val u32 = in UInt (32 bits)
        val u63 = in UInt (63 bits)
        val u64 = in UInt (64 bits)
        val u65 = in UInt (65 bits)
        val u127 = in UInt (127 bits)
        val u128 = in UInt (128 bits)
        val s1  = in SInt (1 bits)
        val s8  = in SInt (8 bits)
        val s16 = in SInt (16 bits)
        val s32 = in SInt (32 bits)
        val s63 = in SInt (63 bits)
        val s64 = in SInt (64 bits)
        val s65 = in SInt (65 bits)
        val s127 = in SInt (127 bits)
        val s128 = in SInt (128 bits)
      }
    }
  }


  def main(args: Array[String]): Unit = {
    import SpinalSimManagedApi._
    SpinalSimManagedVerilator(new Rtl.Dut) { dut =>
      def checkBoolean(value : Boolean, that : Bool): Unit@suspendable ={
        that := value
        sleep(1)
        assert(that.toBoolean == value, that.getName() + " " + value)
      }

      def checkInt(value : Int, that : BitVector): Unit@suspendable ={
        that \= value
        sleep(1)
        assert(that.toInt == value, that.getName() + " " + value)
      }

      def checkLong(value : Long, that : BitVector): Unit@suspendable ={
        that \= value
        sleep(1)
        assert(that.toLong == value, that.getName() + " " + value)
      }

      def checkBigInt(value : BigInt, that : BitVector): Unit@suspendable ={
        that \= value
        sleep(1)
        assert(that.toBigInt == value, that.getName() + " " + value)
      }




      (0 to 3).foreachSim {e =>
        List(false, true).foreachSim(value => checkBoolean(value, dut.io.bool))

        //checkInt
        List(0, 1).foreachSim(value => checkInt(value, dut.io.u1))
        List(0, 1, 127, 255).foreachSim(value => checkInt(value, dut.io.u8))
        List(0, 1, 0xFFFF).foreachSim(value => checkInt(value, dut.io.u16))
        List(0, 1, 0x7FFFFFFF).foreachSim(value => checkInt(value, dut.io.u31))

        List(0, -1).foreachSim(value => checkInt(value, dut.io.s1))
        List(0, 1, -1, 127, -128).foreachSim(value => checkInt(value, dut.io.s8))
        List(0, 1, -1, Short.MaxValue, Short.MinValue).foreachSim(value => checkInt(value, dut.io.s16))
        List(0, 1, -1, 0xFFFFFFFF, -1, Int.MaxValue, Int.MinValue).foreachSim(value => checkInt(value, dut.io.s32))

        //checkLong
        List(0, 1).foreachSim(value => checkLong(value, dut.io.u1))
        List(0, 1, 127, 255).foreachSim(value => checkLong(value, dut.io.u8))
        List(0, 1, 0xFFFF).foreachSim(value => checkLong(value, dut.io.u16))
        List(0, 1, 0x7FFFFFFF).foreachSim(value => checkLong(value, dut.io.u32))
        List(0l, 1l, 0x7FFFFFFFFFFFFFFFl).foreachSim(value => checkLong(value, dut.io.u63))

        List(0, -1).foreachSim(value => checkLong(value, dut.io.s1))
        List(0, 1, -1, 127, -128).foreachSim(value => checkLong(value, dut.io.s8))
        List(0, 1, -1, Short.MaxValue, Short.MinValue).foreachSim(value => checkLong(value, dut.io.s16))
        List(0, 1, -1, 0xFFFFFFFF, -1, Int.MaxValue, Int.MinValue).foreachSim(value => checkLong(value, dut.io.s32))
        List(0l, 1l, 0xFFFFFFFFFFFFFFFFl, -1l, Long.MaxValue, Long.MinValue).foreachSim(value => checkLong(value, dut.io.s64))

        //checkBigInt
        List(0, 1).foreachSim(value => checkBigInt(value, dut.io.u1))
        List(0, 1, 127, 255).foreachSim(value => checkBigInt(value, dut.io.u8))
        List(0, 1, 0xFFFF).foreachSim(value => checkBigInt(value, dut.io.u16))
        List(0, 1, 0x7FFFFFFF).foreachSim(value => checkBigInt(value, dut.io.u32))
        List(0l, 1l, 0x7FFFFFFFFFFFFFFFl).foreachSim(value => checkBigInt(value, dut.io.u63))

        List(0, -1).foreachSim(value => checkBigInt(value, dut.io.s1))
        List(0, 1, -1, 127, -128).foreachSim(value => checkBigInt(value, dut.io.s8))
        List(0, 1, -1, Short.MaxValue, Short.MinValue).foreachSim(value => checkBigInt(value, dut.io.s16))
        List(0, 1, -1, 0xFFFFFFFF, -1, Int.MaxValue, Int.MinValue).foreachSim(value => checkBigInt(value, dut.io.s32))
        List(0l, 1l, 0xFFFFFFFFFFFFFFFFl, -1l, Long.MaxValue, Long.MinValue).foreachSim(value => checkBigInt(value, dut.io.s64))


        (0 to 63).map(n => BigInt("0" + "1" * n, 2)).foreachSim(value => checkBigInt(value, dut.io.u63))
        (0 to 64).map(n => BigInt("0" + "1" * n, 2)).foreachSim(value => checkBigInt(value, dut.io.u64))
        (0 to 65).map(n => BigInt("0" + "1" * n, 2)).foreachSim(value => checkBigInt(value, dut.io.u65))
        (0 to 127).map(n => BigInt("0" + "1" * n, 2)).foreachSim(value => checkBigInt(value, dut.io.u127))
        (0 to 128).map(n => BigInt("0" + "1" * n, 2)).foreachSim(value => checkBigInt(value, dut.io.u128))

        (0 to 62).map(n => BigInt("0" + "1" * n, 2)).foreachSim(value => checkBigInt(value, dut.io.s63))
        (0 to 63).map(n => BigInt("0" + "1" * n, 2)).foreachSim(value => checkBigInt(value, dut.io.s64))
        (0 to 64).map(n => BigInt("0" + "1" * n, 2)).foreachSim(value => checkBigInt(value, dut.io.s65))
        (0 to 126).map(n => BigInt("0" + "1" * n, 2)).foreachSim(value => checkBigInt(value, dut.io.s127))
        (0 to 127).map(n => BigInt("0" + "1" * n, 2)).foreachSim(value => checkBigInt(value, dut.io.s128))

        (0 to 62).map(n => -BigInt("0" + "1" * n, 2) -1).foreachSim(value => checkBigInt(value, dut.io.s63))
        (0 to 63).map(n => -BigInt("0" + "1" * n, 2) -1).foreachSim(value => checkBigInt(value, dut.io.s64))
        (0 to 64).map(n => -BigInt("0" + "1" * n, 2) -1).foreachSim(value => checkBigInt(value, dut.io.s65))
        (0 to 126).map(n => -BigInt("0" + "1" * n, 2) -1).foreachSim(value => checkBigInt(value, dut.io.s127))
        (0 to 127).map(n => -BigInt("0" + "1" * n, 2) -1).foreachSim(value => checkBigInt(value, dut.io.s128))
      }
    }
  }
}

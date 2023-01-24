package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.bus.amba4.axi.sim.SparseMemory

class Axi4SparseMemoryTester extends AnyFunSuite{
  val groundTrue: Array[BigInt] = Array(
    BigInt("36014621", 16), BigInt("01472101", 16), BigInt("fe7e0036", 16), BigInt("0119d209", 16),
    BigInt("7e014621", 16), BigInt("0100c217", 16), BigInt("00165fff", 16), BigInt("19014821", 16),
    BigInt("23794e19", 16), BigInt("57962346", 16), BigInt("da9e2378", 16), BigInt("cab2013f", 16),
    BigInt("7056013f", 16), BigInt("2b715e2b", 16), BigInt("21732b72", 16), BigInt("21340146", 16)
  )

  test("SparseMemory loadHex with 0 offset") {
    val mem = SparseMemory()
    mem.loadHex("tester/src/test/resources/example.hex")
    for(i <- 0 until groundTrue.length){
      assert(mem.readBigInt(0x80000100l + i *4, 4) == groundTrue(i))
    }
  }

  test("SparseMemory loadHex with 0x80000100l offset") {
    val mem = SparseMemory()
    mem.loadHex("tester/src/test/resources/example.hex", 0x80000100l)
    for (i <- 0 until groundTrue.length) {
      assert(mem.readBigInt(i * 4, 4) == groundTrue(i))
    }
  }
}

package spinal.core

import sim._

import spinal.tester.SpinalAnyFunSuite

class SimBigIntPimperTest extends SpinalAnyFunSuite {
  def toBytes(bi: BigInt, bits: Int = -1, endian: Endianness = LITTLE) =
    bi.toBytes(bits, endian).map(i => f"$i%02x").mkString(" ")
  test("positive") {
    // simple cases
    assert(toBytes(0xff) == "ff")
    assert(toBytes(0x80ff) == "ff 80")
    // check result is prepended if necessary
    assert(toBytes(0xff, 4*8) == "ff 00 00 00")
    // check with BIG endian
    assert(toBytes(0x80ff, endian=BIG) == "80 ff")
    assert(toBytes(0x80ff, 4*8, endian=BIG) == "00 00 80 ff")
    // check that bit length != n*8 works
    assert(toBytes(0x3ff, 10) == "ff 03")
    // also needs to work for BigInt bigger than Int
    assert(toBytes(0x1122334455667788L) == "88 77 66 55 44 33 22 11")
    // ... and longer than a Long
    assert(toBytes(BigInt("112233445566778899aabbccddeeff", 16)) == "ff ee dd cc bb aa 99 88 77 66 55 44 33 22 11")

    // negative numbers are sign extended to full bytes by default
    assert(toBytes(-1) == "ff")
    // result must be prepended correctly if expanded
    assert(toBytes(-1, 16) == "ff ff")
    // sign extend is only up to given number of bytes
    assert(toBytes(-1, 4) == "0f")
    // negative numbers also need to be extended to big bitwidths
    assert(toBytes(-1, 7*8) == "ff ff ff ff ff ff ff")
  }
  test("negative") {
    // must throw if too little space is given
    assertThrows[java.lang.AssertionError] { toBytes(0xffff, 8) }
    // length must be checked precisely
    assertThrows[java.lang.AssertionError] { toBytes(0x07, 2) }
    // space for sign bit must be available
    assertThrows[java.lang.AssertionError] { toBytes(-256, 8) }
  }
}

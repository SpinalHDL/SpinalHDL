import cocotb
from cocotb.triggers import Timer, RisingEdge

from cocotblib.misc import randSignal, assertEquals, truncUInt, ClockDomainAsyncReset


@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    #random.seed(0)

    dut.address <= 0
    yield Timer(10)
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address <= 1
    yield Timer(10)
    assertEquals(dut.data_bool,1,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address <= 2
    yield Timer(10)
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,511,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address <= 3
    yield Timer(10)
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,1023,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address <= 4
    yield Timer(10)
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,2047,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address <= 5
    yield Timer(10)
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,2,"1")


    dut.address <= 6
    yield Timer(10)
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,43,"1")
    assertEquals(dut.data_uint,74,"1")
    assertEquals(dut.data_sint,88,"1")
    assertEquals(dut.data_enumeration,1,"1")


    dut.log.info("Cocotb test done")

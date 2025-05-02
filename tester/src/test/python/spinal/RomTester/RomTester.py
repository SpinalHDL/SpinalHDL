import cocotb
from cocotb.triggers import Timer, RisingEdge
from cocotb.clock import Clock

from cocotblib.misc import randSignal, assertEquals, truncUInt, ClockDomainAsyncReset


@cocotb.test()
async def test1(dut):
    cocotb.start_soon(Clock(dut.clk, 10, units='ns').start())
    dut.log.info("Cocotb test boot")
    #random.seed(0)

    dut.address.value = 0
    await RisingEdge(dut.clk)
    await Timer(1, units='ns')
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address.value = 1
    await RisingEdge(dut.clk)
    await Timer(1, units='ns')
    assertEquals(dut.data_bool,1,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address.value = 2
    await RisingEdge(dut.clk)
    await Timer(1, units='ns')
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,511,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address.value = 3
    await RisingEdge(dut.clk)
    await Timer(1, units='ns')
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,1023,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address.value = 4
    await RisingEdge(dut.clk)
    await Timer(1, units='ns')
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,2047,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address.value = 5
    await RisingEdge(dut.clk)
    await Timer(1, units='ns')
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,2,"1")


    dut.address.value = 6
    await RisingEdge(dut.clk)
    await Timer(1, units='ns')
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,43,"1")
    assertEquals(dut.data_uint,74,"1")
    assertEquals(dut.data_sint,88,"1")
    assertEquals(dut.data_enumeration,1,"1")


    dut.log.info("Cocotb test done")

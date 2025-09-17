import cocotb
from cocotb.triggers import Timer
from cocotblib.misc import assertEquals


@cocotb.test()
async def test1(dut):
    dut.log.info("Cocotb test boot")
    #random.seed(0)

    dut.address.value = 0
    await Timer(1, units='ns')
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address.value = 1
    await Timer(1, units='ns')
    assertEquals(dut.data_bool,1,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address.value = 2
    await Timer(1, units='ns')
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,511,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address.value = 3
    await Timer(1, units='ns')
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,1023,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address.value = 4
    await Timer(1, units='ns')
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,2047,"1")
    assertEquals(dut.data_enumeration,0,"1")


    dut.address.value = 5
    await Timer(1, units='ns')
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,0,"1")
    assertEquals(dut.data_uint,0,"1")
    assertEquals(dut.data_sint,0,"1")
    assertEquals(dut.data_enumeration,2,"1")


    dut.address.value = 6
    await Timer(1, units='ns')
    assertEquals(dut.data_bool,0,"1")
    assertEquals(dut.data_bits,43,"1")
    assertEquals(dut.data_uint,74,"1")
    assertEquals(dut.data_sint,88,"1")
    assertEquals(dut.data_enumeration,1,"1")


    dut.log.info("Cocotb test done")

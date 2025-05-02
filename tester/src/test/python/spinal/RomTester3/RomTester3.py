import cocotb
from cocotb.triggers import Timer, RisingEdge

from cocotblib.misc import randBits, assertEquals

from cocotb.clock import Clock

@cocotb.test()
async def test1(dut):
    cocotb.start_soon(Clock(dut.clk, 10, units='ns').start())
    dut._log.info("Cocotb test boot")
    #random.seed(0)
    table = [0x01234567,0x12345670,0x10293857,0x0abcfe23,0x02938571,0xabcfe230,0x717833aa,0x17833aa6]

    for _ in range(1000):
        dut.address.value = randBits(3)
        await RisingEdge(dut.clk)
        await Timer(1, units='ns')
        data = int(dut.data.value)
        assertEquals(data, table[int(dut.address.value)], "expect dut data to match table")



    dut._log.info("Cocotb test done")

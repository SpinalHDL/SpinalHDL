import cocotb
from cocotb.triggers import Timer
from cocotblib.misc import randBits, assertEquals


@cocotb.test()
async def test1(dut):
    dut._log.info("Cocotb test boot")
    #random.seed(0)
    table = [0x01234567,0x12345670,0x10293857,0x0abcfe23,0x02938571,0xabcfe230,0x717833aa,0x17833aa6]

    for _ in range(1000):
        dut.address.value = randBits(3)
        await Timer(1, units='ns')
        data = int(dut.data.value)
        assertEquals(data, table[int(dut.address.value)], "expect dut data to match table")



    dut._log.info("Cocotb test done")

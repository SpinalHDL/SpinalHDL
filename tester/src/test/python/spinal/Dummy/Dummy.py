import cocotb
from cocotb.triggers import Timer


@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    yield Timer(1000)
    dut.log.info("Cocotb test done")

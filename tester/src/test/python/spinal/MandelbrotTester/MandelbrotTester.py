import cocotb
from cocotb.triggers import RisingEdge

from cocotblib.misc import assertEquals, ClockDomainAsyncReset, BoolRandomizer, simulationSpeedPrinter


@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    ref = [ 2,2,2,3,3,4,5,12,16,3,3,2,1,1,1,1,
            2,2,3,3,3,4,12,16,8,4,3,3,2,2,1,1,
            2,3,3,4,5,6,16,16,16,6,4,3,2,2,1,1,
            3,4,4,8,16,16,16,16,16,16,16,10,3,2,2,1,
            4,4,5,11,16,16,16,16,16,16,16,8,4,2,2,2,
            7,6,7,16,16,16,16,16,16,16,16,16,4,3,2,2,
            16,16,12,16,16,16,16,16,16,16,16,16,4,3,2,2,
            16,16,16,16,16,16,16,16,16,16,16,16,4,3,2,2,
            16,16,16,16,16,16,16,16,16,16,16,6,4,3,2,2,
            16,16,16,16,16,16,16,16,16,16,16,16,4,3,2,2,
            16,16,12,16,16,16,16,16,16,16,16,16,4,3,2,2,
            7,6,7,16,16,16,16,16,16,16,16,16,4,3,2,2,
            4,4,5,11,16,16,16,16,16,16,16,8,4,2,2,2,
            3,4,4,8,16,16,16,16,16,16,16,10,3,2,2,1,
            2,3,3,4,5,6,16,16,16,6,4,3,2,2,1,1,
            2,2,3,3,3,4,12,16,8,4,3,3,2,2,1,1]
    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    cocotb.fork(simulationSpeedPrinter(dut.clk))
    readyRandomizer = BoolRandomizer()
    dut.io_cmdPort_valid <= 0;
    dut.io_retPort_ready <= 1;
    dut.io_pixelResult_ready <= 1;
    for i in range(0,len(ref)):
        while True:
            yield RisingEdge(dut.clk)
            if int(dut.io_pixelResult_valid) == 1 and int(dut.io_pixelResult_ready) == 1:
                assertEquals(ref[i], dut.io_pixelResult_payload_fragment_iteration,"io_pixelResult_payload_fragment_iteration")
                assertEquals(i == len(ref)-1, dut.io_pixelResult_payload_last,"io_pixelResult_payload_last")
                break
            dut.io_pixelResult_ready <= readyRandomizer.get()

    dut.log.info("Cocotb test done")

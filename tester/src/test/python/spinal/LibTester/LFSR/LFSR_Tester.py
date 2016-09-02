import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge

from spinal.common.ClockDomain        import ClockDomain, RESET_ACTIVE_LEVEL

@cocotb.test()
def test_fibonacci(dut):

    dut.log.info("Cocotb test LFSR fibonacci")

    clockDomain  = ClockDomain(dut.clk, 500, None , RESET_ACTIVE_LEVEL.LOW)

    # Start clock
    cocotb.fork(clockDomain.start())

    # Init IO and wait the end of the reset
    dut.io_fib_inc       <= 0
    dut.io_fib_init      <= 0
    dut.io_fib_seed      <= 0
    dut.io_fib_rightLeft <= 1

    yield RisingEdge(dut.clk)
    yield RisingEdge(dut.clk)
    yield RisingEdge(dut.clk)

    # init LFSR
    initValue = 0xACE1
    dut.io_fib_init <= 1
    dut.io_fib_seed      <= initValue

    lfsr = FibonacciLFSR(initValue)

    yield RisingEdge(dut.clk)

    dut.io_fib_init <= 0

    yield RisingEdge(dut.clk)
    yield RisingEdge(dut.clk)


    for i in range(0,10):
        dut.io_fib_inc <= 1

        yield RisingEdge(dut.clk)

        dut.io_fib_inc <= 0

        yield RisingEdge(dut.clk)

        lfsr.getRand()
        realResult = int(dut.io_fib_result)

        if(lfsr.state != realResult):
            print("python" , hex(lfsr.state ))
            print("lfsr ", hex(realResult))

        yield RisingEdge(dut.clk)


    dut.io_fib_inc <= 0

    yield RisingEdge(dut.clk)


    dut.log.info("Cocotb test LFSR fibonacci")



class GaloisLFSR:
    # 16-bit
    def __init__(self, seed):
        self.state = seed & 0xFFFF

    def getRand(self):
        self.state = (self.state >> 1) ^ (-(self.state&0x01) & 0xB400)
        return self.state & 0x01



class FibonacciLFSR:
    # 16-bit
    def __init__(self, seed):
        self.state = seed & 0xFFFF

    def getRand(self):
        feedback = ((self.state >> 0) ^ (self.state >> 2) ^ (self.state >> 3) ^ (self.state >> 5)) & 0x01
        self.state = ((self.state >> 1) | (feedback<<15)) & 0xFFFF
        return self.state & 0x01
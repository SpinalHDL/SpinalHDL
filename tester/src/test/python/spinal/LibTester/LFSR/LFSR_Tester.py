import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge

from spinal.common.ClockDomain        import ClockDomain, RESET_ACTIVE_LEVEL

from spinal.common.misc import assertEquals, randInt

###############################################################################
# Fibonacci LFSR right
#
# @cocotb.test()
# def test_fibonacci_right(dut):
#
#     dut.log.info("Cocotb test LFSR fibonacci Right")
#
#     clockDomain  = ClockDomain(dut.clk, 500, None , RESET_ACTIVE_LEVEL.LOW)
#
#     # Start clock
#     cocotb.fork(clockDomain.start())
#
#     # Init IO and wait the end of the reset
#     dut.io_fib_inc       <= 0
#     dut.io_fib_init      <= 0
#     dut.io_fib_seed      <= 0
#     dut.io_fib_rightLeft <= 1
#
#     yield RisingEdge(dut.clk)
#     yield RisingEdge(dut.clk)
#     yield RisingEdge(dut.clk)
#
#     # init LFSR
#     widthData =32
#     initValue = randInt(0, 2**widthData)
#     dut.io_fib_init <= 1
#     dut.io_fib_seed <= initValue
#
#     lfsr = FibonacciLFSR(initValue, [0,2,3,5,10], widthData)
#
#     yield RisingEdge(dut.clk)
#
#     dut.io_fib_init <= 0
#
#     yield RisingEdge(dut.clk)
#     yield RisingEdge(dut.clk)
#
#
#     for _ in range(0,10):
#         dut.io_fib_inc <= 1
#
#         yield RisingEdge(dut.clk)
#
#         dut.io_fib_inc <= 0
#
#         yield RisingEdge(dut.clk)
#
#         lfsr.getRand()
#         realResult = int(dut.io_fib_result)
#
#         assertEquals(lfsr.state, realResult, "LFSR Fibonacci Right - Comparaison Error %s - %s" % (hex(lfsr.state), hex(realResult)))
#
#
#         yield RisingEdge(dut.clk)
#
#
#     dut.io_fib_inc <= 0
#
#     yield RisingEdge(dut.clk)
#
#
#     dut.log.info("Cocotb test LFSR fibonacci Right")

###############################################################################
# Fibonacci LFSR Left
#
@cocotb.test()
def test_fibonacci_right(dut):

    dut.log.info("Cocotb test LFSR fibonacci left")

    clockDomain  = ClockDomain(dut.clk, 500, None , RESET_ACTIVE_LEVEL.LOW)

    # Start clock
    cocotb.fork(clockDomain.start())

    # Init IO and wait the end of the reset
    dut.io_fib_inc       <= 0
    dut.io_fib_init      <= 0
    dut.io_fib_seed      <= 0
    dut.io_fib_rightLeft <= 0

    yield RisingEdge(dut.clk)
    yield RisingEdge(dut.clk)

    # init LFSR
    widthData =32
    initValue = randInt(0, 2**widthData)
    dut.io_fib_init <= 1
    dut.io_fib_seed <= initValue

    lfsr = FibonacciLFSR(initValue, [0,2,3,5,10], widthData, False)

    yield RisingEdge(dut.clk)

    dut.io_fib_init <= 0

    yield RisingEdge(dut.clk)
    yield RisingEdge(dut.clk)


    for _ in range(0,10):
        dut.io_fib_inc <= 1

        yield RisingEdge(dut.clk)

        dut.io_fib_inc <= 0

        yield RisingEdge(dut.clk)

        lfsr.getRand()
        realResult = int(dut.io_fib_result)

        assertEquals(lfsr.state, realResult, "LFSR Fibonacci Left - Comparaison Error python : %s - %s : hdl" % (hex(lfsr.state), hex(realResult)))


        yield RisingEdge(dut.clk)


    dut.io_fib_inc <= 0

    yield RisingEdge(dut.clk)


    dut.log.info("Cocotb test LFSR fibonacci left")

###############################################################################
# Galois LSFR
#
@cocotb.test()
def test_galois(dut):

    dut.log.info("Cocotb test LFSR Galois")

    clockDomain  = ClockDomain(dut.clk, 500, None , RESET_ACTIVE_LEVEL.LOW)

    # Start clock
    cocotb.fork(clockDomain.start())

    # Init IO and wait the end of the reset
    dut.io_gal_inc       <= 0
    dut.io_gal_init      <= 0
    dut.io_gal_seed      <= 0
    dut.io_gal_rightLeft <= 1

    yield RisingEdge(dut.clk)
    yield RisingEdge(dut.clk)
    yield RisingEdge(dut.clk)

    # init LFSR
    initValue = 0xACE1
    dut.io_gal_init <= 1
    dut.io_gal_seed      <= initValue

    lfsr = GaloisLFSR(initValue)

    yield RisingEdge(dut.clk)

    dut.io_gal_init <= 0

    yield RisingEdge(dut.clk)
    yield RisingEdge(dut.clk)


    for i in range(0,10):
        dut.io_gal_inc <= 1

        yield RisingEdge(dut.clk)

        dut.io_gal_inc <= 0

        yield RisingEdge(dut.clk)

        lfsr.getRand()
        realResult = int(dut.io_gal_result)

        if(lfsr.state != realResult):
            print("python" , hex(lfsr.state ))
            print("lfsr ", hex(realResult))

        yield RisingEdge(dut.clk)


    yield RisingEdge(dut.clk)


    dut.log.info("Cocotb test LFSR galois")



###############################################################################
# Python implementation of a Galois LFSR
#
class GaloisLFSR:
    # 16-bit
    def __init__(self, seed):
        self.state = seed & 0xFFFF

    def getRand(self):
        self.state = (self.state >> 1) ^ (-(self.state&0x01) & 0xB400)
        return self.state & 0x01


###############################################################################
# Python implementation of a Fibonacci LFSR
#
class FibonacciLFSR:
    # 16-bit
    def __init__(self, seed, equation=[0,2,3,5], widthReg=16, rightLeft = True):
        self.equation  = equation
        self.rightLeft = rightLeft
        self.width     = widthReg-1
        self.maxValue  = (2**widthReg)-1
        self.state     = seed & self.maxValue


    def getRand(self):

        feedback = (self.state >> self.equation[0]) ^ (self.state >> self.equation[1])

        for index in range(2,len(self.equation)):
            feedback ^= (self.state >> self.equation[index])
        feedback &= 0x01

        print(feedback)
        if self.rightLeft:
            self.state = ((self.state >> 1) | (feedback<<self.width)) & self.maxValue
        else:
            self.state = (((self.state << 1) & (self.maxValue << 1)) | (feedback)) & self.maxValue

        return self.state & 0x01
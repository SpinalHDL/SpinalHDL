import cocotb
from cocotb.triggers import Timer, RisingEdge


###############################################################################
# The different kind of reset active level
#
class RESET_ACTIVE_LEVEL:
    HIGH = 1
    LOW  = 0


###############################################################################
# Clock
#
# Usage :
#
#    # Create a clock with a reset active high
#    clockDomain = ClockDomain(dut.clk, 400, dut.reset, RESET_ACTIVE_LEVEL.HIGH)
#    cocobt.fork( clockDomain.start() )
#
class ClockDomain:


    ##########################################################################
    # Constructor
    #
    # @param clk              : Clock generated
    # @param halfPeriod       : Half period time
    # @param reset            : Reset generated
    # @param resetactiveLevel : Reset active low or high
    def __init__(self, clk, halfPeriod, reset=None, resetActiveLevel=RESET_ACTIVE_LEVEL.LOW):

        self.halfPeriod = halfPeriod

        self.clk       = clk
        self.reset     = reset
        self.typeReset = resetActiveLevel


    ##########################################################################
    # Generate the clock signals
    @cocotb.coroutine
    def start(self):

        if self.reset:
            self.reset <= self.typeReset

        self.clk <= 0

        yield Timer(self.halfPeriod * 3)

        if self.reset:
            self.reset <= 1 if self.typeReset == RESET_ACTIVE_LEVEL.LOW else 0

        while True:
            self.clk <= 0
            yield Timer(self.halfPeriod)
            self.clk <= 1
            yield Timer(self.halfPeriod)


    ##########################################################################
    # Display the frequency of the clock domain
    def __str__(self):
        return self.__class__.__name__ + "(%3.1fMHz)" % self.frequency

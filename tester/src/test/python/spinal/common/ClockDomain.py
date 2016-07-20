import cocotb
from cocotb.triggers import Timer, RisingEdge, Event


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

        self.event_endReset = Event()


    ##########################################################################
    # Generate the clock signals
    @cocotb.coroutine
    def start(self):

        self.fork_gen = cocotb.fork(self._clkGen())
        cocotb.fork(self._waitEndReset())

        if self.reset:
            self.reset <= self.typeReset

        yield Timer(self.halfPeriod * 5)

        if self.reset:
            self.reset <= 1 if self.typeReset == RESET_ACTIVE_LEVEL.LOW else 0


    ##########################################################################
    # Stop all processes
    def stop(self):

        self.fork_gen.kill()


    ##########################################################################
    # Generate the clk
    @cocotb.coroutine
    def _clkGen(self):
        while True:
            self.clk <= 0
            yield Timer(self.halfPeriod)
            self.clk <= 1
            yield Timer(self.halfPeriod)


    ##########################################################################
    # Wait the end of the reset
    @cocotb.coroutine
    def _waitEndReset(self):
        while True:
            yield RisingEdge(self.clk)
            valueReset = 1 if self.typeReset == RESET_ACTIVE_LEVEL.LOW else 0
            if int(self.reset) == valueReset:
                self.event_endReset.set()
                break;


    ##########################################################################
    # Display the frequency of the clock domain
    def __str__(self):
        return self.__class__.__name__ + "(%3.1fMHz)" % self.frequency

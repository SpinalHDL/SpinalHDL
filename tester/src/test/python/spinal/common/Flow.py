
import cocotb
from cocotb.triggers import RisingEdge, Event

from spinal.common.misc import Bundle


###############################################################################
# Flow
#
class Flow:

    #==========================================================================
    # Constructor
    #==========================================================================
    def __init__(self, dut, name):

        # interface
        self.valid = dut.__getattr__(name + "_valid")
        self.payload = Bundle(dut,name + "_payload")

        # Event
        self.event_valid = Event()


    #==========================================================================
    # Start to monitor the valid signal
    #==========================================================================
    def startMonitoringValid(self, clk):
        self.fork_valid = cocotb.fork(self.monitor_valid())
        self.clk  = clk


    #==========================================================================
    # Monitor the valid signal
    #==========================================================================
    @cocotb.coroutine
    def monitor_valid(self):
        while True:
            yield RisingEdge(self.clk)
            if int(self.valid) == 1:
                self.event_valid.set( self.io.payload )

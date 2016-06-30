###############################################################################
# Test for the I2C  HAL
#
###############################################################################

import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event

from spinal.common.misc import assertEquals
from spinal.common.ClockDomain import ClockDomain, RESET_ACTIVE_LEVEL



###############################################################################
# Slave class
class Slave:

    def __init__(self,dut):
        self.io = Slave.IO(dut)

        # Event -------------------------------------------
        self.event_cmd_valid = Event()

    class IO:

        def __init__ (self, dut):
            # CMD ---------------------------------------------
            self.cmd_ready = dut.io_ioSlave_cmd_ready
            self.cmd_valid = dut.io_ioSlave_cmd_valid
            self.cmd_mode  = dut.io_ioSlave_cmd_payload_mode
            self.cmd_data  = dut.io_ioSlave_cmd_payload_data
            # RSP ---------------------------------------------
            self.rsp_ready = dut.io_ioSlave_rsp_valid
            self.rsp_valid = dut.io_ioSlave_rsp_valid
            self.rsp_mode  = dut.io_ioSlave_rsp_payload_mode
            self.rsp_data  = dut.io_ioSlave_rsp_payload_data
            # Clk & Rst ---------------------------------------
            self.clk       = dut.clk
            self.resetn    = dut.resetn

        def init(self):
            self.rsp_valid <= 0
            self.rsp_mode  <= 0
            self.rsp_data  <= 0
            self.cmd_ready <= 0

    class RSP:
        DATA = 0
        NONE = 1
        ACK  = 2

    class CMD:
        START = 0
        NACK  = 1
        ACK   = 2
        STOP  = 3
        DATA  = 4

    @cocotb.coroutine
    def monitor_cmd_valid(self):
        while True:
            yield RisingEdge(self.io.clk)
            if int(self.io.cmd_valid) == 1:
                self.event_cmd_valid.set()


###############################################################################
# Master class
class Master:

    def __init__(self,dut):
        self.io = Master.IO(dut)

        # Event -------------------------------------------
        self.event_cmd_ready = Event()

    class IO:
        def __init__ (self, dut):
            # CMD ---------------------------------------------
            self.cmd_ready = dut.io_ioMaster_cmd_ready
            self.cmd_valid = dut.io_ioMaster_cmd_valid
            self.cmd_mode  = dut.io_ioMaster_cmd_payload_mode
            self.cmd_data  = dut.io_ioMaster_cmd_payload_data
            # RSP ---------------------------------------------
            self.rsp_valid = dut.io_ioMaster_rsp_valid
            self.rsp_mode  = dut.io_ioMaster_rsp_payload_mode
            self.rsp_data  = dut.io_ioMaster_rsp_payload_data
            # Clk & Rst ---------------------------------------
            self.clk       = dut.clk
            self.resetn    = dut.resetn

        def init(self):
            self.cmd_valid <= 0
            self.cmd_mode  <= 0
            self.cmd_data  <= 0

    class RSP:
        ACK       = 0
        NACK      = 1
        DATA      = 2
        COLLISION = 3

    class CMD:
        START = 0
        WRITE = 1
        READ  = 2
        ACK   = 3
        NACK  = 4
        STOP  = 5

    @cocotb.coroutine
    def monitor_cmd_ready(self):
        while True:
            yield RisingEdge(self.io.clk)
            if int(self.io.cmd_ready) == 1:
                self.event_cmd_ready.set()




@cocotb.coroutine
def masterManager(masterHelp):

    io = masterHelp.io

    io.init()

    yield RisingEdge(io.clk)

    # Send start condition
    io.cmd_valid <= 1
    io.cmd_mode  <= Master.CMD.START

    yield masterHelp.event_cmd_ready.wait()

    io.cmd_valid <= 0

    yield RisingEdge(io.clk)

    # Send a data
    io.cmd_valid <= 1
    io.cmd_mode  <= Master.CMD.WRITE
    io.cmd_data  <= 0x39

    yield masterHelp.event_cmd_ready.wait()

    io.cmd_valid <= 0

    yield RisingEdge(io.clk)

    # Send a data
    io.cmd_valid <= 1
    io.cmd_mode  <= Master.CMD.ACK
    io.cmd_data  <= 0x39

    yield masterHelp.event_cmd_ready.wait()

    io.cmd_valid <= 0






@cocotb.coroutine
def slaveManager(slaveHelper):

    io = slaveHelper.io

    io.init()

    yield RisingEdge(io.clk)

    yield slaveHelper.event_cmd_valid.wait()

    io.cmd_ready <= 1
    yield RisingEdge(io.clk)
    io.cmd_ready <= 0






###############################################################################
# Test a sequence of read
@cocotb.test()
def test_scenario_1(dut):

    dut.log.info("Cocotb I2C HAL - Scenario 1")

    masterHelper = Master(dut)
    slaveHelper  = Slave(dut)

    clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    cocotb.fork(clockDomain.start())
    cocotb.fork(masterManager(masterHelper))
    cocotb.fork(slaveManager(slaveHelper))
    cocotb.fork(masterHelper.monitor_cmd_ready())
    cocotb.fork(slaveHelper.monitor_cmd_valid())


    yield Timer(3000000)

    dut.log.info("I2C HAL Test Done - Scenario 1")


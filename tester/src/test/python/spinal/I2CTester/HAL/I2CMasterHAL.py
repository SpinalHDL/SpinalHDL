import cocotb
from cocotb.triggers import RisingEdge, Event

from spinal.common.misc import assertEquals

from spinal.I2CTester.HAL.I2CHAL import *


###############################################################################
# I2C Master HAL Helper class
#
class I2CMasterHAL:

    def __init__(self,dut):
        # IO definition -----------------------------------
        self.io = I2CMasterHAL.IO(dut)

        # Event -------------------------------------------
        self.event_cmd_ready = Event()
        self.event_rsp_valid = Event()

        # Start process -----------------------------------
        cocotb.fork(self.monitor_cmd_ready())
        cocotb.fork(self.monitor_rsp_valid())


    #==========================================================================
    # Rename IO
    #==========================================================================
    class IO:

        def __init__ (self, dut):
            # I2C ---------------------------------------------
            self.sda_wr    = dut.io_i2c_sda_write
            self.sda_rd    = dut.io_i2c_sda_read
            self.scl_wr    = dut.io_i2c_scl_write
            self.scl_rd    = dut.io_i2c_scl_read
            # Config ------------------------------------------
            self.config_clockDivider = dut.io_config_clockDivider
            # CMD ---------------------------------------------
            self.cmd_ready = dut.io_cmd_ready
            self.cmd_valid = dut.io_cmd_valid
            self.cmd_mode  = dut.io_cmd_payload_mode
            self.cmd_data  = dut.io_cmd_payload_data
            # RSP ---------------------------------------------
            self.rsp_valid = dut.io_rsp_valid
            self.rsp_mode  = dut.io_rsp_payload_mode
            self.rsp_data  = dut.io_rsp_payload_data
            # Clk & Rst ---------------------------------------
            self.clk       = dut.clk
            self.resetn    = dut.resetn

        def init(self, sclDivider):
            self.cmd_valid <= 0
            self.cmd_mode  <= 0
            self.cmd_data  <= 0
            self.sda_rd    <= 1
            self.scl_rd    <= 1
            self.config_clockDivider <= sclDivider


    #==========================================================================
    # RSP mode
    #==========================================================================
    class RSP:

        ACK       = 0
        NACK      = 1
        DATA      = 2
        COLLISION = 3


    #==========================================================================
    # CMD  mode
    #==========================================================================
    class CMD:

        START = 0
        WRITE = 1
        READ  = 2
        ACK   = 3
        NACK  = 4
        STOP  = 5


    #==========================================================================
    # Monitor the cmd_ready signal
    #==========================================================================
    @cocotb.coroutine
    def monitor_cmd_ready(self):
        while True:
            yield RisingEdge(self.io.clk)
            if int(self.io.cmd_ready) == 1:
                self.event_cmd_ready.set()


    #==========================================================================
    # Monitor the rsp_valid signal
    #==========================================================================
    @cocotb.coroutine
    def monitor_rsp_valid(self):
        while True:
            yield RisingEdge(self.io.clk)
            if int(self.io.rsp_valid) == 1:
                payload = (self.io.rsp_mode, self.io.rsp_data)
                self.event_rsp_valid.set(payload)


    #==========================================================================
    # Execute a list of operations
    #==========================================================================
    @cocotb.coroutine
    def execOperations(self, listOperation):

        # get all io of the master
        io = self.io

        # execute all operations
        for index in range(0, len(listOperation)):

            operation = listOperation[index]

            # START ---------------------------------------------------------------
            if isinstance(operation, START):

                yield Timer(operation.delayInput)

                io.cmd_valid  <= 1
                io.cmd_mode   <= I2CMasterHAL.CMD.START
                io.cmd_data   <= 0x0

                yield self.event_cmd_ready.wait()
                io.cmd_valid  <= 0

                yield RisingEdge(io.clk)

            # WRITE ---------------------------------------------------------------
            elif isinstance(operation, WRITE):

                yield Timer(operation.delayInput)

                io.cmd_valid  <= 1
                io.cmd_mode   <= I2CMasterHAL.CMD.WRITE
                io.cmd_data   <= operation.data

                yield self.event_cmd_ready.wait()
                io.cmd_valid  <= 0

                yield RisingEdge(io.clk)

            # READ ----------------------------------------------------------------
            elif isinstance(operation, READ):

                yield Timer(operation.delayInput)

                io.cmd_valid  <= 1
                io.cmd_mode   <= I2CMasterHAL.CMD.READ
                io.cmd_data   <= 0

                yield self.event_cmd_ready.wait()
                io.cmd_valid  <= 0

                yield RisingEdge(io.clk)

            # ACK/NACK  -----------------------------------------------------------
            elif isinstance(operation, ACK) or isinstance(operation, NACK):

                prevOperation = listOperation[index-1]

                if isinstance(prevOperation, READ):

                    yield Timer(operation.delayInput)

                    io.cmd_valid  <= 1
                    io.cmd_mode   <= I2CMasterHAL.CMD.ACK if isinstance(operation,ACK) else I2CMasterHAL.CMD.NACK
                    io.cmd_data   <= 0

                    yield self.event_cmd_ready.wait()
                    io.cmd_valid  <= 0

                    yield RisingEdge(io.clk)
                #else:
                    #yield FallingEdge(io.scl_rd)

            # STOP ----------------------------------------------------------------
            elif isinstance(operation, STOP):

                yield Timer(operation.delayInput)

                io.cmd_valid  <= 1
                io.cmd_mode   <= I2CMasterHAL.CMD.STOP
                io.cmd_data   <= 0
                yield RisingEdge(io.clk)

                yield self.event_cmd_ready.wait()
                io.cmd_valid  <= 0

                yield RisingEdge(io.clk)


    #==========================================================================
    # Check the response received from the master
    #==========================================================================
    @cocotb.coroutine
    def checkResponse(self, listOperations):

        for index in range(0, len(listOperations)-1):
            operation = listOperations[index]

            # START/STOP => no response -------------------------------------------
            if isinstance(operation, START) or isinstance(operation,STOP):
                # no response
                pass

            # READ/WRITE => Data response -----------------------------------------
            elif isinstance(operation,WRITE) or isinstance(operation,READ):
                yield self.event_rsp_valid.wait()
                (payloadMode, payloadData) = self.event_rsp_valid.data
                assertEquals(payloadData, operation.data       , "DATA : Rsp data is wrong")
                assertEquals(payloadMode, I2CMasterHAL.RSP.DATA, "DATA : Rsp mode received is wrong")

            # ACK/NACK => ACK/NACK response ---------------------------------------
            elif isinstance(operation,ACK) or isinstance(operation,NACK):
                yield self.event_rsp_valid.wait()
                (payloadMode, _) = self.event_rsp_valid.data
                rspMode = I2CMasterHAL.RSP.ACK if isinstance(operation,ACK) else I2CMasterHAL.RSP.NACK
                assertEquals(payloadMode, rspMode, "ACK : Rsp mode received is wrong")

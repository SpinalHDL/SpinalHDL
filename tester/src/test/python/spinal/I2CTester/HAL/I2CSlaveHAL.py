
import cocotb
from cocotb.triggers import RisingEdge, Event

from spinal.I2CTester.HAL.I2CHAL import *
from spinal.common.misc import assertEquals

###############################################################################
# I2C Slave HAL Helper class
#
class I2CSlaveHAL:

    def __init__(self,dut):

        # IO definition -----------------------------------
        self.io = I2CSlaveHAL.IO(dut)

        # Event -------------------------------------------
        self.event_cmd_valid = Event()
        self.event_rsp_ready = Event()

        # Start process -----------------------------------
        cocotb.fork(self.monitor_cmd_valid())
        cocotb.fork(self.monitor_rsp_ready())


    #==========================================================================
    # Rename IO
    #==========================================================================
    class IO:

        def __init__ (self, dut):
            # I2C ---------------------------------------------
            self.sda_wr   = dut.io_i2c_sda_write
            self.sda_rd   = dut.io_i2c_sda_read
            self.scl_wr   = dut.io_i2c_scl_write
            self.scl_rd   = dut.io_i2c_scl_read
            # CMD ---------------------------------------------
            self.cmd_valid = dut.io_cmd_valid
            self.cmd_mode  = dut.io_cmd_payload_mode
            self.cmd_data  = dut.io_cmd_payload_data
            # RSP ---------------------------------------------
            self.rsp_ready = dut.io_rsp_ready
            self.rsp_valid = dut.io_rsp_valid
            self.rsp_mode  = dut.io_rsp_payload_mode
            self.rsp_data  = dut.io_rsp_payload_data
            # Clk & Rst ---------------------------------------
            self.clk       = dut.clk
            self.resetn    = dut.resetn

        def init(self):
            self.rsp_valid <= 0
            self.rsp_mode  <= 0
            self.rsp_data  <= 0
            self.sda_rd    <= 1
            self.scl_rd    <= 1


    #==========================================================================
    # RSP mode
    #==========================================================================
    class RSP:
        DATA = 0
        NONE = 1
        ACK  = 2


    #==========================================================================
    # CMD  mode
    #==========================================================================
    class CMD:
        START = 0
        NACK  = 1
        ACK   = 2
        STOP  = 3
        DATA  = 4


    #==========================================================================
    # Monitor the cmd_valid signal
    #==========================================================================
    @cocotb.coroutine
    def monitor_cmd_valid(self):
        while True:
            yield RisingEdge(self.io.clk)
            if int(self.io.cmd_valid) == 1:
                self.event_cmd_valid.set(( int(self.io.cmd_mode), int(self.io.cmd_data)))


    #==========================================================================
    # Monitor the rsp_ready signal
    #==========================================================================
    @cocotb.coroutine
    def monitor_rsp_ready(self):
        while True:
            yield RisingEdge(self.io.clk)
            if int(self.io.rsp_ready) == 1:
                self.event_rsp_ready.set()


    #==========================================================================
    # Check the command received from the slave
    #==========================================================================
    @cocotb.coroutine
    def checkResponse(self, listOperation):

        io = self.io

        # check command
        for index in range(0, len(listOperation)):

            operation = listOperation[index]

            # Start -----------------------------------------------------------
            if isinstance(operation, START):

                yield self.event_cmd_valid.wait()
                (payload_mode, payload_data) = self.event_cmd_valid.data
                assertEquals(payload_mode, I2CSlaveHAL.CMD.START, "DATA START : Rsp mode received is wrong")

            # Write -----------------------------------------------------------
            elif isinstance(operation, WRITE):
                yield self.event_cmd_valid.wait()
                (payload_mode, payload_data) = self.event_cmd_valid.data
                assertEquals(payload_mode, I2CSlaveHAL.CMD.DATA, "DATA : Rsp mode received is wrong")
                assertEquals(payload_data, operation.data, "DATA : Rsp mode received is wrong")

            # Read -----------------------------------------------------------
            elif isinstance(operation,READ):
                yield self.event_cmd_valid.wait()
                (payload_mode, payload_data) = self.event_cmd_valid.data
                assertEquals(payload_mode, I2CSlaveHAL.CMD.DATA, "DATA : Rsp mode received is wrong")
                assertEquals(payload_data, operation.data, "DATA : Rsp mode received is wrong")

            # ACK/NACK --------------------------------------------------------
            elif isinstance(operation,ACK):
                yield self.event_cmd_valid.wait()
                (payload_mode, payload_data) = self.event_cmd_valid.data
                assertEquals(payload_mode, I2CSlaveHAL.CMD.ACK, "DATA ACK : Rsp mode received is wrong")


            elif isinstance(operation, NACK):
                yield self.event_cmd_valid.wait()
                (payload_mode, payload_data) = self.event_cmd_valid.data
                assertEquals(payload_mode, I2CSlaveHAL.CMD.NACK, "DATA NACK : Rsp mode received is wrong")

            # Stop ------------------------------------------------------------
            if isinstance(operation, STOP):
                yield self.event_cmd_valid.wait()
                (payload_mode, payload_data) = self.event_cmd_valid.data
                assertEquals(payload_mode, I2CSlaveHAL.CMD.STOP, "DATA STOP : Rsp mode received is wrong")


    #==========================================================================
    # Execute a list of operations
    #==========================================================================
    @cocotb.coroutine
    def execOperations(self, listOperation):

        # get all io of the master
        io = self.io

        # process all commands
        for index in range(0,len(listOperation)):

            operation = listOperation[index]

            # Start -----------------------------------------------------------
            if isinstance(operation, START):
                if index != 0 :
                    io.rsp_valid <= 1
                    io.rsp_mode <= I2CSlaveHAL.RSP.NONE
                    io.rsp_data  <= 0
                    yield RisingEdge(io.clk)

                yield self.event_cmd_valid.wait()

                if index != 0:
                    yield self.event_rsp_ready.wait()


            # Write -----------------------------------------------------------
            elif isinstance(operation, WRITE):
                io.rsp_valid <= 1
                io.rsp_mode <= I2CSlaveHAL.RSP.NONE
                io.rsp_data  <= 0

                yield self.event_rsp_ready.wait()

                io.rsp_valid <= 0

                yield RisingEdge(io.clk)

            # Read -----------------------------------------------------------
            elif isinstance(operation,READ):
                io.rsp_valid <= 1
                io.rsp_mode <= I2CSlaveHAL.RSP.DATA
                io.rsp_data  <= operation.data

                yield self.event_rsp_ready.wait()

                io.rsp_valid <= 0

                yield RisingEdge(io.clk)

            # ACK/NACK --------------------------------------------------------
            elif isinstance(operation,ACK) or isinstance(operation, NACK):

                prevOperation = listOperation[index-1]

                if isinstance(prevOperation, WRITE):
                    io.rsp_valid <= 1
                    io.rsp_mode <= I2CSlaveHAL.RSP.ACK if isinstance(operation,ACK) else I2CSlaveHAL.RSP.NONE
                    io.rsp_data  <= 0

                elif isinstance(prevOperation, READ):
                    io.rsp_valid <= 1
                    io.rsp_mode <= I2CSlaveHAL.RSP.NONE
                    io.rsp_data  <= 0

                yield self.event_rsp_ready.wait()

                io.rsp_valid <= 0

                yield RisingEdge(io.clk)

            # Stop ------------------------------------------------------------
            if isinstance(operation, STOP):

                io.rsp_valid <= 1
                io.rsp_mode <= I2CSlaveHAL.RSP.NONE
                io.rsp_data  <= 0

                yield self.event_rsp_ready.wait()

                io.rsp_valid <= 0

                yield RisingEdge(io.clk)






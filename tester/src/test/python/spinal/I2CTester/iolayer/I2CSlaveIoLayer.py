from cocotb.triggers import Timer

from cocotblib.misc   import assertEquals
from cocotblib.Stream import Stream
from cocotblib.Flow import Flow
from spinal.I2CTester.iolayer.I2CIoLayer import *


###############################################################################
# I2C Slave HAL Helper class
#
class I2CSlaveIoLayer:

    def __init__(self, dut):

        # IO definition -----------------------------------
        self.io = I2CSlaveIoLayer.IO(dut)

        # Start process -----------------------------------
        self.io.cmd.startMonitoringValid(self.io.clk)
        self.io.rsp.startMonitoringReady(self.io.clk)


    #==========================================================================
    # Stop all processes
    #==========================================================================
    def stop(self):
        self.io.cmd.stopMonitoring()
        self.io.rsp.stopMonitoring()


    #==========================================================================
    # Rename IO
    #==========================================================================
    class IO:

        def __init__ (self, dut):
            # CMD ---------------------------------------------
            self.cmd     = Flow(dut, "io_ioSlave_cmd")
            # RSP ---------------------------------------------
            self.rsp     = Stream(dut, "io_ioSlave_rsp")
            # Clk & Rst ---------------------------------------
            self.clk     = dut.clk
            self.resetn  = dut.resetn

        def init(self):
            self.rsp.valid         <= 0
            self.rsp.payload.data  <= 0


    #==========================================================================
    # CMD  mode
    #==========================================================================
    class CMD:
        START = 0
        DATA  = 1
        STOP  = 2


    # #==========================================================================
    # # Check the command received from the slave
    # #==========================================================================
    # @cocotb.coroutine
    # def checkResponse(self, listOperation):
    #
    #     io = self.io
    #
    #     # check command
    #     for index in range(0, len(listOperation)):
    #
    #         operation = listOperation[index]
    #
    #         # Start -----------------------------------------------------------
    #         if isinstance(operation, START):
    #
    #             yield self.event_cmd_valid.wait()
    #             (payload_mode, payload_data) = self.event_cmd_valid.data
    #             assertEquals(payload_mode, I2CSlaveIoLayer.CMD.START, "DATA START : Rsp mode received is wrong")
    #
    #         # Write -----------------------------------------------------------
    #         elif isinstance(operation, WRITE):
    #             yield self.event_cmd_valid.wait()
    #             (payload_mode, payload_data) = self.event_cmd_valid.data
    #             assertEquals(payload_mode, I2CSlaveIoLayer.CMD.DATA, "DATA : Rsp mode received is wrong")
    #             assertEquals(payload_data, operation.data, "DATA : Rsp mode received is wrong")
    #
    #         # Read -----------------------------------------------------------
    #         elif isinstance(operation,READ):
    #             yield self.event_cmd_valid.wait()
    #             (payload_mode, payload_data) = self.event_cmd_valid.data
    #             assertEquals(payload_mode, I2CSlaveIoLayer.CMD.DATA, "DATA : Rsp mode received is wrong")
    #             assertEquals(payload_data, operation.data, "DATA : Rsp mode received is wrong")
    #
    #         # ACK/NACK --------------------------------------------------------
    #         elif isinstance(operation,ACK):
    #             yield self.event_cmd_valid.wait()
    #             (payload_mode, payload_data) = self.event_cmd_valid.data
    #             assertEquals(payload_mode, I2CSlaveIoLayer.CMD.ACK, "DATA ACK : Rsp mode received is wrong")
    #
    #
    #         elif isinstance(operation, NACK):
    #             yield self.event_cmd_valid.wait()
    #             (payload_mode, payload_data) = self.event_cmd_valid.data
    #             assertEquals(payload_mode, I2CSlaveIoLayer.CMD.NACK, "DATA NACK : Rsp mode received is wrong")
    #
    #         # Stop ------------------------------------------------------------
    #         if isinstance(operation, STOP):
    #             yield self.event_cmd_valid.wait()
    #             (payload_mode, payload_data) = self.event_cmd_valid.data
    #             assertEquals(payload_mode, I2CSlaveIoLayer.CMD.STOP, "DATA STOP : Rsp mode received is wrong")



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

                if index != 0:
                    io.rsp.valid        <= 1
                    io.rsp.payload.data <= 1
                #yield Timer(operation.delayRSP)
                #io.rsp.valid         <= 1
                #io.rsp.payload.data  <= 0

                # wait recepetion of the start
                yield self.io.cmd.event_valid.wait()

                io.rsp.valid <= 0



            # Write -----------------------------------------------------------
            elif isinstance(operation, WRITE_BIT):

                #yield Timer(operation.delayRSP)

                io.rsp.valid         <= 1
                io.rsp.payload.data  <= 1

                yield io.rsp.event_ready.wait()

                io.rsp.valid <= 0

                yield RisingEdge(io.clk)

            # READ  ---------------------------------------------------------------
            elif isinstance(operation, READ_BIT):

                #yield Timer(operation.delayCMD)

                io.rsp.valid         <= 1
                io.rsp.payload.data  <= operation.data

                yield io.rsp.event_ready.wait()
                io.rsp.valid <= 0

                yield RisingEdge(io.clk)


            # Stop ------------------------------------------------------------
            if isinstance(operation, STOP):

                #yield Timer(operation.delayRSP)

                io.rsp.valid         <= 1
                io.rsp.payload.data  <= 1

                yield io.rsp.event_ready.wait()

                io.rsp.valid <= 0

                yield RisingEdge(io.clk)






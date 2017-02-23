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
        self.fork_cmdReady = self.io.cmd.startMonitoringReady()
        self.fork_rspValid = self.io.rsp.startMonitoringValid()

        self.fork_cmdValid = self.io.cmd.startMonitoringValid()
        self.fork_rspReady = self.io.rsp.startMonitoringReady()


    #==========================================================================
    # Stop all processes
    #==========================================================================
    def stop(self):
        self.fork_cmdValid.kill()
        self.fork_rspReady.kill()


    #==========================================================================
    # Rename IO
    #==========================================================================
    class IO:

        def __init__ (self, dut):
            # I2C ---------------------------------------------
            self.sda     = dut.io_i2c_sda
            self.scl     = dut.io_i2c_scl
            # CMD ---------------------------------------------
            self.cmd     = Flow(dut, "ioSlave")
            # RSP ---------------------------------------------
            self.rsp     = Stream(dut, "ioSlave")
            # Clk & Rst ---------------------------------------
            self.clk     = dut.clk
            self.resetn  = dut.resetn

        def init(self):
            self.rsp.valid         <= 0
            self.rsp.palyoad.data  <= 0


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
    #
    #
    # #==========================================================================
    # # Execute a list of operations
    # #==========================================================================
    # @cocotb.coroutine
    # def execOperations(self, listOperation):
    #
    #     # get all io of the master
    #     io = self.io
    #
    #     # process all commands
    #     for index in range(0,len(listOperation)):
    #
    #         operation = listOperation[index]
    #
    #         # Start -----------------------------------------------------------
    #         if isinstance(operation, START):
    #
    #             if index != 0 :
    #                 yield Timer(operation.delayRSP)
    #                 io.rsp_valid <= 1
    #                 io.rsp_mode <= I2CSlaveIoLayer.RSP.NONE
    #                 io.rsp_data  <= 0
    #                 yield RisingEdge(io.clk)
    #
    #             yield self.event_cmd_valid.wait()
    #
    #             if index != 0:
    #                 yield self.event_rsp_ready.wait()
    #
    #
    #         # Write -----------------------------------------------------------
    #         elif isinstance(operation, WRITE):
    #
    #             yield Timer(operation.delayRSP)
    #
    #             io.rsp_valid <= 1
    #             io.rsp_mode <= I2CSlaveIoLayer.RSP.NONE
    #             io.rsp_data  <= 0
    #
    #             yield self.event_rsp_ready.wait()
    #
    #             io.rsp_valid <= 0
    #
    #             yield RisingEdge(io.clk)
    #
    #         # Read -----------------------------------------------------------
    #         elif isinstance(operation,READ):
    #
    #             yield Timer(operation.delayRSP)
    #
    #             io.rsp_valid <= 1
    #             io.rsp_mode <= I2CSlaveIoLayer.RSP.DATA
    #             io.rsp_data  <= operation.data
    #
    #             yield self.event_rsp_ready.wait()
    #
    #             io.rsp_valid <= 0
    #
    #             yield RisingEdge(io.clk)
    #
    #         # ACK/NACK --------------------------------------------------------
    #         elif isinstance(operation,ACK) or isinstance(operation, NACK):
    #
    #             prevOperation = listOperation[index-1]
    #
    #             yield Timer(operation.delayRSP)
    #
    #             if isinstance(prevOperation, WRITE):
    #                 io.rsp_valid <= 1
    #                 io.rsp_mode <= I2CSlaveIoLayer.RSP.ACK if isinstance(operation, ACK) else I2CSlaveIoLayer.RSP.NONE
    #                 io.rsp_data  <= 0
    #
    #             elif isinstance(prevOperation, READ):
    #                 io.rsp_valid <= 1
    #                 io.rsp_mode <= I2CSlaveIoLayer.RSP.NONE
    #                 io.rsp_data  <= 0
    #
    #             yield self.event_rsp_ready.wait()
    #
    #             io.rsp_valid <= 0
    #
    #             yield RisingEdge(io.clk)
    #
    #         # Stop ------------------------------------------------------------
    #         if isinstance(operation, STOP):
    #
    #             yield Timer(operation.delayRSP)
    #
    #             io.rsp_valid <= 1
    #             io.rsp_mode <= I2CSlaveIoLayer.RSP.NONE
    #             io.rsp_data  <= 0
    #
    #             yield self.event_rsp_ready.wait()
    #
    #             io.rsp_valid <= 0
    #
    #             yield RisingEdge(io.clk)
    #
    #
    #
    #
    #

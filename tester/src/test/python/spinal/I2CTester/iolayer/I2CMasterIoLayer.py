from cocotb.triggers import Timer
from cocotblib.Stream import Stream
from cocotblib.Flow import Flow

from spinal.I2CTester.iolayer.I2CIoLayer import *


###############################################################################
# I2C Master Io Layer Helper class
#
class I2CMasterIoLayer:

    def __init__(self,dut):
        # IO definition -----------------------------------
        self.io = I2CMasterIoLayer.IO(dut)

        # Start process -----------------------------------
        self.fork_cmdReady = self.io.cmd.startMonitoringReady()
        self.fork_rspValid = self.io.rsp.startMonitoringValid()


    #==========================================================================
    # Stop all processes
    #==========================================================================
    def stop(self):

        self.fork_cmdReady.kill()
        self.fork_rspValid.kill()


    #==========================================================================
    # Rename IO
    #==========================================================================
    class IO:

        def __init__ (self, dut):
            # I2C ---------------------------------------------
            self.sda     = dut.io_i2c_sda
            self.scl     = dut.io_i2c_scl
            # CMD ---------------------------------------------
            self.cmd     = Stream(dut, "ioMaster")
            # RSP ---------------------------------------------
            self.rsp     = Flow(dut, "ioMaster")
            # Clk & Rst ---------------------------------------
            self.clk     = dut.clk
            self.resetn  = dut.resetn

        def init(self):
            self.cmd.valid         <= 0
            self.cmd.payload.mode  <= 0
            self.cmd.payload.data  <= 0


    #==========================================================================
    # CMD  mode
    #==========================================================================
    class CMD:
        START = 0
        DATA  = 1
        STOP  = 2

    # #==========================================================================
    # # Execute a list of operations
    # #==========================================================================
    # @cocotb.coroutine
    # def execOperations(self, listOperation):
    #
    #     # get all io of the master
    #     io = self.io
    #
    #     # execute all operations
    #     for index in range(0, len(listOperation)):
    #
    #         operation = listOperation[index]
    #
    #         # START ---------------------------------------------------------------
    #         if isinstance(operation, START):
    #
    #             yield Timer(operation.delayCMD)
    #
    #             io.cmd_valid  <= 1
    #             io.cmd_mode   <= I2CMasterHAL.CMD.START
    #             io.cmd_data   <= 0x0
    #
    #             yield self.event_cmd_ready.wait()
    #             io.cmd_valid  <= 0
    #
    #             yield RisingEdge(io.clk)
    #
    #         # WRITE ---------------------------------------------------------------
    #         elif isinstance(operation, WRITE):
    #
    #             yield Timer(operation.delayCMD)
    #
    #             io.cmd_valid  <= 1
    #             io.cmd_mode   <= I2CMasterHAL.CMD.WRITE
    #             io.cmd_data   <= operation.data
    #
    #             yield self.event_cmd_ready.wait()
    #             io.cmd_valid  <= 0
    #
    #             yield RisingEdge(io.clk)
    #
    #         # READ ----------------------------------------------------------------
    #         elif isinstance(operation, READ):
    #
    #             yield Timer(operation.delayCMD)
    #
    #             io.cmd_valid  <= 1
    #             io.cmd_mode   <= I2CMasterHAL.CMD.READ
    #             io.cmd_data   <= 0
    #
    #             yield self.event_cmd_ready.wait()
    #             io.cmd_valid  <= 0
    #
    #             yield RisingEdge(io.clk)
    #
    #         # ACK/NACK  -----------------------------------------------------------
    #         elif isinstance(operation, ACK) or isinstance(operation, NACK):
    #
    #             prevOperation = listOperation[index-1]
    #
    #             if isinstance(prevOperation, READ):
    #
    #                 yield Timer(operation.delayCMD)
    #
    #                 io.cmd_valid  <= 1
    #                 io.cmd_mode   <= I2CMasterHAL.CMD.ACK if isinstance(operation,ACK) else I2CMasterHAL.CMD.NACK
    #                 io.cmd_data   <= 0
    #
    #                 yield self.event_cmd_ready.wait()
    #                 io.cmd_valid  <= 0
    #
    #                 yield RisingEdge(io.clk)
    #             #else:
    #                 #yield FallingEdge(io.scl_rd)
    #
    #         # STOP ----------------------------------------------------------------
    #         elif isinstance(operation, STOP):
    #
    #             yield Timer(operation.delayCMD)
    #
    #             io.cmd_valid  <= 1
    #             io.cmd_mode   <= I2CMasterHAL.CMD.STOP
    #             io.cmd_data   <= 0
    #             yield RisingEdge(io.clk)
    #
    #             yield self.event_cmd_ready.wait()
    #             io.cmd_valid  <= 0
    #
    #             yield RisingEdge(io.clk)
    #
    #
    # #==========================================================================
    # # Check the response received from the master
    # #==========================================================================
    # @cocotb.coroutine
    # def checkResponse(self, listOperations):
    #
    #     for index in range(0, len(listOperations)-1):
    #         operation = listOperations[index]
    #
    #         # START/STOP => no response -------------------------------------------
    #         if isinstance(operation, START) or isinstance(operation,STOP):
    #             # no response
    #             pass
    #
    #         # READ/WRITE => Data response -----------------------------------------
    #         elif isinstance(operation,WRITE) or isinstance(operation,READ):
    #             yield self.event_rsp_valid.wait()
    #             if operation.enCollision == True:
    #                 (payloadMode, payloadData) = self.event_rsp_valid.data
    #                 assertEquals(payloadMode, I2CMasterHAL.RSP.COLLISION, "DATA : Rsp Collision received is wrong")
    #                 break
    #             else:
    #                 (payloadMode, payloadData) = self.event_rsp_valid.data
    #                 assertEquals(payloadData, operation.data       , "DATA : Rsp data is wrong")
    #                 assertEquals(payloadMode, I2CMasterHAL.RSP.DATA, "DATA : Rsp mode received is wrong")
    #
    #         # ACK/NACK => ACK/NACK response ---------------------------------------
    #         elif isinstance(operation,ACK) or isinstance(operation,NACK):
    #             yield self.event_rsp_valid.wait()
    #             (payloadMode, _) = self.event_rsp_valid.data
    #             rspMode = I2CMasterHAL.RSP.ACK if isinstance(operation,ACK) else I2CMasterHAL.RSP.NACK
    #             assertEquals(payloadMode, rspMode, "ACK : Rsp mode received is wrong")

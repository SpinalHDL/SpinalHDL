###############################################################################
# Test for the I2C Slave HAL
#
###############################################################################

import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event

from spinal.common.misc import assertEquals
from spinal.common.ClockDomain import ClockDomain, RESET_ACTIVE_LEVEL
from I2CMasterModelHAL import I2CMasterModelHAL


###############################################################################
# Rsp mode
class RspMode :
    DATA = 0
    NONE = 1
    ACK  = 2

###############################################################################
# Cmd mode
class CmdMode :
    START = 0
    NACK  = 1
    ACK   = 2
    STOP  = 3
    DATA  = 4

###############################################################################
# Test the write read sequence...
@cocotb.coroutine
def writeRead_test(dut, model, eventCmdValid, rspReadyEvent):

    dut.io_cmd_ready        <= 0
    dut.io_rsp_payload_mode <= 0
    dut.io_rsp_valid        <= 0
    dut.io_rsp_payload_data <= 0

    # wait end of the rest
    yield RisingEdge(dut.resetn)

    # Start -------------------------------------------------------------------
    cocotb.fork( model.genStart() )
    yield eventCmdValid.wait()

    assertEquals(eventCmdValid.data["mode"], CmdMode.START, "STRAT : Cmd mode received wrong")

    dut.io_cmd_ready <= 1
    yield RisingEdge(dut.clk)
    dut.io_cmd_ready <= 0

    yield RisingEdge(dut.clk)


    # Write -------------------------------------------------------------------
    dut.io_rsp_valid        <= 1
    dut.io_rsp_payload_mode <= RspMode.NONE

    yield rspReadyEvent.wait()

    dut.io_rsp_valid <= 0

    cocotb.fork(  model.writeData(0x24) )
    yield eventCmdValid.wait()

    assertEquals(eventCmdValid.data["data"], 0x24 , "Data send by the master is not equal to the data read by the slave")
    assertEquals(eventCmdValid.data["mode"], CmdMode.DATA, "DATA : Cmd mode received wrong")

    dut.io_cmd_ready <= 1
    yield RisingEdge(dut.clk)
    dut.io_cmd_ready <= 0

    yield RisingEdge(dut.clk)

    # ACK ---------------------------------------------------------------------
    dut.io_rsp_valid <= 1
    dut.io_rsp_payload_mode <= RspMode.ACK

    yield rspReadyEvent.wait()

    dut.io_rsp_valid <= 0

    yield RisingEdge(dut.clk)


    # Restart -----------------------------------------------------------------
    yield model.sclFalling.wait()
    yield model.sclRising.wait()

    cocotb.fork( model.genStart() )
    yield eventCmdValid.wait()

    assertEquals(eventCmdValid.data["mode"], CmdMode.START, "STRAT : Cmd mode received wrong")

    dut.io_cmd_ready <= 1
    yield RisingEdge(dut.clk)
    dut.io_cmd_ready <= 0

    yield RisingEdge(dut.clk)

    # Read --------------------------------------------------------------------
    dut.io_rsp_valid        <= 1
    dut.io_rsp_payload_mode <= RspMode.DATA
    dut.io_rsp_payload_data <= 0xAA

    cocotb.fork(model.readData())

    yield rspReadyEvent.wait()

    dut.io_rsp_valid <= 0

    yield eventCmdValid.wait()

    dut.io_cmd_ready <= 1
    yield RisingEdge(dut.clk)
    dut.io_cmd_ready <= 0

    dut.io_rsp_valid        <= 1
    dut.io_rsp_payload_mode <= RspMode.NONE

    yield rspReadyEvent.wait()

    dut.io_rsp_valid <= 0

    yield eventCmdValid.wait()


    dut.io_rsp_valid <= 0

    assertEquals(eventCmdValid.data["data"], 0xAA, "Data send by the Slave is not equal to the data read by the slave")
 #   assertEquals(eventCmdValid.data["mode"], CmdMode.DATA, "DATA : Cmd mode received wrong")


    dut.io_cmd_ready <= 1
    yield RisingEdge(dut.clk)
    dut.io_cmd_ready <= 0

    yield eventCmdValid.wait()

    assertEquals(eventCmdValid.data["mode"], CmdMode.ACK or CmdMode.NACK, "ACK/NACK : Cmd mode received wrong")

    yield RisingEdge(dut.clk)

    dut.io_rsp_valid        <= 1
    dut.io_rsp_payload_mode <= RspMode.NONE

    yield rspReadyEvent.wait()

    dut.io_rsp_valid <= 0

    yield RisingEdge(dut.clk)

    # STOP --------------------------------------------------------------------
    cocotb.fork(model.genStop())

    yield eventCmdValid.wait()

    assertEquals(eventCmdValid.data["mode"], CmdMode.STOP, "STOP : Cmd mode received wrong")

    dut.io_cmd_ready <= 1
    yield RisingEdge(dut.clk)
    dut.io_cmd_ready <= 0

    yield RisingEdge(dut.clk)

    dut.io_rsp_valid        <= 1
    dut.io_rsp_payload_mode <= RspMode.NONE

    yield rspReadyEvent.wait()

    dut.io_rsp_valid <= 0

    yield RisingEdge(dut.clk)


###############################################################################
# Test the read
@cocotb.coroutine
def read_test(dut, model, eventCmdValid, rspReadyEvent, data, freeze):

    dut.io_cmd_ready        <= 0
    dut.io_rsp_payload_mode <= 0
    dut.io_rsp_valid        <= 0
    dut.io_rsp_payload_data <= 0

    # wait end of the rest
    yield RisingEdge(dut.resetn)

    # Start -------------------------------------------------------------------
    cocotb.fork( model.genStart() )
    yield eventCmdValid.wait()

    assertEquals(eventCmdValid.data["mode"], CmdMode.START, "START : Cmd mode received wrong")

   # if freeze == True :
    #    yield Timer(200000)

    dut.io_cmd_ready <= 1
    yield RisingEdge(dut.clk)
    dut.io_cmd_ready <= 0

    yield RisingEdge(dut.clk)


    # Read --------------------------------------------------------------------
    for i in range(0,len(data)):
        dut.io_rsp_valid        <= 1
        dut.io_rsp_payload_mode <= RspMode.DATA
        dut.io_rsp_payload_data <= data[i]

        cocotb.fork(model.readData())

        yield rspReadyEvent.wait()

        dut.io_rsp_valid <= 0

        yield eventCmdValid.wait()

        if freeze == True :
            yield Timer(200000)

        dut.io_cmd_ready <= 1
        yield RisingEdge(dut.clk)
        dut.io_cmd_ready <= 0

        dut.io_rsp_valid        <= 1
        dut.io_rsp_payload_mode <= RspMode.NONE

        yield rspReadyEvent.wait()

        dut.io_rsp_valid <= 0

        assertEquals(eventCmdValid.data["data"], data[i], "Data send by the Slave is not equal to the data read by the slave")
        assertEquals(eventCmdValid.data["mode"], CmdMode.DATA, "DATA : Cmd mode received wrong")

        dut.io_cmd_ready <= 1
        yield RisingEdge(dut.clk)
        dut.io_cmd_ready <= 0

        yield eventCmdValid.wait()

        assertEquals(eventCmdValid.data["mode"], CmdMode.ACK or CmdMode.NACK, "ACK/NACK : Cmd mode received wrong")

        if freeze == True :
            yield Timer(200000)

        dut.io_cmd_ready <= 1
        yield RisingEdge(dut.clk)
        dut.io_cmd_ready <= 0

        yield RisingEdge(dut.clk)

        dut.io_rsp_valid        <= 1
        dut.io_rsp_payload_mode <= RspMode.NONE

        yield rspReadyEvent.wait()

        dut.io_rsp_valid <= 0

        yield RisingEdge(dut.clk)


    # STOP --------------------------------------------------------------------
    cocotb.fork(model.genStop())

    yield eventCmdValid.wait()

    assertEquals(eventCmdValid.data["mode"], CmdMode.STOP, "STOP : Cmd mode received wrong")

    dut.io_cmd_ready <= 1

    yield RisingEdge(dut.clk)

    dut.io_cmd_ready <= 0

    yield RisingEdge(dut.clk)

    dut.io_rsp_valid        <= 1
    dut.io_rsp_payload_mode <= RspMode.NONE

    yield rspReadyEvent.wait()

    dut.io_rsp_valid <= 0

    yield RisingEdge(dut.clk)





###############################################################################
# Test the write
@cocotb.coroutine
def write_test(dut, model, eventCmdValid, rspReadyEvent, data, freeze=False):

    dut.io_cmd_ready        <= 0
    dut.io_rsp_payload_mode <= 0
    dut.io_rsp_valid        <= 0
    dut.io_rsp_payload_data <= 0

    # wait end of the rest
    yield RisingEdge(dut.resetn)

    # Start -------------------------------------------------------------------
    cocotb.fork( model.genStart() )
    yield eventCmdValid.wait()

    assertEquals(eventCmdValid.data["mode"], CmdMode.START, "START : Cmd mode received wrong")

    if freeze == True :
        yield Timer(200000)

    dut.io_cmd_ready <= 1
    yield RisingEdge(dut.clk)
    dut.io_cmd_ready <= 0

    yield RisingEdge(dut.clk)

    # Write -------------------------------------------------------------------
    for i in range(0,len(data)):

        dut.io_rsp_valid        <= 1
        dut.io_rsp_payload_mode <= RspMode.NONE

        yield rspReadyEvent.wait()

        dut.io_rsp_valid <= 0

        cocotb.fork(  model.writeData(data[i]) )
        yield eventCmdValid.wait()

    #    assertEquals(eventCmdValid.data["data"], data[i], "Data send by the master is not equal to the data read by the slave")
   #     assertEquals(eventCmdValid.data["mode"], CmdMode.DATA, "DATA : Cmd mode received wrong")

     #   if freeze == True :
      #      yield Timer(200000)

        dut.io_cmd_ready <= 1
        yield RisingEdge(dut.clk)
        dut.io_cmd_ready <= 0

        yield RisingEdge(dut.clk)

        # ACK ---------------------------------------------------------------------
        dut.io_rsp_valid <= 1
        dut.io_rsp_payload_mode <= RspMode.ACK

        yield rspReadyEvent.wait()

        dut.io_rsp_valid <= 0

        yield RisingEdge(dut.clk)

    # STOP --------------------------------------------------------------------
    cocotb.fork(model.genStop())

    yield eventCmdValid.wait()

 #   assertEquals(eventCmdValid.data["mode"], CmdMode.STOP, "STOP : Cmd mode received wrong")

    dut.io_cmd_ready <= 1

    yield RisingEdge(dut.clk)

    dut.io_cmd_ready <= 0

    yield RisingEdge(dut.clk)

    dut.io_rsp_valid        <= 1
    dut.io_rsp_payload_mode <= RspMode.NONE

    yield rspReadyEvent.wait()

    dut.io_rsp_valid <= 0

    yield RisingEdge(dut.clk)


###############################################################################
# Detect the cmd valid signal
@cocotb.coroutine
def monitor_cmd_valid(dut, event):
    while True:
        yield RisingEdge(dut.clk)
        if int(dut.io_cmd_valid) == 1:

            result = dict()
            result["data"]  = int(dut.io_cmd_payload_data)
            result["mode"]  = int(dut.io_cmd_payload_mode)

            event.set(result)


###############################################################################
# Detect the RSP ready signal
@cocotb.coroutine
def monitor_rsp_ready(dut, event):
    while True:
        yield RisingEdge(dut.clk)
        if int(dut.io_rsp_ready) == 1:
            event.set()



###############################################################################
# Test a sequence of write
#@cocotb.test()
def slave_hal_test_write(dut):

    dut.log.info("Cocotb I2C Slave HAL - write Test ")


    cmdValidEvent = Event()
    rspReadyEvent = Event()

    data2Write = [0x65, 0xf1]

    modelMaster = I2CMasterModelHAL(dut.clk, dut.io_i2c_scl_read, dut.io_i2c_sda_read, dut.io_i2c_scl_write, dut.io_i2c_sda_write, 50)

    clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    cocotb.fork(clockDomain.start())
    cocotb.fork(monitor_cmd_valid(dut, cmdValidEvent))
    cocotb.fork(monitor_rsp_ready(dut, rspReadyEvent))
    cocotb.fork(write_test(dut, modelMaster, cmdValidEvent,rspReadyEvent, data2Write))
    modelMaster.startMaster()

    yield Timer(3000000)

    dut.log.info("I2C Slave HAL - write Test done")



###############################################################################
# Test a sequence of read
#@cocotb.test()
def slave_hal_test_read(dut):

    dut.log.info("Cocotb I2C Slave HAL - read Test ")

    cmdValidEvent = Event()
    rspReadyEvent = Event()

    data2Read = [0x34, 0xAA]

    modelMaster = I2CMasterModelHAL(dut.clk, dut.io_i2c_scl_read, dut.io_i2c_sda_read, dut.io_i2c_scl_write, dut.io_i2c_sda_write, 50)

    clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    cocotb.fork(clockDomain.start())
    cocotb.fork(monitor_cmd_valid(dut, cmdValidEvent))
    cocotb.fork(monitor_rsp_ready(dut, rspReadyEvent))
    cocotb.fork(read_test(dut, modelMaster, cmdValidEvent,rspReadyEvent, data2Read))
    modelMaster.startMaster()

    yield Timer(3000000)

    dut.log.info("I2C Slave HAL - read Test done")


###############################################################################
# Test a sequence of read
#@cocotb.test()
def slave_hal_test_WriteRead(dut):

    dut.log.info("Cocotb I2C Slave HAL - read Test ")


    cmdValidEvent = Event()
    rspReadyEvent = Event()

    modelMaster = I2CMasterModelHAL(dut.clk, dut.io_i2c_scl_read, dut.io_i2c_sda_read, dut.io_i2c_scl_write, dut.io_i2c_sda_write, 50)

    clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    cocotb.fork(clockDomain.start())
    cocotb.fork(monitor_cmd_valid(dut, cmdValidEvent))
    cocotb.fork(monitor_rsp_ready(dut, rspReadyEvent))
    cocotb.fork(writeRead_test(dut, modelMaster, cmdValidEvent,rspReadyEvent))
    modelMaster.startMaster()

    yield Timer(3000000)

    dut.log.info("I2C Slave HAL - read Test done")


###############################################################################
# Test a sequence of write
@cocotb.test()
def slave_hal_test_write_freeze(dut):

    dut.log.info("Cocotb I2C Slave HAL - write Test ")


    cmdValidEvent = Event()
    rspReadyEvent = Event()

    data2Write = [0x85]

    modelMaster = I2CMasterModelHAL(dut.clk, dut.io_i2c_scl_read, dut.io_i2c_sda_read, dut.io_i2c_scl_write, dut.io_i2c_sda_write, 50)

    clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    cocotb.fork(clockDomain.start())
    cocotb.fork(monitor_cmd_valid(dut, cmdValidEvent))
    cocotb.fork(monitor_rsp_ready(dut, rspReadyEvent))
    cocotb.fork(write_test(dut, modelMaster, cmdValidEvent,rspReadyEvent, data2Write, True))
    modelMaster.startMaster()

    yield Timer(3000000)

    dut.log.info("I2C Slave HAL - write Test done")


###############################################################################
# Test a sequence of read
#@cocotb.test()
def slave_hal_test_read_Freeze(dut):

    dut.log.info("Cocotb I2C Slave HAL - read Test ")

    cmdValidEvent = Event()
    rspReadyEvent = Event()

    data2Read = [0x34]

    modelMaster = I2CMasterModelHAL(dut.clk, dut.io_i2c_scl_read, dut.io_i2c_sda_read, dut.io_i2c_scl_write, dut.io_i2c_sda_write, 50)

    clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

    cocotb.fork(clockDomain.start())
    cocotb.fork(monitor_cmd_valid(dut, cmdValidEvent))
    cocotb.fork(monitor_rsp_ready(dut, rspReadyEvent))
    cocotb.fork(read_test(dut, modelMaster, cmdValidEvent,rspReadyEvent, data2Read, True))
    modelMaster.startMaster()

    yield Timer(3000000)

    dut.log.info("I2C Slave HAL - read Test done")
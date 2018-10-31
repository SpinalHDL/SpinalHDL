import random

import cocotb
from cocotb import fork, log
from cocotb.decorators import coroutine
from cocotb.triggers import RisingEdge, FallingEdge, Event, Timer

from cocotblib.Flow import Flow
from cocotblib.Stream import Stream
from cocotblib.misc import assertEquals, randInt, ClockDomainAsyncReset, simulationSpeedPrinter, clockedWaitTrue, Bundle, SimulationTimeout
from spinal.I2CTester2.lib.misc import OpenDrainInterconnect, I2cSoftMaster

@coroutine
def i2cMasterThread(master, cmds, rsps):
    for cmd in cmds:
        if cmd == "start":
            yield master.sendStart()
        elif cmd == "restart":
            yield master.sendRestart()
        elif cmd == "drop":
            yield master.sendDrop()
        elif cmd == "stop":
            yield master.sendStop()
        elif isinstance(cmd,bool):
            ret = [42]
            yield master.sendBit(cmd,ret)
            assert ret[0] == rsps.pop(0)
        elif isinstance(cmd,int):
            yield master.wait(cmd)
        else:
            raise Exception("???")


NONE = 0
START  = 1
RESTART = 2
STOP = 3
DROP  = 4
DRIVE = 5
READ = 6

@coroutine
def i2cSlaveThread(cmdBus, rspBus, cmds, rsps, clk):
    rspBus.valid <= False
    while cmds:
        yield FallingEdge(clk)

        rspBus.valid <= False
        if str(cmdBus.kind) == "xxx" or cmdBus.kind == NONE:
            continue

        expected = cmds.pop(0)

        # log.debug(expected)
        if expected == "start":
            assert cmdBus.kind == START
        elif expected == "restart":
            assert cmdBus.kind == RESTART
        elif expected == "stop":
            assert cmdBus.kind == STOP
        elif expected == "drop":
            assert cmdBus.kind == DROP
        elif expected == "drive":
            assert cmdBus.kind == DRIVE
            if random.uniform(0, 1) < 1.0 / (2500000/100000) * 2:
                rsp = rsps.pop(0)
                if rsp == "Z":
                    rspBus.valid <= True
                    rspBus.enable <= False
                    rspBus.data <= randInt(0, 1)
                elif isinstance(rsp, bool):
                    rspBus.valid <= True
                    rspBus.enable <= True
                    rspBus.data <= rsp
                else:
                    raise Exception(rsp)
            else:
               cmds.insert(0,expected)
        elif isinstance(expected, bool):
            assert cmdBus.kind == READ
            assert cmdBus.data == expected
        else:
            raise Exception("???")

        # log.info(str(expected) + "  " + str(cmds) + "  " + str(rsps))


@cocotb.test()
def test1(dut):
    # random.seed(13)
    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset,100000))
    cocotb.fork(simulationSpeedPrinter(dut.clk))


    sclInterconnect = OpenDrainInterconnect()
    sclInterconnect.addHardDriver(dut.io_i2c_scl_write)
    sclInterconnect.addHardReader(dut.io_i2c_scl_read)

    sdaInterconnect = OpenDrainInterconnect()
    sdaInterconnect.addHardDriver(dut.io_i2c_sda_write)
    sdaInterconnect.addHardReader(dut.io_i2c_sda_read)

    dut.io_config_samplingClockDivider <= 3
    dut.io_config_timeout <= 25*10-1
    dut.io_config_tsuData <= 4

    softMaster = I2cSoftMaster(sclInterconnect.newSoftConnection(), sdaInterconnect.newSoftConnection(), 2500000,dut.clk)


    masterCmds = []
    slaveCmds = []
    slaveRsps = []
    masterRsps = []

    masterCmds.append(10)
    for frameId in range(50):
        masterCmds.append("start")
        slaveCmds.append("start")
        while True:
            for bitId in range(randInt(1,10)):
                masterValue = random.uniform(0,1) > 0.5
                slaveValue = random.uniform(0,1) > 0.5
                slaveEnable = random.uniform(0,1) > 0.5
                value = masterValue and (slaveValue or not slaveEnable)
                masterCmds.append(masterValue)
                slaveRsps.append(slaveValue if slaveEnable else "Z")
                slaveCmds.append("drive")
                slaveCmds.append(value)
                masterRsps.append(value)

            if random.uniform(0,1) < 0.1:
                slaveCmds.append("drive")
                slaveRsps.append(random.uniform(0,1) > 0.5)
                masterCmds.append(20)
                masterCmds.append("drop")
                masterCmds.append(randInt(1,10))
                slaveCmds.append("drop")
                break

            slaveRsps.append("Z")
            if random.uniform(0,1) < 0.5:
                masterCmds.append("stop")
                slaveCmds.append("drive")
                slaveCmds.append(False)
                slaveCmds.append("stop")
                masterCmds.append(randInt(1,10))
                break

            masterCmds.append("restart")
            slaveCmds.append("drive")
            slaveCmds.append(True)
            slaveCmds.append("restart")

    masterThread = fork(i2cMasterThread(softMaster, masterCmds,masterRsps))
    slaveThread = fork(i2cSlaveThread(Bundle(dut,"io_bus_cmd"), Bundle(dut,"io_bus_rsp"),slaveCmds,slaveRsps, dut.clk))

    yield masterThread.join()
    cocotb.fork(SimulationTimeout(100 * 2500000))
    while True:
        if not slaveCmds and not slaveRsps and not masterRsps:
            break
        yield Timer(10000)

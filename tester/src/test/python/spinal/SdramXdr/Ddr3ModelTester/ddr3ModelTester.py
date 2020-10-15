import random
import cocotb
from cocotb.triggers import Timer

from Verilog_VCD.Verilog_VCD import parse_vcd
from spinal.SdramXdr.common.VcdLib import *


@cocotb.test()
def test1(dut):
    random.seed(0)
    from cocotblib.misc import cocotbXHack
    cocotbXHack()

    forks = []
    def map(component, net, apply, delay = 0):
        forks.append(cocotb.fork(stim(wave, component, net, apply, delay)))


    wave = parse_vcd("../../../../../../../simWorkspace/SdramXdrCtrlPlusRtlPhy/test.vcd")
    phy = "TOP.SdramXdrCtrlPlusRtlPhy"
    top = "TOP"

    yield Timer(0)
    phaseCount = getLastValue(wave, top, "phaseCount")
    dataRate = 2
    phaseDelay = 0
    clockPeriod = getClockPeriod(wave, top, "clk")

    cocotb.fork(genClock(dut.ck, dut.ck_n, clockPeriod//phaseCount))

    list(map(top, "ADDR", lambda v : dut.addr <= v))
    list(map(top, "BA", lambda v : dut.ba <= v))
    list(map(top, "CASn", lambda v : dut.cas_n <= v))
    list(map(top, "CKE", lambda v : dut.cke <= v))
    list(map(top, "CSn", lambda v : dut.cs_n <= v))
    list(map(top, "RASn", lambda v : dut.ras_n <= v))
    list(map(top, "WEn", lambda v : dut.we_n <= v))
    list(map(top, "RESETn", lambda v : dut.rst_n <= v))
    list(map(top, "ODT", lambda v : dut.odt <= v))

    cocotb.fork(stimPulse(wave, top, "writeEnable", lambda v : cocotb.fork(genDqs(dut.dqs, dut.dqs_n, 1+v/clockPeriod*phaseCount*dataRate//2, clockPeriod//(phaseCount*dataRate)*(phaseCount*dataRate-1), clockPeriod//phaseCount))))

    for fork in forks:
        yield fork

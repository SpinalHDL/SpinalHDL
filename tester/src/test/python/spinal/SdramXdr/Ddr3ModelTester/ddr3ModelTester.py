import random
import cocotb
from cocotb.triggers import Timer

from Verilog_VCD.Verilog_VCD import parse_vcd


@cocotb.coroutine
def stim(wave, componentName, netName, apply, delay = 0):
    yield Timer(delay)
    for netinfo in wave.values():
        for net in netinfo['nets']:
            if net["hier"] == componentName:
                if net["name"].startswith(netName):
                    time = 0
                    for t, v in netinfo['tv']:
                        yield Timer(t-time)
                        apply(int(v, 2))
                        time = t

@cocotb.coroutine
def stimPulse(wave, componentName, netName, apply):
    for netinfo in wave.values():
        for net in netinfo['nets']:
            if net["hier"] == componentName:
                if net["name"].startswith(netName):
                    time = 0
                    previousV = 0
                    previousT = 0
                    for t, v in netinfo['tv']:
                        current = int(v, 2)
                        if current == 0 and previousV == 1:
                            yield Timer(previousT - time)
                            apply(t - previousT)
                            time = previousT
                        previousT = t
                        previousV = current

def getClockPeriod(wave, componentName, netName):
    for netinfo in wave.values():
        for net in netinfo['nets']:
            if net["hier"] == componentName:
                if net["name"].startswith(netName):
                    return netinfo['tv'][102][0]-netinfo['tv'][100][0]

def countSignal(wave, componentName, prefix, postfix):
    count = 0
    for netinfo in wave.values():
        for net in netinfo['nets']:
            if net["hier"] == componentName:
                if net["name"].startswith(prefix) and net["name"].endswith(postfix):
                    count += 1
    return count


@cocotb.coroutine
def genClock(clkp, clkn, period, delay = 0):
    yield Timer(delay)
    while True:
        clkp <= 0
        clkn <= 1
        yield Timer(period // 2)
        clkp <= 1
        clkn <= 0
        yield Timer(period // 2)

@cocotb.coroutine
def genDqs(dqs_p, dqs_n, cycles, delay, period):
    # print("{} {} {} {} {}".format(dqs_p, dqs_n, cycles, delay, period))
    yield Timer(delay)
    count = len(dqs_p)
    mask = (1 << count)-1
    for i in range(cycles):
        dqs_p <= mask
        dqs_n <= 0
        yield Timer(period//2);
        dqs_p <= 0
        dqs_n <= mask
        yield Timer(period//2);

    # dqs_p <= ("z"*count)
    # dqs_n <= ("z"*count)




@cocotb.test()
def test1(dut):
    random.seed(0)
    from cocotblib.misc import cocotbXHack
    cocotbXHack()

    forks = []
    def map(component, net, apply, delay = 0):
        forks.append(cocotb.fork(stim(wave, component, net, apply, delay)))


    wave = parse_vcd("/home/miaou/pro/riscv/SaxonSoc.git/simWorkspace/SdrXdrCtrlPlusRtlPhy/test.vcd")
    phy = "TOP.SdrXdrCtrlPlusRtlPhy"
    top = "TOP"
    phaseCount = countSignal(wave, phy, "ctrl_io_phy_phases_", "_CASn")
    dataRate = 2
    phaseDelay = 0
    clockPeriod = getClockPeriod(wave, phy, "clk")

    cocotb.fork(genClock(dut.ck, dut.ck_n, clockPeriod//phaseCount))

    map(phy, "ctrl_io_phy_ADDR", lambda v : dut.addr <= v)
    map(phy, "ctrl_io_phy_BA", lambda v : dut.ba <= v)
    map(top, "CASn", lambda v : dut.cas_n <= v)
    map(top, "CKE", lambda v : dut.cke <= v)
    map(top, "CSn", lambda v : dut.cs_n <= v)
    map(top, "RASn", lambda v : dut.ras_n <= v)
    map(top, "WEn", lambda v : dut.we_n <= v)
    map(top, "RESETn", lambda v : dut.rst_n <= v)
    map(top, "ODT", lambda v : dut.odt <= v)

    cocotb.fork(stimPulse(wave, phy, "ctrl_io_phy_writeEnable", lambda v : cocotb.fork(genDqs(dut.dqs, dut.dqs_n, 1+v/clockPeriod*phaseCount*dataRate//2, clockPeriod//(phaseCount*dataRate)*(phaseCount*dataRate-1), clockPeriod//phaseCount))))

    for fork in forks:
        yield fork

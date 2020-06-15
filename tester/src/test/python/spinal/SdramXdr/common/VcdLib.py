import random
import cocotb
from cocotb.triggers import Timer

from Verilog_VCD.Verilog_VCD import parse_vcd


@cocotb.coroutine
def stim(wave, componentName, netName, apply, delay = 0):
    yield Timer(delay)
    for netinfo in list(wave.values()):
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
    for netinfo in list(wave.values()):
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
    for netinfo in list(wave.values()):
        for net in netinfo['nets']:
            if net["hier"] == componentName:
                if net["name"].startswith(netName):
                    return netinfo['tv'][102][0]-netinfo['tv'][100][0]

def countSignal(wave, componentName, prefix, postfix):
    count = 0
    for netinfo in list(wave.values()):
        for net in netinfo['nets']:
            if net["hier"] == componentName:
                if net["name"].startswith(prefix) and net["name"].endswith(postfix):
                    count += 1
    return count

def getLastValue(wave, componentName, netName):
    for netinfo in list(wave.values()):
        for net in netinfo['nets']:
            if net["hier"] == componentName:
                if net["name"].startswith(netName):
                    return int(netinfo['tv'][0][1], 2)
    raise Exception("????")

@cocotb.coroutine
def genClock(clkp, clkn, period, delay = 0):
    yield Timer(delay)
    while True:
        clkp <= 0
        if clkn:
            clkn <= 1
        yield Timer(period // 2)
        clkp <= 1
        if clkn:
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
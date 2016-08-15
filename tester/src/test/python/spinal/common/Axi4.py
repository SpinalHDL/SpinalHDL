import random

import cocotb
from cocotb.triggers import RisingEdge

from spinal.common.Stream import Stream
from spinal.common.misc import BoolRandomizer, log2Up


class Axi4:
    def __init__(self,dut,name):
        self.ar = Stream(dut,name + "_ar")
        self.r  = Stream(dut, name + "_r")
        self.aw = Stream(dut, name + "_aw")
        self.w  = Stream(dut, name + "_w")
        self.b  = Stream(dut, name + "_b")

class Axi4ReadOnly:
    def __init__(self,dut,name):
        self.ar = Stream(dut,name + "_ar")
        self.r  = Stream(dut, name + "_r")

class Axi4WriteOnly:
    def __init__(self,dut,name):
        self.aw = Stream(dut, name + "_aw")
        self.w  = Stream(dut, name + "_w")
        self.b  = Stream(dut, name + "_b")

class Axi4Shared:
    def __init__(self,dut,name):
        self.arw = Stream(dut,name + "_arw")
        self.r  = Stream(dut, name + "_r")
        self.w  = Stream(dut, name + "_w")
        self.b  = Stream(dut, name + "_b")


def Axi4AddrIncr(address,burst,len,size):
    if burst == 0:
        return address
    if burst == 1:
        return address + (1 << size)
    if burst == 2:
        burstSize = (1 << size) * (len+1)
        burstMask = burstSize-1
        base = (address + (1 << size)) & burstMask
        return (address & ~burstMask) | base
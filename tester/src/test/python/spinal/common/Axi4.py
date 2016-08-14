import random

import cocotb
from cocotb.triggers import RisingEdge

from spinal.common.Stream import Stream
from spinal.common.misc import BoolRandomizer


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
        self.ar = Stream(dut,name + "_arw")
        self.r  = Stream(dut, name + "_r")
        self.w  = Stream(dut, name + "_w")
        self.b  = Stream(dut, name + "_b")


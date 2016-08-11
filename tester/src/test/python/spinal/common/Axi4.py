import random

import cocotb
from cocotb.triggers import RisingEdge

from spinal.common.Stream import Stream
from spinal.common.misc import BoolRandomizer


class Axi4:
    def __init__(self,dut,name):
        if dut.__hasattr__(name + "_ar_valid"):
            self.ar = Stream(dut,name + "_ar")
        if dut.__hasattr__(name + "_r_valid"):
            self.r  = Stream(dut, name + "_r")
        if dut.__hasattr__(name + "_aw_valid"):
            self.aw = Stream(dut, name + "_aw")
        if dut.__hasattr__(name + "_w_valid"):
            self.w  = Stream(dut, name + "_w")
        if dut.__hasattr__(name + "_b_valid"):
            self.b  = Stream(dut, name + "_b")



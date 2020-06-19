import operator
from cocotb.regression import TestFactory
from spinal.RiscvTester.RiscvTester import  isaTestsMemory, isaTestsMulDiv, isaTestsBase, testIsa

from cocotblib.misc import cocotbXHack
from functools import reduce
cocotbXHack()

factory = TestFactory(testIsa)
factory.add_option("iHexPath",  ["../" + x for x in reduce(operator.add, [["../Pinsec/hex/dhrystone.hex"]])])
factory.generate_tests()

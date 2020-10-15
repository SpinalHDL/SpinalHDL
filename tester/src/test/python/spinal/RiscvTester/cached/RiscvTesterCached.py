import operator
from cocotb.regression import TestFactory
from spinal.RiscvTester.RiscvTester import  isaTestsMemory, isaTestsMulDiv, isaTestsBase, testIsa

from cocotblib.misc import cocotbXHack
from functools import reduce
cocotbXHack()

factory = TestFactory(testIsa)
factory.add_option("iHexPath",  reduce(operator.add, [isaTestsBase, isaTestsMemory, isaTestsMulDiv]))
factory.generate_tests()

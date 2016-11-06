import operator
from cocotb.regression import TestFactory
from spinal.RiscvTester.RiscvTester import  isaTestsMemory, isaTestsMulDiv, isaTestsBase, testIsa

from cocotblib.misc import cocotbXHack
cocotbXHack()

factory = TestFactory(testIsa)
factory.add_option("iHexPath",  map(lambda x : "../" + x,reduce(operator.add, [["../Pinsec/hex/dhrystone.hex"]])))
factory.generate_tests()

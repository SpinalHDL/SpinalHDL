import operator
from cocotb.regression import TestFactory
from spinal.RiscvTester.RiscvTester import  isaTestsMemory, isaTestsMulDiv, isaTestsBase, testIsa


factory = TestFactory(testIsa)
factory.add_option("iHexPath",  map(lambda x : "../" + x,reduce(operator.add, [isaTestsBase, isaTestsMemory, isaTestsMulDiv])))
factory.generate_tests()

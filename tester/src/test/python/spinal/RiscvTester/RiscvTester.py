import array

import cocotb
from cocotb.result import TestFailure, TestSuccess
from cocotb.triggers import Edge, RisingEdge

from cocotblib.misc import randSignal, ClockDomainAsyncReset, randBoolSignal, \
    simulationSpeedPrinter


def loadIHex(path,array):
    with open(path) as f:
        offset = 0
        for line in f:
            if len(line) > 0:
                assert line[0] == ':'
                byteCount = int(line[1:3],16)
                nextAddr  = int(line[3:7],16) + offset
                key = int(line[7:9],16)
                if key == 0:
                    for i in range(0,byteCount):
                        array[nextAddr + i] = int(line[9+i*2:11+i*2],16)
                elif key == 2:
                    offset = int(line[9:13],16) << 4
                else:
                    pass


class Tester:
    def __init__(self,dut):
        self.dut = dut
        self.ram = array.array('B', (0xFF for i in range(0, 1024 * 1024)))
        self.rom = array.array('B', (0xFF for i in range(0, 1024 * 1024)))
        self.allowRomWrite = False
        self.io_iCheck_valid = True
        self.wrapICmdZero = False


    @cocotb.coroutine
    def driveICmdReady(self):
        dut = self.dut
        dut.io_i_cmd_ready <= 1
        while True:
            yield [Edge(dut.io_i_rsp_valid),Edge(dut.io_i_rsp_ready)]
            dut.io_i_cmd_ready <= (int(dut.io_i_rsp_valid) == 0 or int(dut.io_i_rsp_ready) == 1);

    @cocotb.coroutine
    def driveDCmdReady(self):
        dut = self.dut
        dut.io_d_cmd_ready <= 1
        while True:
            yield [Edge(dut.io_d_rsp_valid),Edge(dut.io_d_rsp_ready)]
            dut.io_d_cmd_ready <= (int(dut.io_d_rsp_valid) == 0 or int(dut.io_d_rsp_ready) == 1);

    @cocotb.coroutine
    def driveMisc(self):
        self.dut.io_iCmdDrive <= 1
        self.dut.io_iRspDrive <= 1
        self.dut.io_dCmdDrive <= 1
        self.dut.io_dRspDrive <= 1
        self.dut.io_doCacheFlush <= 0
        cocotb.fork(self.driveICmdReady())
        cocotb.fork(self.driveDCmdReady())
        while True:
            yield RisingEdge(self.dut.clk)
            randBoolSignal(self.dut.io_iCmdDrive,0.7)
            randBoolSignal(self.dut.io_iRspDrive,0.7)
            randBoolSignal(self.dut.io_dCmdDrive,0.7)
            # randBoolSignal(self.dut.io_dRspDrive,0.7)
            randBoolSignal(self.dut.io_doCacheFlush,0.0003)

    @cocotb.coroutine
    def driveIRsp(self):
        dut = self.dut
        dut.io_i_rsp_valid <= 0
        while True:
            yield RisingEdge(dut.clk)
            if int(dut.io_i_rsp_ready) == 1:
              dut.io_i_rsp_valid <= 0
              randSignal(dut.io_i_rsp_payload_instruction)

            if int(dut.io_i_cmd_valid) == 1 and int(dut.io_i_cmd_ready) == 1:
              dut.io_i_rsp_valid <= 1
              dut.io_i_rsp_payload_pc <= int(dut.io_i_cmd_payload_pc)
              data = 0
              if int(dut.io_i_cmd_payload_pc) <= 0x03FFFFFF :
                for i in range(0,4):
                  data = data | (self.rom[int(dut.io_i_cmd_payload_pc) + i] << (i*8))

              elif int(dut.io_i_cmd_payload_pc) <= 0x04007FFF :
                for i in range(0, 4):
                  data = data | (self.ram[int(dut.io_i_cmd_payload_pc and 0x00007FFF) + i] << (i*8))
              else:
                raise TestFailure("out of range ICmd read")

              if self.wrapICmdZero and int(dut.io_i_cmd_payload_pc) == 0:
                dut.io_i_rsp_payload_instruction <= 0xffc02e23; #01c02023   ffc02e23
              elif data == 0x00000073:
                dut.io_i_rsp_payload_instruction <= 0xffc02e23;
              elif data == 0x0FF0000F:
                dut.io_i_rsp_payload_instruction <= 0x00000013; #TODO remove me
              else:
                dut.io_i_rsp_payload_instruction <= data;

    @cocotb.coroutine
    def driveDRsp(self):
        logg = open('log.txt', 'wb')
        dut = self.dut
        dut.io_d_rsp_valid <= 0
        counter = 0
        while True:
            yield RisingEdge(dut.clk)
            counter = counter + 1
            if int(dut.io_d_rsp_ready) == 1 :
              dut.io_d_rsp_valid <= 0
              randSignal(dut.io_d_rsp_payload)

            if int(dut.io_d_cmd_valid) == 1 and  int(dut.io_d_cmd_ready) == 1 :
              if int(dut.io_d_cmd_payload_wr) == 1 :
                if int(dut.io_d_cmd_payload_address) == 0xF0000000 :
                  logg.write(str(chr(int(dut.io_d_cmd_payload_data) & 0xFF)))
                elif int(dut.io_d_cmd_payload_address) == 0xF0000004 :
                  pass
                elif int(dut.io_d_cmd_payload_address) == 0xFFFFFFF8 :
                  pass
                elif int(dut.io_d_cmd_payload_address) == 0xF0000010 :
                    pass
                elif int(dut.io_d_cmd_payload_address) == 0xF0000044 :
                    pass
                elif int(dut.io_d_cmd_payload_address) == 0xFFFFFFFC :
                    pass

                elif int(dut.io_d_cmd_payload_address) <= 0x03FFFFFF :
                  if not self.allowRomWrite:
                    raise TestFailure("Rom was written :(")
                  for i in range(0,1 << int(dut.io_d_cmd_payload_size)):
                    self.rom[int(dut.io_d_cmd_payload_address) + i] = (int(dut.io_d_cmd_payload_data) >> (i*8)) & 0xFF
                elif int(dut.io_d_cmd_payload_address) <= 0x04007FFF :
                  # print("write %x %x" % (int(dut.io_d_cmd_payload_address),int(dut.io_d_cmd_payload_data)))
                  for i in range(0,1 << int(dut.io_d_cmd_payload_size)):
                      self.ram[(int(dut.io_d_cmd_payload_address) & 0x00007FFF) + i] = (int(dut.io_d_cmd_payload_data) >> (i*8)) & 0xFF

                else:
                    raise TestFailure("dCmd out of range %x" %(int(dut.io_d_cmd_payload_address)))

              else:
                dut.io_d_rsp_valid <= 1
                if int(dut.io_d_cmd_payload_address) == 0xF0000040 :
                  dut.io_d_rsp_payload <= counter
                elif int(dut.io_d_cmd_payload_address) == 0xF0000020 :
                  dut.io_d_rsp_payload <= 0
                elif int(dut.io_d_cmd_payload_address) == 0xF0000000 :
                  dut.io_d_rsp_payload <= 0
                elif int(dut.io_d_cmd_payload_address) == 0xF0000004 :
                  dut.io_d_rsp_payload <= 0xFFFF0000
                elif int(dut.io_d_cmd_payload_address) <= 0x03FFFFFF :
                    data = 0
                    for i in range(0,4):
                        data |= self.rom[int(dut.io_d_cmd_payload_address) + i] << (i*8)
                    dut.io_d_rsp_payload <= data
                elif int(dut.io_d_cmd_payload_address) <= 0x04007FFF :
                    data = 0
                    for i in range(0, 4):
                        data |= self.ram[(int(dut.io_d_cmd_payload_address) & 0x00007FFF) + i]  << (i*8)
                    # print("read %x %x" % (int(dut.io_d_cmd_payload_address), int(dut.io_d_cmd_payload_data)))
                    dut.io_d_rsp_payload <= data
                else:
                    raise TestFailure("dCmd out of range %x" %(int(dut.io_d_cmd_payload_address)))




            if int(dut.io_iCheck_valid) == 1 :
              if (int(dut.io_iCheck_payload_address) & 3) != 0:
                raise TestFailure("iCmd bad allignement")
              if int(dut.io_iCheck_payload_data) != 0x00000013 and int(dut.io_iCheck_payload_data) != 0x01c02023 and int(dut.io_iCheck_payload_data) != 0xffc02e23 :
                for i in range(0,4):
                  if self.rom[int(dut.io_iCheck_payload_address)+i] != ((int(dut.io_iCheck_payload_data) >> (i*8)) & 0xFF):
                      raise TestFailure("wrong instruction read")



    @cocotb.coroutine
    def do(self,iHexPath):
        loadIHex(iHexPath, self.rom)
        cocotb.fork(simulationSpeedPrinter(self.dut.clk))
        cocotb.fork(ClockDomainAsyncReset(self.dut.clk, self.dut.reset))
        cocotb.fork(self.driveMisc())
        cocotb.fork(self.driveIRsp())
        cocotb.fork(self.driveDRsp())
        while True:
            yield RisingEdge(self.dut.clk)
            if int(self.dut.io_d_cmd_valid) == 1 and int(self.dut.io_d_cmd_payload_address) == 0xFFFFFFFC:
                if int(self.dut.io_d_cmd_payload_data) != 0x00000001:
                    print("ERROR")
                    raise TestFailure("RISCV test %s fail with %d" % (iHexPath,int(self.dut.io_d_cmd_payload_data)))
                else:
                    raise TestSuccess("RISCV test %s pass with %d" % (iHexPath, int(self.dut.io_d_cmd_payload_data)))

@cocotb.coroutine
def testIsa(dut,iHexPath=None):
    print(str(iHexPath))
    tester = Tester(dut)
    tester.wrapICmdZero = True
    tester.allowRomWrite = True
    yield tester.do(iHexPath)

import os
testLocation = os.path.dirname(os.path.realpath(__file__)) + "/tests/"

isaTestsBase =  [testLocation + "rv32ui-pt-add.hex",
                 testLocation + "rv32ui-pt-addi.hex",
                 testLocation + "rv32ui-pt-and.hex",
                 testLocation + "rv32ui-pt-andi.hex",
                 testLocation + "rv32ui-pt-auipc.hex",
                 testLocation + "rv32ui-pt-beq.hex",
                 testLocation + "rv32ui-pt-bge.hex",
                 testLocation + "rv32ui-pt-bgeu.hex",
                 testLocation + "rv32ui-pt-blt.hex",
                 testLocation + "rv32ui-pt-bltu.hex",
                 testLocation + "rv32ui-pt-bne.hex",
                 testLocation + "rv32ui-pt-j.hex",
                 testLocation + "rv32ui-pt-jal.hex",
                 testLocation + "rv32ui-pt-jalr.hex",
                 testLocation + "rv32ui-pt-or.hex",
                 testLocation + "rv32ui-pt-ori.hex",
                 testLocation + "rv32ui-pt-simple.hex",
                 testLocation + "rv32ui-pt-sll.hex",
                 testLocation + "rv32ui-pt-slli.hex",
                 testLocation + "rv32ui-pt-slt.hex",
                 testLocation + "rv32ui-pt-slti.hex",
                 testLocation + "rv32ui-pt-sra.hex",
                 testLocation + "rv32ui-pt-srai.hex",
                 testLocation + "rv32ui-pt-srl.hex",
                 testLocation + "rv32ui-pt-srli.hex",
                 testLocation + "rv32ui-pt-sub.hex",
                 testLocation + "rv32ui-pt-xor.hex",
                 testLocation + "rv32ui-pt-xori.hex"]


isaTestsMemory = [testLocation + "rv32ui-pt-lb.hex",
                  testLocation + "rv32ui-pt-lbu.hex",
                  testLocation + "rv32ui-pt-lh.hex",
                  testLocation + "rv32ui-pt-lhu.hex",
                  testLocation + "rv32ui-pt-lui.hex",
                  testLocation + "rv32ui-pt-lw.hex",
                  testLocation + "rv32ui-pt-sb.hex",
                  testLocation + "rv32ui-pt-sh.hex",
                  testLocation + "rv32ui-pt-sw.hex"]


isaTestsMulDiv = [testLocation + "rv32ui-pt-mul.hex",
                  testLocation + "rv32ui-pt-mulh.hex",
                  testLocation + "rv32ui-pt-mulhsu.hex",
                  testLocation + "rv32ui-pt-mulhu.hex",
                  testLocation + "rv32ui-pt-div.hex",
                  testLocation + "rv32ui-pt-divu.hex",
                  testLocation + "rv32ui-pt-rem.hex",
                  testLocation + "rv32ui-pt-remu.hex"]

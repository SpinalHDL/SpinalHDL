###############################################################################
# Test for the I2C  HAL
#
###############################################################################

import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge, Event

from spinal.common.misc import assertEquals, simulationSpeedPrinter
from spinal.common.ClockDomain import ClockDomain, RESET_ACTIVE_LEVEL








class SimData:
    pass

class Nothing(SimData):
    def __repr__(self):
        return "Nothing"

class SimCMD(SimData):

    def __init__(self, cmd, data=0):
        self.mode = cmd
        self.data = data

    def __repr__(self):
        return "SimCMD %d" % (self.mode)

class SimRsp(SimData):

    def __init__(self, rsp, data=0):
        self.mode = rsp
        self.data = data

    def __repr__(self):
        return "SimRSP %d" % (self.mode)








@cocotb.coroutine
def masterManager(masterHelp, seqMaster):

    io = masterHelp.io

    io.init()

    yield RisingEdge(io.clk)

    for (simCMD, simRSP, delay) in seqMaster:

        # Execute the command
        if not (isinstance(simCMD, Nothing)):

            yield Timer(delay)

            yield RisingEdge(io.clk)

            io.cmd_valid <= 1
            io.cmd_mode  <= simCMD.mode
            io.cmd_data  <= simCMD.data

            yield masterHelp.event_cmd_ready.wait()

            io.cmd_valid <= 0

        if isinstance(simRSP, Nothing):
            yield RisingEdge(io.clk)
        else:

            yield masterHelp.event_rsp_valid.wait()

            # check response
        #    assertEquals(simRSP.mode, int(io.rsp_mode), "Master mode response wrong")
        #    if simRSP.mode == Master.RSP.DATA:
        #        assertEquals(simRSP.data, int(io.rsp_data), "Master data response wrong")





@cocotb.coroutine
def slaveManager(slaveHelper, seqSlave):

    io = slaveHelper.io

    io.init()

    yield RisingEdge(io.clk)

    for (simCMD, simRSP, delay) in seqSlave:

        if not (isinstance(simCMD, Nothing)):

            yield slaveHelper.event_cmd_valid.wait()


            # check command
       #     assertEquals(simCMD.mode, int(io.cmd_mode), "Slave mode command wrong")
       #     if simCMD.mode == Slave.CMD.DATA:
       #         assertEquals(simCMD.data, int(io.cmd_data), "Slave data command wrong")


            yield Timer(delay)

            yield RisingEdge(io.clk)
            io.cmd_ready <= 1
            yield RisingEdge(io.clk)
            io.cmd_ready <= 0

        if not (isinstance(simRSP, Nothing)):
            io.rsp_valid <= 1
            io.rsp_mode  <= simRSP.mode
            io.rsp_data  <= simRSP.data

            yield slaveHelper.event_rsp_ready.wait()

            io.rsp_valid <= 0






class ScenarioAction():
    def __init__(self,mdelay=0):
        self.mDelay = mdelay

class sSTART(ScenarioAction):
    pass

class sWRITE(ScenarioAction):
    def __init__(self, data, mdelay=0):
        self.data = data
        self.mDelay = mdelay

class sREAD(ScenarioAction):
    def __init__(self, data, mdelay=0):
        self.data = data
        self.mDelay = mdelay

class sACK(ScenarioAction):
    pass

class sNACK(ScenarioAction):
    pass

class sSTOP(ScenarioAction):
    pass


def genScenario(listAction):

    masterSeq = list()
    slaveSeq  = list()

    for index in range(0,len(listAction)):

        action = listAction[index]

        if isinstance(action, sSTART):
            masterSeq.append( ( SimCMD(Master.CMD.START) , Nothing(), action.mDelay ) )

            nextAction = listAction[index+1]
            if isinstance(nextAction, sREAD):
                slaveSeq.append(( SimCMD(Slave.CMD.START) , SimRsp(Slave.RSP.DATA, nextAction.data) , 0 ))
            elif isinstance(nextAction, sWRITE):
                slaveSeq.append(  ( SimCMD(Slave.CMD.START) , SimRsp(Slave.RSP.NONE) , 0 ) )

        elif isinstance(action, sWRITE):
            masterSeq.append( ( SimCMD(Master.CMD.WRITE, action.data) , SimRsp(Master.RSP.DATA, action.data)    , action.mDelay ))
            nextAction = listAction[index+1]
            if isinstance(nextAction, sACK):
                slaveSeq.append(  ( SimCMD(Slave.CMD.DATA, action.data) , SimRsp(Slave.RSP.ACK)     , 0 ))
            elif isinstance(nextAction, sNACK):
                slaveSeq.append(  ( SimCMD(Slave.CMD.DATA, action.data) , SimRsp(Slave.RSP.NONE)     , 0 ))
            else:
                return -1

        elif isinstance(action, sACK):

            prevAction = listAction[index-1]
            if isinstance(prevAction, sWRITE):
                masterSeq.append(( Nothing() , SimRsp(Master.RSP.ACK), action.mDelay ))
            elif isinstance(prevAction, sREAD):
                masterSeq.append(( SimCMD(Master.CMD.ACK )       , Nothing()    , action.mDelay ))
            else:
                return -1

            nextAction = listAction[index+1]
            if isinstance(nextAction, sWRITE):

                slaveSeq.append( ( SimCMD(Slave.CMD.ACK), SimRsp(Slave.RSP.NONE), 0 ) )

            elif isinstance(nextAction, sREAD):

                slaveSeq.append(( SimCMD(Slave.CMD.ACK )       , SimRsp(Slave.RSP.DATA, nextAction.data)      , 0 ))

            elif isinstance(nextAction, sSTOP):
                slaveSeq.append( ( SimCMD(Slave.CMD.ACK), SimRsp(Slave.RSP.NONE), 0 ) )
            else:
                return -1

        elif isinstance(action, sNACK):
            prevAction = listAction[index-1]
            if isinstance(prevAction, sWRITE):
                masterSeq.append(( Nothing() , SimRsp(Master.RSP.NACK), action.mDelay ))
            elif isinstance(prevAction, sREAD):
                masterSeq.append(( SimCMD(Master.CMD.NACK )       , Nothing()    , action.mDelay ))
            else:
                return -1

            nextAction = listAction[index+1]
            if isinstance(nextAction, sWRITE):

                slaveSeq.append( ( SimCMD(Slave.CMD.NACK), SimRsp(Slave.RSP.NONE), 0 ) )

            elif isinstance(nextAction, sREAD):

                slaveSeq.append(( SimCMD(Slave.CMD.NACK )       , SimRsp(Slave.RSP.DATA, nextAction.data)      , 0 ))

            elif isinstance(nextAction, sSTOP):
                slaveSeq.append( ( SimCMD(Slave.CMD.NACK), SimRsp(Slave.RSP.NONE), 0 ) )
            else:
                return -1


        elif isinstance(action, sREAD):
            masterSeq.append(( SimCMD(Master.CMD.READ )      , SimRsp(Master.RSP.DATA, action.data)      , action.mDelay ))
            slaveSeq.append(( SimCMD(Slave.CMD.DATA, action.data) , SimRsp(Slave.RSP.NONE)            , 0 ))

        elif isinstance(action, sSTOP):
            masterSeq.append(( SimCMD(Master.CMD.STOP)       , Nothing()                       , action.mDelay ))
            slaveSeq.append(( SimCMD(Slave.CMD.STOP)       , SimRsp(Slave.RSP.NONE)            , 0 ))


    return (masterSeq, slaveSeq)






###############################################################################
# Test a sequence of read
@cocotb.test()
def test_scenario_1(dut):

    dut.log.info("Cocotb I2C HAL - Scenario 1")

    masterHelper = Master(dut)
    slaveHelper  = Slave(dut)


    seqMasterList = list()
    seqSlaveList  = list()


    # Scenario simple write
    (mSeq, sSeq) = genScenario([sSTART(), sWRITE(0x33), sACK(), sSTOP() ])
    seqMasterList.append(mSeq)
    seqSlaveList.append(sSeq)

    # Scenario double Write
    (mSeq, sSeq) = genScenario([sSTART(), sWRITE(0x33), sACK(), sWRITE(0x88), sNACK(), sSTOP() ])
    seqMasterList.append(mSeq)
    seqSlaveList.append(sSeq)

    # Scenario simple read
    (mSeq, sSeq) = genScenario([sSTART(), sREAD(0x81), sNACK(),  sSTOP() ])
    seqMasterList.append(mSeq)
    seqSlaveList.append(sSeq)

    # Scenario double read
    (mSeq, sSeq) = genScenario([sSTART(), sREAD(0x81), sACK(), sREAD(11), sNACK(),  sSTOP() ])
    seqMasterList.append(mSeq)
    seqSlaveList.append(sSeq)

    (mSeq, sSeq) = genScenario([sSTART(), sWRITE(0x81), sNACK(), sREAD(0x11), sNACK(),  sSTOP() ])
    seqMasterList.append(mSeq)
    seqSlaveList.append(sSeq)

    (mSeq, sSeq) = genScenario([sSTART(), sREAD(0x81), sACK(), sWRITE(0x11), sACK(),  sSTOP() ])
    seqMasterList.append(mSeq)
    seqSlaveList.append(sSeq)





    # Senario : One Write
    # ------------------------------------------------------------------------------------------
    seqMasterList.append( [( SimCMD(Master.CMD.START)      , Nothing()                         , 0 ),
                           ( SimCMD(Master.CMD.WRITE, 0x70) , SimRsp(Master.RSP.DATA, 0x70)    , 0 ),
                           ( Nothing()                      , SimRsp(Master.RSP.ACK)           , 0 ),
                           ( SimCMD(Master.CMD.STOP)        , Nothing()                        , 0 )])

    seqSlaveList.append( [( SimCMD(Slave.CMD.START)      , SimRsp(Slave.RSP.NONE)    , 0 ),
                          ( SimCMD(Slave.CMD.DATA, 0x70) , SimRsp(Slave.RSP.ACK)     , 300000 ),
                          ( SimCMD(Slave.CMD.ACK)        , SimRsp(Slave.RSP.NONE)    , 0 ),
                          ( SimCMD(Slave.CMD.STOP)       , SimRsp(Slave.RSP.NONE)    , 0 )])



    # # Senario : Simple Read
    # # ------------------------------------------------------------------------------------------
    # seqMasterList.append( [( SimCMD(Master.CMD.START)     , Nothing()                           , 0 ),
    #                        ( SimCMD(Master.CMD.READ )      , SimRsp(Master.RSP.DATA, 0x81)      , 0 ),
    #                        ( SimCMD(Master.CMD.ACK )       , Nothing()                          , 0 ),
    #                        ( SimCMD(Master.CMD.STOP)       , Nothing()                          , 0 )])
    #
    #
    # seqSlaveList.append( [( SimCMD(Slave.CMD.START)      , SimRsp(Slave.RSP.DATA, 0x81)       , 0 ),
    #                       ( SimCMD(Slave.CMD.DATA, 0x81) , SimRsp(Slave.RSP.NONE)             , 0 ),
    #                       ( SimCMD(Slave.CMD.ACK )       , SimRsp(Slave.RSP.NONE)            , 0 ),
    #                       ( SimCMD(Slave.CMD.STOP)       , SimRsp(Slave.RSP.NONE)            , 0 )])
    #
    #
    # # Senario : read - write
    # # ------------------------------------------------------------------------------------------
    # seqMasterList.append( [( SimCMD(Master.CMD.START)     , Nothing()                           , 0 ),
    #                        ( SimCMD(Master.CMD.READ )      , SimRsp(Master.RSP.DATA, 0x81)      , 0 ),
    #                        ( SimCMD(Master.CMD.ACK )       , Nothing()                          , 0 ),
    #                        ( SimCMD(Master.CMD.WRITE, 0x81) , SimRsp(Master.RSP.DATA, 0x81)    , 0 ),
    #                        ( Nothing()                      , SimRsp(Master.RSP.ACK)           , 0 ),
    #                        ( SimCMD(Master.CMD.STOP)       , Nothing()                          , 0 )])
    #
    #
    # seqSlaveList.append( [( SimCMD(Slave.CMD.START)      , SimRsp(Slave.RSP.DATA, 0x81)   , 0 ),
    #                       ( SimCMD(Slave.CMD.DATA, 0x81) , SimRsp(Slave.RSP.NONE)         , 0 ),
    #                       ( SimCMD(Slave.CMD.ACK )       , SimRsp(Slave.RSP.NONE)         , 0 ),
    #                       ( SimCMD(Slave.CMD.DATA, 0x81) , SimRsp(Slave.RSP.ACK)         , 0 ),
    #                       ( SimCMD(Slave.CMD.ACK)        , SimRsp(Slave.RSP.NONE)         , 0 ),
    #                       ( SimCMD(Slave.CMD.STOP)       , SimRsp(Slave.RSP.NONE)         , 0 )])
    #
    #
    # # Senario : write - read
    # # ------------------------------------------------------------------------------------------
    # seqMasterList.append( [( SimCMD(Master.CMD.START)      , Nothing()                         , 0 ),
    #                        ( SimCMD(Master.CMD.WRITE, 0x81) , SimRsp(Master.RSP.DATA, 0x81)    , 0 ),
    #                        ( Nothing()                      , SimRsp(Master.RSP.ACK)           , 0 ),
    #                        ( SimCMD(Master.CMD.READ )      , SimRsp(Master.RSP.DATA, 0x81)      , 0 ),
    #                        ( SimCMD(Master.CMD.ACK )       , Nothing()                          , 0 ),
    #                        ( SimCMD(Master.CMD.STOP)        , Nothing()                        , 0 )])
    #
    # seqSlaveList.append( [( SimCMD(Slave.CMD.START)      , SimRsp(Slave.RSP.NONE)       , 0 ),
    #                       ( SimCMD(Slave.CMD.DATA, 0x81) , SimRsp(Slave.RSP.ACK)        , 0 ),
    #                       ( SimCMD(Slave.CMD.ACK)        , SimRsp(Slave.RSP.DATA, 0x81) , 0 ),
    #                       ( SimCMD(Slave.CMD.DATA, 0x81) , SimRsp(Slave.RSP.NONE)       , 0 ),
    #                       ( SimCMD(Slave.CMD.ACK )       , SimRsp(Slave.RSP.NONE)       , 0 ),
    #                       ( SimCMD(Slave.CMD.STOP)       , SimRsp(Slave.RSP.NONE)       , 0 )])
    #
    #
    #


    # select the scenario
    index = 6
    masterSeq = seqMasterList[index]
    slaveSea  = seqSlaveList[index]

    if True:

        clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

        cocotb.fork(clockDomain.start())
        cocotb.fork(masterManager(masterHelper, masterSeq))
        cocotb.fork(slaveManager(slaveHelper, slaveSea))

        yield Timer(3000000)

    else:

        for (masterSequence, slaveSequence) in zip(seqMasterList, seqSlaveList):

            clockDomain = ClockDomain(dut.clk, 500, dut.resetn, RESET_ACTIVE_LEVEL.LOW)

            cocotb.fork(clockDomain.start())
            cocotb.fork(masterManager(masterHelper, masterSequence))
            cocotb.fork(slaveManager(slaveHelper, slaveSequence))

            yield Timer(3000000)

    dut.log.info("I2C HAL Test Done - Scenario 1")


import cocotb
from cocotb.result import TestFailure, TestError
from cocotb.triggers import Timer

PHASE_NULL = 0
PHASE_SIM = 100
PHASE_WAIT_TASKS_END = 200
PHASE_CHECK_SCORBOARDS = 300
PHASE_DONE = 400


class Infrastructure:
    def __init__(self,name,parent):
        self.name = name
        self.parent = parent
        if parent != None:
            parent.addChild(self)


    def startPhase(self, phase):
        pass

    def canPhaseProgress(self, phase):
        return True

    def endPhase(self, phase):
        pass


class InfrastructureWithChild(Infrastructure):
    def __init__(self,name,parent):
        Infrastructure.__init__(self,name,parent)
        self.children = []


    def startPhase(self, phase):
        error = False
        for child in self.children:
            child.startPhase(phase)

    def canPhaseProgress(self, phase):
        for child in self.children:
            if not child.canPhaseProgress(phase):
                return False
        return True

    def endPhase(self, phase):
        for child in self.children:
            child.endPhase(phase)

    def addChild(self,child):
        self.children.append(child)


class PhaseManager(InfrastructureWithChild):
    def __init__(self):
        InfrastructureWithChild.__init__(self,None,None)
        self.phase = PHASE_NULL
        # setSimManager(self)


    @cocotb.coroutine
    def waitChild(self):
        while True:
            if self.canPhaseProgress(self.phase):
                break
            yield Timer(10000)



    def switchPhase(self,phase):
        for infra in self.children:
            infra.endPhase(self.phase)
        self.phase = phase
        for infra in self.children:
            infra.startPhase(self.phase)

    @cocotb.coroutine
    def run(self):
        self.switchPhase(PHASE_SIM)
        yield self.waitChild()
        self.switchPhase(PHASE_WAIT_TASKS_END)
        yield self.waitChild()
        self.switchPhase(PHASE_CHECK_SCORBOARDS)
        self.switchPhase(PHASE_DONE)

# _simManager = None
#
# def getSimManager():
#     return _simManager
#
# def setSimManager(that):
#     global _simManager
#     _simManager = that
#




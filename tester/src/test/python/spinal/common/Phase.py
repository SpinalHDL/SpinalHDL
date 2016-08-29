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
        self.children = []

    def getPhase(self):
        return self.parent.getPhase()

    def startPhase(self, phase):
        error = False
        for child in self.children:
            child.startPhase(phase)

    def hasEnoughSim(self):
        return True

    def canPhaseProgress(self, phase):
        for child in self.children:
            if not child.canPhaseProgress(phase):
                return False
        if phase == PHASE_SIM:
            return self.hasEnoughSim()
        return True

    def endPhase(self, phase):
        for child in self.children:
            child.endPhase(phase)

    def addChild(self,child):
        if child not in self.children:
            self.children.append(child)

    def getPath(self):
        if self.parent != None:
            return self.parent.getPath() + "/" + self.name
        else:
            return self.name


class PhaseManager(Infrastructure):
    def __init__(self):
        Infrastructure.__init__(self, None, None)
        self.phase = PHASE_NULL
        self.name = "top"
        self.waitTasksEndTime = 0
        # setSimManager(self)

    def setWaitTasksEndTime(self,value):
        self.waitTasksEndTime = value

    @cocotb.coroutine
    def waitChild(self):
        while True:
            if self.canPhaseProgress(self.phase):
                break
            yield Timer(10000)

    def getPhase(self):
        return self.phase

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
        yield Timer(self.waitTasksEndTime)
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




###############################################################################
# LFSR python implementaion


###############################################################################
# Select the shift direction
#
class LFSR_SHIFT_DIR:
    SHIFT_RIGHT = 1
    SHIFT_LEFT  = 0

###############################################################################
# Galois LFSR right/left
#
class GaloisLFSR:

    def __init__(self, seed, equation=[1,2], widthReg=16, rightLeft=LFSR_SHIFT_DIR.SHIFT_RIGHT):
        self.equation  = equation
        self.rightLeft = rightLeft
        self.width     = widthReg-1
        self.maxValue  = (2**widthReg)-1
        self.state     = seed & self.maxValue

    def getRand(self):

        bitsList = []

        if self.rightLeft ==  LFSR_SHIFT_DIR.SHIFT_RIGHT:

            for index in range(self.width,-1,-1):
                if index == self.width :
                    bitsList.append(self.state & 0x01)
                elif index in  self.equation:
                    bitsList.append(((self.state >> index+1) ^ self.state) & 0x01)
                else :
                    bitsList.append((self.state >> index+1) & 0x01)
        else:

            for index in range(0, self.width+1):
                if index == 0:
                    bitsList.append((self.state >> self.width) & 0x01 )
                elif index in self.equation:
                    bitsList.append(((self.state >> index-1) ^ (self.state >> self.width)) & 0x01)
                else:
                    bitsList.append((self.state >> index-1) & 0x01)

            bitsList = bitsList[::-1] # reverse list

        self.state = int("".join([str(x) for x in bitsList]),2)



###############################################################################
# Fibonacci LFSR right/left
#
class FibonacciLFSR:

    def __init__(self, seed, equation=[0,2,3,5], widthReg=16, rightLeft=LFSR_SHIFT_DIR.SHIFT_RIGHT):
        self.equation  = equation
        self.rightLeft = rightLeft
        self.width     = widthReg-1
        self.maxValue  = (2**widthReg)-1
        self.state     = seed & self.maxValue


    def getRand(self):

        feedback = (self.state >> self.equation[0]) ^ (self.state >> self.equation[1])

        for index in range(2,len(self.equation)):
            feedback ^= (self.state >> self.equation[index])
        feedback &= 0x01

        if self.rightLeft == LFSR_SHIFT_DIR.SHIFT_RIGHT:
            self.state = ((self.state >> 1) | (feedback<<self.width)) & self.maxValue
        else:
            self.state = (((self.state << 1) & (self.maxValue << 1)) | (feedback)) & self.maxValue


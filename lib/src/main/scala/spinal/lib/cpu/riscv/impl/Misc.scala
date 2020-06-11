package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib.IMasterSlave
import spinal.lib.cpu.riscv.impl.Utils.InstructionCtrl


case class IrqUsage(isException : Boolean)


object Utils{
  object PC extends SpinalEnum(binarySequential){
    val INC,BRA,J,JR = newElement()
  }
  object BR extends SpinalEnum{
    val N, NE, EQ, GE, GEU, LT, LTU, J, JR = newElement()

    defaultEncoding = SpinalEnumEncoding("opt")(
      EQ -> 0,
      NE -> 1,
      J -> 2,
      JR -> 3,
      LT -> 4,
      GE -> 5,
      LTU -> 6,
      GEU -> 7,
      N -> 8
    )

    def isSignedComp(that : C) = !that.asBits(1)
  }

  object OP0 extends SpinalEnum(binarySequential){
    val RS, IMU, IMZ, IMJB = newElement()
    def X = RS
  }


  object OP1 extends SpinalEnum(binarySequential){
    val RS, IMI, IMS, PC = newElement()
    def X = RS
  }

  object WB extends SpinalEnum(binarySequential){
    val ALU, MEM, PC4, CSR1 = newElement()
    def X = ALU
  }

  object MWR extends SpinalEnum(binarySequential){
    val R, W, F = newElement()
  }


  object MSK extends SpinalEnum(binarySequential){
    val B,H,W = newElement()
    def X = B
  }


  object MFS extends SpinalEnum{
    val N, SI, SD, FA, FD = newElement()
  }


  object M extends SpinalEnum(binarySequential){
    val XRD, XWR = newElement()
    def X = XRD
  }

  object ALU extends SpinalEnum{
    val ADD, SLL1, SLT, SLTU, XOR, SRL, OR, AND, SUB, COPY, SRA = newElement()

    defaultEncoding = SpinalEnumEncoding("opt")(
      ADD -> 0,
      SLL1 -> 1,
      SLT -> 2,
      SLTU -> 3,
      XOR -> 4,
      SRL -> 5,
      OR -> 6,
      AND -> 7,
      SUB -> (8+0),
      COPY -> 15,
      SRA -> (8+5)
    )
    def X = ADD
    def isSltX(that : C) =  that.asBits === M"-01-"
    def isAddSub(that : C) =  that.asBits === M"-000"
  }
  def apply(x : Int) = 2

  object CSR extends SpinalEnum(binarySequential){
    val N, W, S, C = newElement()
  }


  case class InstructionCtrl() extends Bundle{
      val instVal = Bool()
      val br = BR()
      val jmp = Bool()
      val op0 = OP0()
      val op1 = OP1()
      val alu = ALU()
      val wb  = WB()
      val rfen = Bool
      val execute0AluBypass  = Bool
      val execute1AluBypass  = Bool
      val canInternalyStallWriteBack0 = Bool
      val men  = Bool
      val m  = M()
      val msk = MSK()
      val csr = CSR()
      val mfs = MFS()
      val useSrc0 = Bool
      val useSrc1 = Bool
      val extensionTag = Bits()
      val extensionData = Bits()
      val fencei  = Bool
  }

  def BASE                 = M"------------------------------11"
  def BASE_MEM             = M"-------------------------0-000--"
  def BASE_MEM_L           = M"--------------------------0-----"
  def BASE_MEM_S           = M"--------------------------1-----"
  def BASE_AUIPC           = M"-------------------------00101--"
  def BASE_LUI             = M"-------------------------01101--"
  def BASE_OPX             = M"-------------------------0-100--"
  def BASE_OPX_I           = M"--------------------------0-----"
  def BASE_OPX_SHIFT       = M"------------------01------------"
  def BASE_JALR            = M"-------------------------11001--"
  def BASE_JAL             = M"-------------------------11011--"
  def BASE_B               = M"-------------------------11000--"
  def BASE_CSR             = M"-------------------------11100--"
  def BASE_CSR_W           = M"------------------01------------"
  def BASE_CSR_S           = M"------------------10------------"
  def BASE_CSR_C           = M"------------------11------------"
  def BASE_CSR_I           = M"-----------------1--------------"
  def BASE_FENCEI          = M"000000000000000000010000000011--"


  object InstructionCtrl{
    def apply(instruction : Bits) : InstructionCtrl = {
      val ctrl = InstructionCtrl()
      ctrl.instVal := False
      ctrl.br := BR.N
      ctrl.jmp := False
      ctrl.op0 := OP0.X
      ctrl.op1 := OP1.X
      ctrl.alu := ALU.ADD
      ctrl.wb := WB.X
      ctrl.rfen := False
      ctrl.execute0AluBypass  := False
      ctrl.execute1AluBypass := False
      ctrl.canInternalyStallWriteBack0 := False
      ctrl.men := False
      ctrl.m := M.XRD
      ctrl.msk.assignFromBits(instruction(13 downto 12))
      ctrl.csr := CSR.N
      ctrl.mfs := MFS.N
      ctrl.useSrc0 := False
      ctrl.useSrc1 := False
      ctrl.extensionTag := 0
      ctrl.extensionData := 0
      ctrl.fencei := False

      when(instruction === BASE){
        ctrl.fencei := instruction === BASE_FENCEI
        when(instruction === BASE_MEM){
          ctrl.instVal := True
          ctrl.op0 := OP0.RS
          ctrl.alu := ALU.ADD
          ctrl.men := True
          ctrl.useSrc0 := True
          when(instruction === BASE_MEM_L){
            ctrl.op1 := OP1.IMI
            ctrl.wb := WB.MEM
            ctrl.rfen := True
            ctrl.m := M.XRD
          }otherwise{
            ctrl.op1 := OP1.IMS
            ctrl.m := M.XWR
            ctrl.useSrc1 := True
          }
        }
        when(instruction === BASE_AUIPC){
          ctrl.instVal := True
          ctrl.op0 := OP0.IMU
          ctrl.op1 := OP1.PC
          ctrl.alu := ALU.ADD
          ctrl.wb  := WB.ALU
          ctrl.rfen := True
          ctrl.execute0AluBypass := True
          ctrl.execute1AluBypass := True
        }
        when(instruction === BASE_LUI){
          ctrl.instVal := True
          ctrl.op0 := OP0.IMU
          ctrl.alu := ALU.COPY
          ctrl.wb  := WB.ALU
          ctrl.rfen := True
          ctrl.execute0AluBypass := True
          ctrl.execute1AluBypass := True
        }
        when(instruction === BASE_OPX){
          val isShift = instruction === BASE_OPX_SHIFT
          when(instruction === BASE_OPX_I) {
            when(!isShift || (instruction === M"0-00000-------------------------" && !(instruction(30) && !instruction(14)))) {
              ctrl.instVal := True
              ctrl.op0 := OP0.RS
              ctrl.op1 := OP1.IMI
              ctrl.alu.assignFromBits((isShift && instruction(30)) ## instruction(14 downto 12))
              ctrl.wb  := WB.ALU
              ctrl.rfen := True
              ctrl.execute0AluBypass := !isShift
              ctrl.execute1AluBypass := True
              ctrl.useSrc0 := True
            }
          }otherwise{
            when(instruction === M"0-00000-------------------------"){
              when(instruction(30) === False || instruction(14 downto 12) === B"000" || instruction(14 downto 12) === B"101"){
                ctrl.instVal := True
                ctrl.op0 := OP0.RS
                ctrl.op1 := OP1.RS
                ctrl.alu.assignFromBits(instruction(30) ## instruction(14 downto 12))
                ctrl.wb  := WB.ALU
                ctrl.rfen := True
                ctrl.execute0AluBypass := !isShift
                ctrl.execute1AluBypass := True
                ctrl.useSrc0 := True
                ctrl.useSrc1 := True
              }
            }
          }
        }
        when(instruction === BASE_JAL){
          ctrl.instVal := True
          ctrl.br := BR.J
          ctrl.alu := ALU.ADD
          ctrl.op0 := OP0.IMJB
          ctrl.op1 := OP1.PC
          ctrl.jmp := True
          ctrl.wb := WB.PC4
          ctrl.rfen := True
        }
        when(instruction === BASE_JALR){
          ctrl.instVal := True
          ctrl.br := BR.JR
          ctrl.jmp := True
          ctrl.op0 := OP0.RS
          ctrl.op1 := OP1.IMI
          ctrl.alu := ALU.ADD
          ctrl.wb := WB.PC4
          ctrl.rfen := True
          ctrl.useSrc0 := True
        }
        when(instruction === BASE_B){
          ctrl.instVal := True
          ctrl.alu := ALU.ADD
          ctrl.op0 := OP0.IMJB
          ctrl.op1 := OP1.PC
          ctrl.br.assignFromBits(False ## instruction(14 downto 12))
          ctrl.useSrc0 := True
          ctrl.useSrc1 := True
        }
        when(instruction === BASE_CSR){
          ctrl.instVal := True
          when(instruction === BASE_CSR_I){
            ctrl.op0 := OP0.IMZ
          }otherwise {
            ctrl.op0 := OP0.RS
          }
          ctrl.alu := ALU.COPY
          ctrl.wb := WB.CSR1
          ctrl.rfen := True
          ctrl.csr.assignFromBits(instruction(13 downto 12))
        }
      }
      ctrl
    }
  }



  case class IMM(instruction  : Bits) extends Area{
    // immediates
    def i = instruction(31 downto 20)
    def s = instruction(31 downto  25) ## instruction(11 downto  7)
    def b = instruction(31) ## instruction(7) ## instruction(30 downto 25) ## instruction(11 downto 8)
    def u = instruction(31 downto 12) ## U"x000"
    def j = instruction(31) ## instruction(19 downto 12) ## instruction(20) ## instruction(30 downto 21)
    def z = instruction(19 downto 15)

    // sign-extend immediates
    def i_sext = B((19 downto 0) -> i(11)) ## i
    def s_sext = B((19 downto 0) -> s(11)) ## s
    def b_sext = B((18 downto 0) -> b(11)) ## b ## False
    def j_sext = B((10 downto 0) -> j(19)) ## j ## False
  }

  def src0Range = 19 downto 15
  def src1Range = 24 downto 20
  def dstRange = 11 downto 7
}

object UtilsTest{
  class TopLevel extends Component{
    val instruction = in Bits(32 bit)
    val result = out(RegNext(Utils.InstructionCtrl(RegNext(instruction))))
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel())
    SpinalVerilog(new TopLevel().setDefinitionName("TopLevelV"))
  }
}
package spinal.lib.cpu.riscv.impl

import spinal.core._

object Utils{
  object PC extends SpinalEnum(sequancial){
    val INC,BR1,J,JR,EXC = newElement()
  }
  object BR extends SpinalEnum{
    val N,
    NE,
    EQ,
    GE,
    GEU,
    LT,
    LTU,
    J,
    JR = newElement()

    defaultEncoding = Encoding("opt")(
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
  }

  object OP1 extends SpinalEnum(sequancial){
    val RS1,
    IMU,
    IMZ = newElement()
    def X = RS1
  }


  object OP2 extends SpinalEnum(sequancial){
    val RS2,
    IMI,
    IMS,
    PC1 = newElement()

    def X = RS2
  }

  object WB extends SpinalEnum(sequancial){
    val ALU1,
    MEM,
    PC4,
    CSR1 = newElement()

    def X = ALU1
  }

  object MWR extends SpinalEnum(sequancial){
    val R,
    W,
    F
   = newElement()
  }


  object MSK extends SpinalEnum(sequancial){
    val B,H,W = newElement()

    def X = B
  }


  object MFS extends SpinalEnum{
    val N,
    SI,
    SD,
    FA,
    FD = newElement()
  }


  object M extends SpinalEnum(sequancial){
    val XRD,
    XWR = newElement()

    def X = XRD
  }

  object ALU extends SpinalEnum{
    val ADD,
    SLL1,
    SLT,
    SLTU,
    XOR1,
    SRL1,
    OR1,
    AND1,
    SUB1,
    COPY1,
    SRA1 = newElement()

    defaultEncoding = Encoding("opt")(
      ADD -> 0,
      SLL1 -> 1,
      SLT -> 2,
      SLTU -> 3,
      XOR1 -> 4,
      SRL1 -> 5,
      OR1 -> 6,
      AND1 -> 7,
      SUB1 -> (8+0),
      COPY1 -> 15,
      SRA1 -> (8+5)
    )
    def X = ADD
  }
  def apply(x : Int) = 2

  object CSR extends SpinalEnum(sequancial){
    val N,
    W,
    S,
    C = newElement()
  }


  case class InstructionCtrl() extends Bundle{
      val instVal = Bool()
      val br = BR()
      val jmp = Bool()
      val op1 = OP1()
      val op2 = OP2()
      val alu = ALU()
      val wb  = WB()
      val rfen = Bool
      val bypassable  = Bool
      val men  = Bool
      val m  = M()
      val msk = MSK()
      val csr = CSR()
      val mfs = MFS()
  }

  def BASE                 = M"------------------------------11"
  def BASE_MEM             = M"-------------------------0-000--"
  def BASE_MEM_L           = M"--------------------------0-----"
  def BASE_MEM_S           = M"--------------------------1-----"
  def BASE_AUIPC           = M"-------------------------00101--"
  def BASE_LUI             = M"-------------------------01101--"
  def BASE_OPX             = M"-------------------------0-100--"
  def BASE_OPX_I           = M"--------------------------0-----"
  def BASE_JALR            = M"-------------------------11001--"
  def BASE_JAL             = M"-------------------------11011--"
  def BASE_B               = M"-------------------------11000--"
  def BASE_CSR             = M"-------------------------11100--"
  def BASE_CSR_W           = M"------------------01------------"
  def BASE_CSR_S           = M"------------------10------------"
  def BASE_CSR_C           = M"------------------11------------"
  def BASE_CSR_I           = M"-----------------1--------------"


  object InstructionCtrl{
    def apply(instruction : Bits) : InstructionCtrl = {
      val ctrl = InstructionCtrl()

      ctrl.instVal := False
      ctrl.br := BR.N
      ctrl.jmp := False
      ctrl.op1 := OP1.X
      ctrl.op2 := OP2.X
      ctrl.alu := ALU.ADD
      ctrl.wb := WB.X
      ctrl.rfen := False
      ctrl.bypassable  := False
      ctrl.men := False
      ctrl.m := M.XRD
      ctrl.msk.assignFromBits(instruction(13,12))
      ctrl.csr := CSR.N
      ctrl.mfs := MFS.N

      when(instruction === BASE){
        when(instruction === BASE_MEM){
          ctrl.instVal := True
          ctrl.op1 := OP1.RS1
          ctrl.alu := ALU.ADD
          ctrl.men := True
          when(instruction === BASE_MEM_L){
            ctrl.op2 := OP2.IMI
            ctrl.wb := WB.MEM
            ctrl.rfen := True
            ctrl.m := M.XRD
          }otherwise{
            ctrl.op2 := OP2.IMS
            ctrl.m := M.XWR
          }
        }
        when(instruction === BASE_AUIPC){
          ctrl.instVal := True
          ctrl.op1 := OP1.IMU
          ctrl.op2 := OP2.PC1
          ctrl.alu := ALU.ADD
          ctrl.wb  := WB.ALU1
          ctrl.rfen := True
          ctrl.bypassable := True
        }
        when(instruction === BASE_LUI){
          ctrl.instVal := True
          ctrl.op1 := OP1.IMU
          ctrl.alu := ALU.COPY1
          ctrl.wb  := WB.ALU1
          ctrl.rfen := True
          ctrl.bypassable := True
        }
        when(instruction === BASE_OPX){
          ctrl.instVal := True
          ctrl.op1 := OP1.RS1
          val extra = False
          when(instruction === BASE_OPX_I) {
            ctrl.op2 := OP2.IMI
            when(instruction(13 downto 12) === B"01" ) {
              extra := instruction(30)
            }
          }otherwise{
            ctrl.op2 := OP2.RS2
            extra := instruction(30)
          }
          ctrl.alu.assignFromBits(extra ## instruction(14 downto 12))
          ctrl.wb  := WB.ALU1
          ctrl.rfen := True
          ctrl.bypassable := True
        }
        when(instruction === BASE_JAL){
          ctrl.instVal := True
          ctrl.br := BR.J
          ctrl.jmp := True
          ctrl.wb := WB.PC4
          ctrl.rfen := True
          ctrl.bypassable := True
        }
        when(instruction === BASE_JALR){
          ctrl.instVal := True
          ctrl.br := BR.JR
          ctrl.jmp := True
          ctrl.op1 := OP1.RS1
          ctrl.op2 := OP2.IMI
          ctrl.alu := ALU.ADD
          ctrl.wb := WB.PC4
          ctrl.rfen := True
        }
        when(instruction === BASE_B){
          ctrl.instVal := True
          ctrl.br.assignFromBits(False ## instruction(14 downto 12))
        }
        when(instruction === BASE_CSR){
          ctrl.instVal := True
          when(instruction === BASE_CSR_I){
            ctrl.op1 := OP1.IMZ
          }otherwise {
            ctrl.op1 := OP1.RS1
          }
          ctrl.alu := ALU.COPY1
          ctrl.wb := WB.CSR1
          ctrl.rfen := True
          ctrl.csr.assignFromBits(instruction(13 downto 12))
        }
      }
      ctrl
    }
  }

  def src0Range = 19 downto 15
  def src1Range = 24 downto 20
  def regFileRange = 11 downto 7
}




object UtilsTest{
  class TopLevel extends Component{
    val instruction = in Bits(32 bit)
    val result = out(RegNext(Utils.InstructionCtrl(RegNext(instruction))))
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel())
  }
}


//
//    def raw(instruction : Bits) : InstructionCtrl = {
//      val ctrl = instruction.map(
//        default -> InstructionCtrl(False,  BR.N  , False, OP1.IMU ,OP2.IMI, ALU.ADD , WB.ALU1, False,False, False, M.X  , MSK.B,  CSR.C, MFS.N),
//        LW      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True , False, True, M.XRD, MSK.W,  CSR.N, MFS.N),
//        LB      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True , False, True, M.XRD, MSK.B,  CSR.N, MFS.N),
//        LBU     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True , False, True, M.XRD, MSK.B, CSR.N, MFS.N),
//        LH      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True , False, True, M.XRD, MSK.H,  CSR.N, MFS.N),
//        LHU     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True , False, True, M.XRD, MSK.H, CSR.N, MFS.N),
//        SW      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMS , ALU.ADD , WB.X  , False, False, True, M.XWR, MSK.W,  CSR.N, MFS.N),
//        SB      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMS , ALU.ADD , WB.X  , False, False, True, M.XWR, MSK.B,  CSR.N, MFS.N),
//        SH      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMS , ALU.ADD , WB.X  , False, False, True, M.XWR, MSK.H,  CSR.N, MFS.N),
//
//        AUIPC   -> InstructionCtrl(True , BR.N  , False, OP1.IMU, OP2.PC1  , ALU.ADD  ,WB.ALU1, True, True , False, M.X ,  MSK.X,  CSR.N, MFS.N),
//        LUI     -> InstructionCtrl(True , BR.N  , False, OP1.IMU, OP2.X    , ALU.COPY1,WB.ALU1, True, True , False, M.X ,  MSK.X,  CSR.N, MFS.N),
//
//        ADDI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        ANDI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.AND1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        ORI     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.OR1  , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        XORI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.XOR1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SLTI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SLT , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SLTIU   -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SLTU, WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SLLI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SLL1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SRAI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SRA1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SRLI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SRL1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//
//        SLL     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SLL1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        ADD     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.ADD , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SUB     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SUB1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SLT     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SLT , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SLTU    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SLTU, WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        AND     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.AND1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        OR      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.OR1  , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        XOR     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.XOR1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SRA     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SRA1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SRL     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SRL1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//
//        JAL     -> InstructionCtrl(True , BR.J  , True , OP1.X  , OP2.X   , ALU.X   , WB.PC4, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        JALR    -> InstructionCtrl(True , BR.JR , True , OP1.RS1, OP2.IMI , ALU.ADD , WB.PC4, True, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
//        BEQ     -> InstructionCtrl(True , BR.EQ , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
//        BNE     -> InstructionCtrl(True , BR.NE , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
//        BGE     -> InstructionCtrl(True , BR.GE , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
//        BGEU    -> InstructionCtrl(True , BR.GEU, False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
//        BLT     -> InstructionCtrl(True , BR.LT , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
//        BLTU    -> InstructionCtrl(True , BR.LTU, False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
//
//        CSRRWI  -> InstructionCtrl(True , BR.N  , False, OP1.IMZ, OP2.X   , ALU.COPY1,WB.CSR1, True, False, False, M.X ,  MSK.X,  CSR.W, MFS.N),
//        CSRRSI  -> InstructionCtrl(True , BR.N  , False, OP1.IMZ, OP2.X   , ALU.COPY1,WB.CSR1, True, False, False, M.X ,  MSK.X,  CSR.S, MFS.N),
//        CSRRW   -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.X   , ALU.COPY1,WB.CSR1, True, False, False, M.X ,  MSK.X,  CSR.W, MFS.N),
//        CSRRS   -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.X   , ALU.COPY1,WB.CSR1, True, False, False, M.X ,  MSK.X,  CSR.S, MFS.N),
//        CSRRC   -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.X   , ALU.COPY1,WB.CSR1, True, False, False, M.X ,  MSK.X,  CSR.C, MFS.N),
//        CSRRCI  -> InstructionCtrl(True , BR.N  , False, OP1.IMZ, OP2.X   , ALU.COPY1,WB.CSR1, True, False, False, M.X ,  MSK.X,  CSR.C, MFS.N),
//        // TODO:
//        SCALL   -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, MFS.FD), // don't think I actually
//        SRET    -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, MFS.FD), // need to flush memory here
//        MRTS    -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, MFS.FD),
//        SBREAK  -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, MFS.FD),
//        WFI     -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, MFS.FD),
//
//        FENCE_I -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.SI),
//        FENCE   -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, True, M.X  , MSK.X,  CSR.N, MFS.SD)
//      )
//      //ctrl.msk.assignFromBits(instruction(13,12))
//      ctrl
//    }
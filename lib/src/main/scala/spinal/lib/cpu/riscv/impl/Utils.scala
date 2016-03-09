package spinal.lib.cpu.riscv.impl

import spinal.core._

object Utils{




  def BEQ                = M"-----------------000-----1100011"
  def BNE                = M"-----------------001-----1100011"
  def BLT                = M"-----------------100-----1100011"
  def BGE                = M"-----------------101-----1100011"
  def BLTU               = M"-----------------110-----1100011"
  def BGEU               = M"-----------------111-----1100011"
  def JALR               = M"-----------------000-----1100111"
  def JAL                = M"-------------------------1101111"
  def LUI                = M"-------------------------0110111"
  def AUIPC              = M"-------------------------0010111"
  def ADDI               = M"-----------------000-----0010011"
  def SLLI               = M"000000-----------001-----0010011"
  def SLTI               = M"-----------------010-----0010011"
  def SLTIU              = M"-----------------011-----0010011"
  def XORI               = M"-----------------100-----0010011"
  def SRLI               = M"000000-----------101-----0010011"
  def SRAI               = M"010000-----------101-----0010011"
  def ORI                = M"-----------------110-----0010011"
  def ANDI               = M"-----------------111-----0010011"
  def ADD                = M"0000000----------000-----0110011"
  def SUB                = M"0100000----------000-----0110011"
  def SLL                = M"0000000----------001-----0110011"
  def SLT                = M"0000000----------010-----0110011"
  def SLTU               = M"0000000----------011-----0110011"
  def XOR                = M"0000000----------100-----0110011"
  def SRL                = M"0000000----------101-----0110011"
  def SRA                = M"0100000----------101-----0110011"
  def OR                 = M"0000000----------110-----0110011"
  def AND                = M"0000000----------111-----0110011"
  def ADDIW              = M"-----------------000-----0011011"
  def SLLIW              = M"0000000----------001-----0011011"
  def SRLIW              = M"0000000----------101-----0011011"
  def SRAIW              = M"0100000----------101-----0011011"
  def ADDW               = M"0000000----------000-----0111011"
  def SUBW               = M"0100000----------000-----0111011"
  def SLLW               = M"0000000----------001-----0111011"
  def SRLW               = M"0000000----------101-----0111011"
  def SRAW               = M"0100000----------101-----0111011"
  def LB                 = M"-----------------000-----0000011"
  def LH                 = M"-----------------001-----0000011"
  def LW                 = M"-----------------010-----0000011"
  def LD                 = M"-----------------011-----0000011"
  def LBU                = M"-----------------100-----0000011"
  def LHU                = M"-----------------101-----0000011"
  def LWU                = M"-----------------110-----0000011"
  def SB                 = M"-----------------000-----0100011"
  def SH                 = M"-----------------001-----0100011"
  def SW                 = M"-----------------010-----0100011"
  def SD                 = M"-----------------011-----0100011"
  def FENCE              = M"-----------------000-----0001111"
  def FENCE_I            = M"-----------------001-----0001111"
  def MUL                = M"0000001----------000-----0110011"
  def MULH               = M"0000001----------001-----0110011"
  def MULHSU             = M"0000001----------010-----0110011"
  def MULHU              = M"0000001----------011-----0110011"
  def DIV                = M"0000001----------100-----0110011"
  def DIVU               = M"0000001----------101-----0110011"
  def REM                = M"0000001----------110-----0110011"
  def REMU               = M"0000001----------111-----0110011"
  def MULW               = M"0000001----------000-----0111011"
  def DIVW               = M"0000001----------100-----0111011"
  def DIVUW              = M"0000001----------101-----0111011"
  def REMW               = M"0000001----------110-----0111011"
  def REMUW              = M"0000001----------111-----0111011"
  def AMOADD_W           = M"00000------------010-----0101111"
  def AMOXOR_W           = M"00100------------010-----0101111"
  def AMOOR_W            = M"01000------------010-----0101111"
  def AMOAND_W           = M"01100------------010-----0101111"
  def AMOMIN_W           = M"10000------------010-----0101111"
  def AMOMAX_W           = M"10100------------010-----0101111"
  def AMOMINU_W          = M"11000------------010-----0101111"
  def AMOMAXU_W          = M"11100------------010-----0101111"
  def AMOSWAP_W          = M"00001------------010-----0101111"
  def LR_W               = M"00010--00000-----010-----0101111"
  def SC_W               = M"00011------------010-----0101111"
  def AMOADD_D           = M"00000------------011-----0101111"
  def AMOXOR_D           = M"00100------------011-----0101111"
  def AMOOR_D            = M"01000------------011-----0101111"
  def AMOAND_D           = M"01100------------011-----0101111"
  def AMOMIN_D           = M"10000------------011-----0101111"
  def AMOMAX_D           = M"10100------------011-----0101111"
  def AMOMINU_D          = M"11000------------011-----0101111"
  def AMOMAXU_D          = M"11100------------011-----0101111"
  def AMOSWAP_D          = M"00001------------011-----0101111"
  def LR_D               = M"00010--00000-----011-----0101111"
  def SC_D               = M"00011------------011-----0101111"
  def SCALL              = M"00000000000000000000000001110011"
  def SBREAK             = M"00000000000100000000000001110011"
  def SRET               = M"00010000000000000000000001110011"
  def SFENCE_VM          = M"000100000001-----000000001110011"
  def WFI                = M"00010000001000000000000001110011"
  def MRTH               = M"00110000011000000000000001110011"
  def MRTS               = M"00110000010100000000000001110011"
  def HRTS               = M"00100000010100000000000001110011"
  def CSRRW              = M"-----------------001-----1110011"
  def CSRRS              = M"-----------------010-----1110011"
  def CSRRC              = M"-----------------011-----1110011"
  def CSRRWI             = M"-----------------101-----1110011"
  def CSRRSI             = M"-----------------110-----1110011"
  def CSRRCI             = M"-----------------111-----1110011"
  def FADD_S             = M"0000000------------------1010011"
  def FSUB_S             = M"0000100------------------1010011"
  def FMUL_S             = M"0001000------------------1010011"
  def FDIV_S             = M"0001100------------------1010011"
  def FSGNJ_S            = M"0010000----------000-----1010011"
  def FSGNJN_S           = M"0010000----------001-----1010011"
  def FSGNJX_S           = M"0010000----------010-----1010011"
  def FMIN_S             = M"0010100----------000-----1010011"
  def FMAX_S             = M"0010100----------001-----1010011"
  def FSQRT_S            = M"010110000000-------------1010011"
  def FADD_D             = M"0000001------------------1010011"
  def FSUB_D             = M"0000101------------------1010011"
  def FMUL_D             = M"0001001------------------1010011"
  def FDIV_D             = M"0001101------------------1010011"
  def FSGNJ_D            = M"0010001----------000-----1010011"
  def FSGNJN_D           = M"0010001----------001-----1010011"
  def FSGNJX_D           = M"0010001----------010-----1010011"
  def FMIN_D             = M"0010101----------000-----1010011"
  def FMAX_D             = M"0010101----------001-----1010011"
  def FCVT_S_D           = M"010000000001-------------1010011"
  def FCVT_D_S           = M"010000100000-------------1010011"
  def FSQRT_D            = M"010110100000-------------1010011"
  def FLE_S              = M"1010000----------000-----1010011"
  def FLT_S              = M"1010000----------001-----1010011"
  def FEQ_S              = M"1010000----------010-----1010011"
  def FLE_D              = M"1010001----------000-----1010011"
  def FLT_D              = M"1010001----------001-----1010011"
  def FEQ_D              = M"1010001----------010-----1010011"
  def FCVT_W_S           = M"110000000000-------------1010011"
  def FCVT_WU_S          = M"110000000001-------------1010011"
  def FCVT_L_S           = M"110000000010-------------1010011"
  def FCVT_LU_S          = M"110000000011-------------1010011"
  def FMV_X_S            = M"111000000000-----000-----1010011"
  def FCLASS_S           = M"111000000000-----001-----1010011"
  def FCVT_W_D           = M"110000100000-------------1010011"
  def FCVT_WU_D          = M"110000100001-------------1010011"
  def FCVT_L_D           = M"110000100010-------------1010011"
  def FCVT_LU_D          = M"110000100011-------------1010011"
  def FMV_X_D            = M"111000100000-----000-----1010011"
  def FCLASS_D           = M"111000100000-----001-----1010011"
  def FCVT_S_W           = M"110100000000-------------1010011"
  def FCVT_S_WU          = M"110100000001-------------1010011"
  def FCVT_S_L           = M"110100000010-------------1010011"
  def FCVT_S_LU          = M"110100000011-------------1010011"
  def FMV_S_X            = M"111100000000-----000-----1010011"
  def FCVT_D_W           = M"110100100000-------------1010011"
  def FCVT_D_WU          = M"110100100001-------------1010011"
  def FCVT_D_L           = M"110100100010-------------1010011"
  def FCVT_D_LU          = M"110100100011-------------1010011"
  def FMV_D_X            = M"111100100000-----000-----1010011"
  def FLW                = M"-----------------010-----0000111"
  def FLD                = M"-----------------011-----0000111"
  def FSW                = M"-----------------010-----0100111"
  def FSD                = M"-----------------011-----0100111"
  def FMADD_S            = M"-----00------------------1000011"
  def FMSUB_S            = M"-----00------------------1000111"
  def FNMSUB_S           = M"-----00------------------1001011"
  def FNMADD_S           = M"-----00------------------1001111"
  def FMADD_D            = M"-----01------------------1000011"
  def FMSUB_D            = M"-----01------------------1000111"
  def FNMSUB_D           = M"-----01------------------1001011"
  def FNMADD_D           = M"-----01------------------1001111"
  def CUSTOM0            = M"-----------------000-----0001011"
  def CUSTOM0_RS1        = M"-----------------010-----0001011"
  def CUSTOM0_RS1_RS2    = M"-----------------011-----0001011"
  def CUSTOM0_RD         = M"-----------------100-----0001011"
  def CUSTOM0_RD_RS1     = M"-----------------110-----0001011"
  def CUSTOM0_RD_RS1_RS2 = M"-----------------111-----0001011"
  def CUSTOM1            = M"-----------------000-----0101011"
  def CUSTOM1_RS1        = M"-----------------010-----0101011"
  def CUSTOM1_RS1_RS2    = M"-----------------011-----0101011"
  def CUSTOM1_RD         = M"-----------------100-----0101011"
  def CUSTOM1_RD_RS1     = M"-----------------110-----0101011"
  def CUSTOM1_RD_RS1_RS2 = M"-----------------111-----0101011"
  def CUSTOM2            = M"-----------------000-----1011011"
  def CUSTOM2_RS1        = M"-----------------010-----1011011"
  def CUSTOM2_RS1_RS2    = M"-----------------011-----1011011"
  def CUSTOM2_RD         = M"-----------------100-----1011011"
  def CUSTOM2_RD_RS1     = M"-----------------110-----1011011"
  def CUSTOM2_RD_RS1_RS2 = M"-----------------111-----1011011"
  def CUSTOM3            = M"-----------------000-----1111011"
  def CUSTOM3_RS1        = M"-----------------010-----1111011"
  def CUSTOM3_RS1_RS2    = M"-----------------011-----1111011"
  def CUSTOM3_RD         = M"-----------------100-----1111011"
  def CUSTOM3_RD_RS1     = M"-----------------110-----1111011"
  def CUSTOM3_RD_RS1_RS2 = M"-----------------111-----1111011"


  object PC extends SpinalEnum{
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
  }

  object OP1 extends SpinalEnum{
    val RS1,
    IMU,
    IMZ = newElement()
    def X = RS1
  }


  object OP2 extends SpinalEnum{
    val RS2,
    IMI,
    IMS,
    PC1 = newElement()

    def X = RS2
  }

  object WB extends SpinalEnum{
    val ALU1,
    MEM,
    PC4,
    CSR1 = newElement()

    def X = ALU1
  }

  object MWR extends SpinalEnum{
    val R,
    W,
    F
   = newElement()
  }


  object MSK extends SpinalEnum{
    val B,
    BU,
    H,
    HU,
    W = newElement()

    def X = B
  }



  object MFS extends SpinalEnum{
    val N,
    SI,
    SD,
    FA,
    FD = newElement()
  }




//  object MT extends SpinalEnum{
//    val READ,
//    WRITE,
//    FENCE = newElement()
//  }
  object MT extends SpinalEnum{
    val X,
    B,
    H,
    W ,
    D ,
    BU,
    HU,
    WU= newElement()
  }

  object M extends SpinalEnum{
    val XRD,
    XWR = newElement()

    def X = XRD
  }

  object ALU extends SpinalEnum{
    val X,
    ADD,
    SLL1,
    XOR1,
    OR1,
    AND1,
    SRL1,
    SUB1,
    SRA1,
    SLT,
    SLTU,
    COPY1 = newElement()


//    val aaa = this()
  //  def isMulFN(fn: Bits, cmp: Bits) = fn(1,0) === cmp(1,0)
//    def isSub(cmd: ALU.T) : Bool = cmd === SUB1 || cmd === SRA1 || cmd === SLT || cmd === SLTU || cmd === COPY1
//    def isSLTU(cmd: ALU.T) = ???
  }

  object CSR extends SpinalEnum{
    val X,
    N,
    W,
    S,
    C,
    I,
    R = newElement()
  }



//  val ePC = enum(
//    'INC,
//     'BR,
//     'J,
//     'JR,
//     'EXC)
//
//  val eBR = enum(
//    'N,
//    'NE,
//    'EQ,
//    'GE,
//    'GEU,
//    'LT,
//    'LTU,
//    'J,
//    'JR)
//
//  val eOP1 = enum(
//    'RS1,
//   'IMU,
//   'IMZ)
//
//
//  val eOP2 = enum(
//    'RS2,
//    'IMI,
//    'IMS,
//    'PC)
//
//  val eWB = enum(
//    'ALU,
//    'MEM,
//    'PC4,
//    'CSR
//  )
//
//  val eMWR = enum(
//    'R,
//    'W,
//    'F
//  )
//
//
//  val eMSK = enum(
//    'B,
//    'BU,
//    'H,
//    'HU,
//    'W)
//
//
//
//  val eM = enum(
//    'N,
//    'SI,
//    'SD,
//    'FA,
//    'FD)
//
//
//  val eMT = enum(
//    'READ,
//    'WRITE,
//    'FENCE)
//
//  object InstructionCtrl{
//    def apply(
//      instVal : Bool,
//      br : BR.T,
//      isJmp : Bool,
//      op1 : OP1.T,
//      op2 : OP2.T,
//      alu : ALU.T,
//      wb : WB.T,
//      ren : Bool,
//      bypassable : Bool,
//      men : Bool,
//      m : M.T,
//      mt : MSK.T,
//      csr : CSR.T) : InstructionCtrl = {
//
//    }
//  }

  case class InstructionCtrl(
      instVal : Bool = Bool(),
      br : BR.T = BR(),
      jmp : Bool = Bool(),
      op1 : OP1.T = OP1(),
      op2 : OP2.T = OP2(),
      alu : ALU.T = ALU(),
      wb : WB.T = WB(),
      rfen : Bool = Bool,
      bypassable : Bool = Bool,
      men : Bool = Bool,
      m : M.T = M(),
      msk : MSK.T = MSK(),
      csr : CSR.T = CSR(),
      mfs : MFS.T = MFS())  extends BundleCase



  object InstructionCtrl{
    def apply(instruction : Bits) : InstructionCtrl = instruction.map(
      default -> InstructionCtrl(False,  BR.N  , False, OP1.IMU ,OP2.IMI, ALU.ADD , WB.ALU1, False,False, False, M.X  , MSK.B,  CSR.C, MFS.N),
      LW      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True , False, True, M.XRD, MSK.W,  CSR.N, MFS.N),
      LB      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True , False, True, M.XRD, MSK.B,  CSR.N, MFS.N),
      LBU     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True , False, True, M.XRD, MSK.BU, CSR.N, MFS.N),
      LH      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True , False, True, M.XRD, MSK.H,  CSR.N, MFS.N),
      LHU     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True , False, True, M.XRD, MSK.HU, CSR.N, MFS.N),
      SW      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMS , ALU.ADD , WB.X  , False, False, True, M.XWR, MSK.W,  CSR.N, MFS.N),
      SB      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMS , ALU.ADD , WB.X  , False, False, True, M.XWR, MSK.B,  CSR.N, MFS.N),
      SH      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMS , ALU.ADD , WB.X  , False, False, True, M.XWR, MSK.H,  CSR.N, MFS.N),

      AUIPC   -> InstructionCtrl(True , BR.N  , False, OP1.IMU, OP2.PC1  , ALU.ADD  ,WB.ALU1, True, True , False, M.X ,  MSK.X,  CSR.N, MFS.N),
      LUI     -> InstructionCtrl(True , BR.N  , False, OP1.IMU, OP2.X   , ALU.COPY1,WB.ALU1, True, True , False, M.X ,  MSK.X,  CSR.N, MFS.N),

      ADDI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      ANDI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.AND1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      ORI     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.OR1  , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      XORI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.XOR1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      SLTI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SLT , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      SLTIU   -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SLTU, WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      SLLI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SLL1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      SRAI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SRA1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      SRLI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SRL1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),

      SLL     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SLL1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      ADD     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.ADD , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      SUB     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SUB1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      SLT     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SLT , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      SLTU    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SLTU, WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      AND     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.AND1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      OR      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.OR1  , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      XOR     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.XOR1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      SRA     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SRA1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      SRL     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SRL1 , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),

      JAL     -> InstructionCtrl(True , BR.J  , True , OP1.X  , OP2.X   , ALU.X   , WB.PC4, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
      JALR    -> InstructionCtrl(True , BR.JR , True , OP1.RS1, OP2.IMI , ALU.ADD , WB.PC4, True, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
      BEQ     -> InstructionCtrl(True , BR.EQ , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
      BNE     -> InstructionCtrl(True , BR.NE , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
      BGE     -> InstructionCtrl(True , BR.GE , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
      BGEU    -> InstructionCtrl(True , BR.GEU, False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
      BLT     -> InstructionCtrl(True , BR.LT , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
      BLTU    -> InstructionCtrl(True , BR.LTU, False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),

      CSRRWI  -> InstructionCtrl(True , BR.N  , False, OP1.IMZ, OP2.X   , ALU.COPY1,WB.CSR1, True, False, False, M.X ,  MSK.X,  CSR.W, MFS.N),
      CSRRSI  -> InstructionCtrl(True , BR.N  , False, OP1.IMZ, OP2.X   , ALU.COPY1,WB.CSR1, True, False, False, M.X ,  MSK.X,  CSR.S, MFS.N),
      CSRRW   -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.X   , ALU.COPY1,WB.CSR1, True, False, False, M.X ,  MSK.X,  CSR.W, MFS.N),
      CSRRS   -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.X   , ALU.COPY1,WB.CSR1, True, False, False, M.X ,  MSK.X,  CSR.S, MFS.N),
      CSRRC   -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.X   , ALU.COPY1,WB.CSR1, True, False, False, M.X ,  MSK.X,  CSR.C, MFS.N),
      CSRRCI  -> InstructionCtrl(True , BR.N  , False, OP1.IMZ, OP2.X   , ALU.COPY1,WB.CSR1, True, False, False, M.X ,  MSK.X,  CSR.C, MFS.N),
      // TODO:
      SCALL   -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, MFS.FD), // don't think I actually
      SRET    -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, MFS.FD), // need to flush memory here
      MRTS    -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, MFS.FD),
      SBREAK  -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, MFS.FD),
      WFI     -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, MFS.FD),

      FENCE_I -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.SI),
      FENCE   -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, True, M.X  , MSK.X,  CSR.N, MFS.SD)
    )

  }

  def src0Range = 19 downto 15
  def src1Range = 24 downto 20

//  SpinalMap
//                             //
//                             //   inst val-                                                                                mem flush/sync
//                             //   |    br type                      alu fcn                 bypassable-                    |
//                             //   |    |     is jmp-                |        wb sel         |  mem en               csr cmd|
//                             //   |    |     |  op1 sel  op2 sel    |        |       rf wen |  |      mem cmd       |      |
//  val csignals =             //   |    |     |  |        |          |        |       |      |  |      |      mask type     |
//    InstructionCtrlLookup(io.imem.resp.M.inst,//   |  |        |          |        |       |      |  |      |      |      |      |
//      InstructionCtrl(N, BR_N  , N, OP1_X  , OP2_X   , ALU_X   , WB_X,   REN_0, N, MEN_0, M_X  , MT_X,  CSR.N, M_N),
//      Array(       //
//        LW      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True, False, True, M.XRD, MSK.W,  CSR.N, MFS.N),
//        LB      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True, False, True, M.XRD, MSK.B,  CSR.N, MFS.N),
//        LBU     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True, False, True, M.XRD, MSK.BU, CSR.N, MFS.N),
//        LH      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True, False, True, M.XRD, MSK.H,  CSR.N, MFS.N),
//        LHU     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.MEM, True, False, True, M.XRD, MSK.HU, CSR.N, MFS.N),
//        SW      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMS , ALU.ADD , WB.X  , False, False, True, M.XWR, MSK.W,  CSR.N, MFS.N),
//        SB      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMS , ALU.ADD , WB.X  , False, False, True, M.XWR, MSK.B,  CSR.N, MFS.N),
//        SH      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMS , ALU.ADD , WB.X  , False, False, True, M.XWR, MSK.H,  CSR.N, MFS.N),
//
//        AUIPC   -> InstructionCtrl(True , BR.N  , False, OP1.IMU, OP2.PC  , ALU.ADD  ,WB.ALU1, True, True , False, M.X ,  MSK.X,  CSR.N, MFS.N),
//        LUI     -> InstructionCtrl(True , BR.N  , False, OP1.IMU, OP2.X   , ALU.COPY1,WB.ALU1, True, True , False, M.X ,  MSK.X,  CSR.N, MFS.N),
//
//        ADDI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.ADD , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        ANDI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.AND , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        ORI     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.OR  , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        XORI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.XOR , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SLTI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SLT , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SLTIU   -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SLTU, WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SLLI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SLL , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SRAI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SRA , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SRLI    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.IMI , ALU.SRL , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//
//        SLL     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SLL , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        ADD     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.ADD , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SUB     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SUB , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SLT     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SLT , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SLTU    -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SLTU, WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        AND     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.AND , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        OR      -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.OR  , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        XOR     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.XOR , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SRA     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SRA , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        SRL     -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.RS2 , ALU.SRL , WB.ALU1, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//
//        JAL     -> InstructionCtrl(True , BR.J  , True , OP1.X  , OP2.X   , ALU.X   , WB.PC4, True, True , False, M.X  , MSK.X,  CSR.N, MFS.N),
//        JALR    -> InstructionCtrl(True , BR.JR , True , OP1.RS1, OP2.IMI , ALU.X   , WB.PC4, True, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
//        BEQ     -> InstructionCtrl(True , BR.EQ , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
//        BNE     -> InstructionCtrl(True , BR.NE , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
//        BGE     -> InstructionCtrl(True , BR.GE , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
//        BGEU    -> InstructionCtrl(True , BR.GEU, False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
//        BLT     -> InstructionCtrl(True , BR.LT , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
//        BLTU    -> InstructionCtrl(True , BR.LTU, False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, MFS.N),
//
//        CSRRWI  -> InstructionCtrl(True , BR.N  , False, OP1.IMZ, OP2.X   , ALU.COPY1,WB.CSR, True, False, False, M.X ,  MSK.X,  CSR.W, MFS.N),
//        CSRRSI  -> InstructionCtrl(True , BR.N  , False, OP1.IMZ, OP2.X   , ALU.COPY1,WB.CSR, True, False, False, M.X ,  MSK.X,  CSR.S, MFS.N),
//        CSRRW   -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.X   , ALU.COPY1,WB.CSR, True, False, False, M.X ,  MSK.X,  CSR.W, MFS.N),
//        CSRRS   -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.X   , ALU.COPY1,WB.CSR, True, False, False, M.X ,  MSK.X,  CSR.S, MFS.N),
//        CSRRC   -> InstructionCtrl(True , BR.N  , False, OP1.RS1, OP2.X   , ALU.COPY1,WB.CSR, True, False, False, M.X ,  MSK.X,  CSR.C, MFS.N),
//        CSRRCI  -> InstructionCtrl(True , BR.N  , False, OP1.IMZ, OP2.X   , ALU.COPY1,WB.CSR, True, False, False, M.X ,  MSK.X,  CSR.C, MFS.N),
//        // TODO:
//        SCALL   -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, M.FD), // don't think I actually
//        SRET    -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, M.FD), // need to flush memory here
//        MRTS    -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, M.FD),
//        SBREAK  -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, M.FD),
//        WFI     -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.I, M.FD),
//
//        FENCE.I -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, False, M.X  , MSK.X,  CSR.N, M.SI),
//        FENCE   -> InstructionCtrl(True , BR.N  , False, OP1.X  , OP2.X   , ALU.X   , WB.X  , False, False, True, M.X  , MSK.X,  CSR.N, M.SD)
//        // we are already sequentially consistent, so no need to honor the fence instruction
//      ))
}



//
//
//  val ePC = enum(
//    'INC,
//    'BR,
//    'J,
//    'JR,
//    'EXC)
//
//  val eBR = enum(
//    'N,
//    'NE,
//    'EQ,
//    'GE,
//    'GEU,
//    'LT,
//    'LTU,
//    'J,
//    'JR)
//
//  val eOP1 = enum(
//    'RS1,
//    'IMU,
//    'IMZ)
//
//
//  val eOP2 = enum(
//    'RS2,
//    'IMI,
//    'IMS,
//    'PC)
//
//  val eWB = enum(
//    'ALU,
//    'MEM,
//    'PC4,
//    'CSR
//  )
//
//  val eMWR = enum(
//    'R,
//    'W,
//    'F
//  )
//
//
//  val eMSK = enum(
//    'B,
//    'BU,
//    'H,
//    'HU,
//    'W)
//
//
//
//  val eM = enum(
//    'N,
//    'SI,
//    'SD,
//    'FA,
//    'FD)
//
//
//  val eMT = enum(
//    'READ,
//    'WRITE,
//    'FENCE)


object UtilsTest{
  class TopLevel extends Component{
    val instruction = in Bits(32 bit)
    val result = out(Utils.InstructionCtrl(instruction))
//    import Utils._
//    val result = out(InstructionCtrl(True,BR.N,False,OP1.IMU,OP2.IMI,ALU.ADD,WB.ALU1,False,False,False,M.FA,MSK.FENCE,CSR.C))
//    when(instruction === LW){
//      result := InstructionCtrl(True,BR.N,False,OP1.IMU,OP2.IMI,ALU.ADD,WB.ALU1,False,False,False,M.FA,MSK.FENCE,CSR.C)
//    }
//    when(instruction === LB) {
//      result := InstructionCtrl(True, BR.N, False, OP1.IMU, OP2.IMI, ALU.ADD, WB.ALU1, False, False, False, M.FA, MSK.FENCE, CSR.C)
//    }

  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}
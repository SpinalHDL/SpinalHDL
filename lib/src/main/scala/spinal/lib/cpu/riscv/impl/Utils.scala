package spinal.lib.cpu.riscv.impl

import spinal.core._

class Utils{


  val ePC = enum(
    'INC,
     'BR,
     'J,
     'JR,
     'EXC)

  val eBR = enum(
    'N,
    'NE,
    'EQ,
    'GE,
    'GEU,
    'LT,
    'LTU,
    'J,
    'JR)

  val eOP1 = enum(
    'RS1,
   'IMU,
   'IMZ)


  val eOP2 = enum(
    'RS2,
    'IMI,
    'IMS,
    'PC)

  val eWB = enum(
    'ALU,
    'MEM,
    'PC4,
    'CSR
  )

  val eMWR = enum(
    'R,
    'W,
    'F
  )


  val eMSK = enum(
    'B,
    'BU,
    'H,
    'HU,
    'W)



  val eM = enum(
    'N,
    'SI,
    'SD,
    'FA,
    'FD)


  val eMT = enum(
    'READ,
    'WRITE,
    'FENCE)


//  val ALU_X    = Bits(0) // TODO use a more optimal decode table, which uses "???" format
//  val ALU_ADD  = Bits(0)
//  val ALU_SLL  = Bits(1)
//  val ALU_XOR  = Bits(4)
//  val ALU_OR   = Bits(6)
//  val ALU_AND  = Bits(7)
//  val ALU_SRL  = Bits(5)
//  val ALU_SUB  = Bits(10)
//  val ALU_SRA  = Bits(11)
//  val ALU_SLT  = Bits(12)
//  val ALU_SLTU = Bits(14)
//  val ALU_COPY1= Bits(8)

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

//  class InstructionCtrl extends Bundle{
//    val instVal = Bool()
//    val br = eBR()
//    val isJmp = Bool()
//    val op1 = eOP1()
//    val op2 = eOP2()
//    val alu = eALU()
//    val wb = eWB()
//    val ren = wREN()
//    val bypassable = Bool()
//    val men = eMEN()
//    val m = eM()
//    val mt = eMT()
//    val csr = eCSR()
//    val mfs = ()
//
//  }
                             //
                             //   inst val-                                                                                mem flush/sync
                             //   |    br type                      alu fcn                 bypassable-                    |
                             //   |    |     is jmp-                |        wb sel         |  mem en               csr cmd|
                             //   |    |     |  op1 sel  op2 sel    |        |       rf wen |  |      mem cmd       |      |
  /*val csignals =             //   |    |     |  |        |          |        |       |      |  |      |      mask type     |
    ListLookup(io.imem.resp.M.inst,//   |  |        |          |        |       |      |  |      |      |      |      |
      List(N, BR_N  , N, OP1_X  , OP2_X   , ALU_X   , WB_X,   REN_0, N, MEN_0, M_X  , MT_X,  CSR.N, M_N),
      Array(       //
        LW      -> List(Y, BR_N  , N, OP1_RS1, OP2_IMI , ALU_ADD , WB_MEM, REN_1, N, MEN_1, M_XRD, MT_W,  CSR.N, M_N),
        LB      -> List(Y, BR_N  , N, OP1_RS1, OP2_IMI , ALU_ADD , WB_MEM, REN_1, N, MEN_1, M_XRD, MT_B,  CSR.N, M_N),
        LBU     -> List(Y, BR_N  , N, OP1_RS1, OP2_IMI , ALU_ADD , WB_MEM, REN_1, N, MEN_1, M_XRD, MT_BU, CSR.N, M_N),
        LH      -> List(Y, BR_N  , N, OP1_RS1, OP2_IMI , ALU_ADD , WB_MEM, REN_1, N, MEN_1, M_XRD, MT_H,  CSR.N, M_N),
        LHU     -> List(Y, BR_N  , N, OP1_RS1, OP2_IMI , ALU_ADD , WB_MEM, REN_1, N, MEN_1, M_XRD, MT_HU, CSR.N, M_N),
        SW      -> List(Y, BR_N  , N, OP1_RS1, OP2_IMS , ALU_ADD , WB_X  , REN_0, N, MEN_1, M_XWR, MT_W,  CSR.N, M_N),
        SB      -> List(Y, BR_N  , N, OP1_RS1, OP2_IMS , ALU_ADD , WB_X  , REN_0, N, MEN_1, M_XWR, MT_B,  CSR.N, M_N),
        SH      -> List(Y, BR_N  , N, OP1_RS1, OP2_IMS , ALU_ADD , WB_X  , REN_0, N, MEN_1, M_XWR, MT_H,  CSR.N, M_N),

        AUIPC   -> List(Y, BR_N  , N, OP1_IMU, OP2_PC  , ALU_ADD  ,WB_ALU, REN_1, Y, MEN_0, M_X ,  MT_X,  CSR.N, M_N),
        LUI     -> List(Y, BR_N  , N, OP1_IMU, OP2_X   , ALU_COPY1,WB_ALU, REN_1, Y, MEN_0, M_X ,  MT_X,  CSR.N, M_N),

        ADDI    -> List(Y, BR_N  , N, OP1_RS1, OP2_IMI , ALU_ADD , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        ANDI    -> List(Y, BR_N  , N, OP1_RS1, OP2_IMI , ALU_AND , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        ORI     -> List(Y, BR_N  , N, OP1_RS1, OP2_IMI , ALU_OR  , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        XORI    -> List(Y, BR_N  , N, OP1_RS1, OP2_IMI , ALU_XOR , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        SLTI    -> List(Y, BR_N  , N, OP1_RS1, OP2_IMI , ALU_SLT , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        SLTIU   -> List(Y, BR_N  , N, OP1_RS1, OP2_IMI , ALU_SLTU, WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        SLLI    -> List(Y, BR_N  , N, OP1_RS1, OP2_IMI , ALU_SLL , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        SRAI    -> List(Y, BR_N  , N, OP1_RS1, OP2_IMI , ALU_SRA , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        SRLI    -> List(Y, BR_N  , N, OP1_RS1, OP2_IMI , ALU_SRL , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),

        SLL     -> List(Y, BR_N  , N, OP1_RS1, OP2_RS2 , ALU_SLL , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        ADD     -> List(Y, BR_N  , N, OP1_RS1, OP2_RS2 , ALU_ADD , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        SUB     -> List(Y, BR_N  , N, OP1_RS1, OP2_RS2 , ALU_SUB , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        SLT     -> List(Y, BR_N  , N, OP1_RS1, OP2_RS2 , ALU_SLT , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        SLTU    -> List(Y, BR_N  , N, OP1_RS1, OP2_RS2 , ALU_SLTU, WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        AND     -> List(Y, BR_N  , N, OP1_RS1, OP2_RS2 , ALU_AND , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        OR      -> List(Y, BR_N  , N, OP1_RS1, OP2_RS2 , ALU_OR  , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        XOR     -> List(Y, BR_N  , N, OP1_RS1, OP2_RS2 , ALU_XOR , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        SRA     -> List(Y, BR_N  , N, OP1_RS1, OP2_RS2 , ALU_SRA , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        SRL     -> List(Y, BR_N  , N, OP1_RS1, OP2_RS2 , ALU_SRL , WB_ALU, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),

        JAL     -> List(Y, BR_J  , Y, OP1_X  , OP2_X   , ALU_X   , WB_PC4, REN_1, Y, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        JALR    -> List(Y, BR_JR , Y, OP1_RS1, OP2_IMI , ALU_X   , WB_PC4, REN_1, N, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        BEQ     -> List(Y, BR_EQ , N, OP1_X  , OP2_X   , ALU_X   , WB_X  , REN_0, N, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        BNE     -> List(Y, BR_NE , N, OP1_X  , OP2_X   , ALU_X   , WB_X  , REN_0, N, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        BGE     -> List(Y, BR_GE , N, OP1_X  , OP2_X   , ALU_X   , WB_X  , REN_0, N, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        BGEU    -> List(Y, BR_GEU, N, OP1_X  , OP2_X   , ALU_X   , WB_X  , REN_0, N, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        BLT     -> List(Y, BR_LT , N, OP1_X  , OP2_X   , ALU_X   , WB_X  , REN_0, N, MEN_0, M_X  , MT_X,  CSR.N, M_N),
        BLTU    -> List(Y, BR_LTU, N, OP1_X  , OP2_X   , ALU_X   , WB_X  , REN_0, N, MEN_0, M_X  , MT_X,  CSR.N, M_N),

        CSRRWI  -> List(Y, BR_N  , N, OP1_IMZ, OP2_X   , ALU_COPY1,WB_CSR, REN_1, N, MEN_0, M_X ,  MT_X,  CSR.W, M_N),
        CSRRSI  -> List(Y, BR_N  , N, OP1_IMZ, OP2_X   , ALU_COPY1,WB_CSR, REN_1, N, MEN_0, M_X ,  MT_X,  CSR.S, M_N),
        CSRRW   -> List(Y, BR_N  , N, OP1_RS1, OP2_X   , ALU_COPY1,WB_CSR, REN_1, N, MEN_0, M_X ,  MT_X,  CSR.W, M_N),
        CSRRS   -> List(Y, BR_N  , N, OP1_RS1, OP2_X   , ALU_COPY1,WB_CSR, REN_1, N, MEN_0, M_X ,  MT_X,  CSR.S, M_N),
        CSRRC   -> List(Y, BR_N  , N, OP1_RS1, OP2_X   , ALU_COPY1,WB_CSR, REN_1, N, MEN_0, M_X ,  MT_X,  CSR.C, M_N),
        CSRRCI  -> List(Y, BR_N  , N, OP1_IMZ, OP2_X   , ALU_COPY1,WB_CSR, REN_1, N, MEN_0, M_X ,  MT_X,  CSR.C, M_N),
        // TODO:
        SCALL   -> List(Y, BR_N  , N, OP1_X  , OP2_X   , ALU_X   , WB_X  , REN_0, N, MEN_0, M_X  , MT_X,  CSR.I, M_FD), // don't think I actually
        SRET    -> List(Y, BR_N  , N, OP1_X  , OP2_X   , ALU_X   , WB_X  , REN_0, N, MEN_0, M_X  , MT_X,  CSR.I, M_FD), // need to flush memory here
        MRTS    -> List(Y, BR_N  , N, OP1_X  , OP2_X   , ALU_X   , WB_X  , REN_0, N, MEN_0, M_X  , MT_X,  CSR.I, M_FD),
        SBREAK  -> List(Y, BR_N  , N, OP1_X  , OP2_X   , ALU_X   , WB_X  , REN_0, N, MEN_0, M_X  , MT_X,  CSR.I, M_FD),
        WFI     -> List(Y, BR_N  , N, OP1_X  , OP2_X   , ALU_X   , WB_X  , REN_0, N, MEN_0, M_X  , MT_X,  CSR.I, M_FD),

        FENCE_I -> List(Y, BR_N  , N, OP1_X  , OP2_X   , ALU_X   , WB_X  , REN_0, N, MEN_0, M_X  , MT_X,  CSR.N, M_SI),
        FENCE   -> List(Y, BR_N  , N, OP1_X  , OP2_X   , ALU_X   , WB_X  , REN_0, N, MEN_1, M_X  , MT_X,  CSR.N, M_SD)
        // we are already sequentially consistent, so no need to honor the fence instruction
      ))*/
}
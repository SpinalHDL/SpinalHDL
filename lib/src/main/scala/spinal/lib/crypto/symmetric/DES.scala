/******************************************************************************
  * DES Block
  *                      _________
  *                     |         |
  *    -- Plaintext --->|   DES   |-- Ciphertext -->
  *       (64 bits)     |_________|   (64 bits)
  *                          |
  *                      Key (56 bits)
  */
package spinal.lib.crypto.symmetric

import spinal.core._
import spinal.lib._

case class DESGenerics(initialPermutation  : Seq[Int] = Seq(58, 50, 42, 34, 26, 18, 10, 2, 60, 52, 44, 36, 28, 20, 12, 4,
                                                            62, 54, 46, 38, 30, 22, 14, 6, 64, 56, 48, 40, 32, 24, 16, 8,
                                                            57, 49, 41, 33, 25, 17, 9,  1, 59, 51, 43, 35, 27, 19, 11, 3,
                                                            61, 53, 45, 37, 29, 21, 13, 5, 63, 55, 47, 39, 31, 23, 15, 7),
                       finalPermutation    : Seq[Int] = Seq(40, 8, 48, 16, 56, 24, 64, 32, 39, 7, 47, 15, 55, 23, 63, 31,
                                                            38, 6, 46, 14, 54, 22, 62, 30, 37, 5, 45, 13, 53, 21, 61, 29,
                                                            36, 4, 44, 12, 52, 20, 60, 28, 35, 3, 43, 11, 51, 19, 59, 27,
                                                            34, 2, 42, 10, 50, 18, 58, 26, 33, 1, 41, 9,  49, 17, 57, 25),
                       expansion           : List[Int] = List(32,  1,  2,  3,  4,  5,  4,  5,  6,  7,  8,  9,
                                                               8,  9, 10, 11, 12, 13, 12, 13, 14, 15, 16, 17,
                                                              16, 17, 18, 19, 20, 21, 20, 21, 22, 23, 24, 25,
                                                              24, 25, 26, 27, 28, 29, 28, 29, 30, 31, 32,  1),
                       fixedPermutation    : List[Int] =   List(16,  7 , 20, 21, 29, 12, 28, 17,  1, 15, 23, 26,  5, 18, 31, 10,
                                                                 2,  8, 24, 14, 32, 27,  3,  9, 19, 13, 30,  6, 22, 11,  4, 25),
                       pc_1                : Seq[Int] =   Seq(57, 49, 41, 33, 25, 17,  9,  1, 58, 50, 42, 34, 26, 18,
                                                              10,  2, 59, 51, 43, 35, 27, 19, 11,  3, 60, 52, 44, 36,
                                                              63, 55, 47, 39, 31, 23, 15,  7, 62, 54, 46, 38, 30, 22,
                                                              14,  6, 61, 53, 45, 37, 29, 21, 13,  5, 28, 20, 12,  4),
                       pc_2                : Seq[Int] =   Seq(14, 17, 11, 24,  1,  5,  3, 28, 15,  6, 21, 10,
                                                              23, 19, 12,  4, 26,  8, 16,  7, 27, 20, 13,  2,
                                                              41, 52, 31, 37, 47, 55, 30, 40, 51, 45, 33, 48,
                                                              44, 49, 39, 56, 34, 53, 46, 42, 50, 36, 29, 32),
                       sBox_1              : List[Int] = List(14,  4, 13, 1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9, 0,  7,
                                                               0, 15,  7, 4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5, 3,  8,
                                                               4,  1, 14, 8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10, 5,  0,
                                                              15, 12,  8, 2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0, 6, 13),
                       sBox_2              : List[Int] = List(15,  1,  8, 14,  6, 11,  3,  4,  9, 7,  2, 13, 12, 0,  5, 10,
                                                               3, 13,  4,  7, 15,  2,  8, 14, 12, 0,  1, 10,  6, 9, 11,  5,
                                                               0, 14,  7, 11, 10,  4, 13,  1,  5, 8, 12,  6,  9, 3,  2, 15,
                                                               13,  8, 10,  1,  3, 15,  4,  2, 11, 6,  7, 12,  0, 5, 14,  9),
                       sBox_3              : List[Int] = List(10,  0,  9, 14, 6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8,
                                                              13,  7,  0,  9, 3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1,
                                                              13,  6,  4,  9, 8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7,
                                                              1, 10, 13,  0, 6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12),
                       sBox_4              : List[Int] = List( 7, 13, 14, 3,  0,  6,  9, 10,  1, 2, 8,  5, 11, 12,  4, 15,
                                                              13,  8, 11, 5,  6, 15,  0,  3,  4, 7, 2, 12,  1, 10, 14,  9,
                                                              10,  6,  9, 0, 12, 11,  7, 13, 15, 1, 3, 14,  5,  2,  8,  4,
                                                              3, 15,  0, 6, 10,  1, 13,  8,  9, 4, 5, 11, 12,  7,  2, 14),
                       sBox_5              : List[Int] = List( 2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13, 0, 14, 9,
                                                              14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3, 9,  8,  6,
                                                               4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6, 3,  0, 14,
                                                              11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10, 4,  5,  3),
                       sBox_6              : List[Int] = List(12,  1, 10, 15, 9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11,
                                                              10, 15,  4,  2, 7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8,
                                                               9, 14, 15,  5, 2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6,
                                                               4,  3,  2, 12, 9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13),
                       sBox_7              : List[Int] = List( 4, 11,  2, 14, 15, 0,  8, 13,  3, 12, 9,  7,  5, 10, 6,  1,
                                                               13,  0, 11,  7,  4, 9,  1, 10, 14,  3, 5, 12,  2, 15, 8,  6,
                                                               1,  4, 11, 13, 12, 3,  7, 14, 10, 15, 6,  8,  0,  5, 9,  2,
                                                               6, 11, 13,  8,  1, 4, 10,  7,  9,  5, 0, 15, 14,  2, 3, 12),
                       sBox_8              : List[Int] = List(13,  2,  8, 4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7,
                                                               1, 15, 13, 8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2,
                                                               7, 11,  4, 1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8,
                                                               2,  1, 14, 7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11)
                      ){

  val blockWidth : BitCount = 64 bits
  val keyWidth   : BitCount = 56 bits
  val nbrRound   : Int      = 16

  val oneShiftRound = List(1,2,9,16) // 1 left for 1,2,9,16 and others 2 left shift
}

case class DEScmd(g : DESGenerics) extends Bundle{
  val key   = Bits(g.keyWidth)
  val block = Bits(g.blockWidth)
}


case class DESrsp(g: DESGenerics) extends Bundle{
  val block = Bits(g.blockWidth)
}

class DES(g : DESGenerics) extends Component{

  val io = new Bundle{
    val cmd  = slave  Stream(DEScmd(g))
    val res  = master Flow(DESrsp(g))
  }

  io.cmd.ready  := False


  val roundNbr = UInt(log2Up(g.nbrRound) bits)
  val init : Bool = io.cmd.valid.rise(False)
  val nextRound   = Reg(Bool) init(False) setWhen(init) clearWhen(roundNbr === g.nbrRound)


  val ctnRound = new Area{
    val round = Reg(UInt(log2Up(g.nbrRound) bits))

    when(nextRound){ round := round + 1 }
    when(init){ round := 0}

    roundNbr := round
  }



  val initialPermutation = new Area{
    val perm = Cat(g.initialPermutation.map(index => io.cmd.block(index - 1)))
  }




  val keySchedule = new Area{

    val shiftKey   = Reg(Bits(g.keyWidth))

    val leftRange  = 55 downto 28
    val rightRange = 27 downto 0

    // parity drop : 64bits -> 56 bits
    // TODO : check if the key is 56 or 64 bits
    when(init){ shiftKey := io.cmd.key } //Cat(g.pc_1.map(index => io.cmd.key(index-1))) }

    // shift key
    val shiftRes   = Bits(56 bits)
    when(g.oneShiftRound.map(index => ctnRound.round === (index-1)).reduce(_ || _) ){
      shiftRes  := shiftKey(leftRange).asBits.rotateLeft(1) ## shiftKey(rightRange).asBits.rotateLeft(1)
    }otherwise{
      shiftRes  := shiftKey(leftRange).asBits.rotateLeft(2) ## shiftKey(rightRange).asBits.rotateLeft(2)
    }

    // update key shift
    when(nextRound){ shiftKey := shiftRes }

    // compression : (56bits -> 48 bits)
    val keyRound = Cat(g.pc_2.map(index => shiftRes(56 - index )))
  }


  val funcDES = new Area{

    // list of SBox ROM 1 to 8
    val sBox     = List(Mem(Bits(4 bits), g.sBox_1.map(B(_))),
                        Mem(Bits(4 bits), g.sBox_2.map(B(_))),
                        Mem(Bits(4 bits), g.sBox_3.map(B(_))),
                        Mem(Bits(4 bits), g.sBox_4.map(B(_))),
                        Mem(Bits(4 bits), g.sBox_5.map(B(_))),
                        Mem(Bits(4 bits), g.sBox_6.map(B(_))),
                        Mem(Bits(4 bits), g.sBox_7.map(B(_))),
                        Mem(Bits(4 bits), g.sBox_8.map(B(_))))

    val rightRound   = Bits(32 bits) // set in roundArea

    // xor the key with the right block expanded(32 bits -> 48 bits)
    val xorRes = keySchedule.keyRound ^ Cat(g.expansion.map(index => rightRound(index-1)))

    // sBox stage
    val boxRes   = Bits(32 bits)
    for(i <- 0 to sBox.size-1){
      val addrSBox = xorRes(i*6+6-1 downto i*6)
      boxRes(i*4+4-1 downto i*4) := sBox(i).readAsync( (addrSBox(5) ## addrSBox(0) ## addrSBox(4 downto 1)).asUInt )
    }

    // fixed permutation
    val rResult = Cat(g.fixedPermutation.map(index => boxRes(index - 1)))
  }


  val roundArea = new Area{
    val inBlock    = Reg(Bits(g.blockWidth))

    val leftRange  = 63 downto 32
    val rightRange = 31 downto 0

    val outBlock = inBlock(rightRange) ## (inBlock(leftRange) ^ funcDES.rResult)

    when(init){ inBlock := initialPermutation.perm }
    when(nextRound){ inBlock := outBlock }

    funcDES.rightRound  := inBlock(rightRange)
  }



  val finalPermutation = new Area{
    val perm = Cat(g.finalPermutation.map(index => roundArea.outBlock(index - 1)))
  }

  io.res.block := finalPermutation.perm // TODOD : regNext output ??
  io.res.valid := False

}
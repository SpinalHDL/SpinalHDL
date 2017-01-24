/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Lib                          **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/  MIT Licence                      **
**                                                                           **
** Permission is hereby granted, free of charge, to any person obtaining a   **
** copy of this software and associated documentation files (the "Software"),**
** to deal in the Software without restriction, including without limitation **
** the rights to use, copy, modify, merge, publish, distribute, sublicense,  **
** and/or sell copies of the Software, and to permit persons to whom the     **
** Software is furnished to do so, subject to the following conditions:      **
**                                                                           **
** The above copyright notice and this permission notice shall be included   **
** in all copies or substantial portions of the Software.                    **
**                                                                           **
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS   **
** OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                **
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.    **
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY      **
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT **
** OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR  **
** THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                **
\*                                                                           */
package spinal.lib.crypto.symmetric

import spinal.core._


/**
  * Contains all constants for the DES Block
  */
case class DESBlockGenerics(){

  val initialPermutation = Seq(
    58, 50, 42, 34, 26, 18, 10, 2, 60, 52, 44, 36, 28, 20, 12, 4,
    62, 54, 46, 38, 30, 22, 14, 6, 64, 56, 48, 40, 32, 24, 16, 8,
    57, 49, 41, 33, 25, 17, 9,  1, 59, 51, 43, 35, 27, 19, 11, 3,
    61, 53, 45, 37, 29, 21, 13, 5, 63, 55, 47, 39, 31, 23, 15, 7)

  val finalPermutation   = Seq(
    40, 8, 48, 16, 56, 24, 64, 32, 39, 7, 47, 15, 55, 23, 63, 31,
    38, 6, 46, 14, 54, 22, 62, 30, 37, 5, 45, 13, 53, 21, 61, 29,
    36, 4, 44, 12, 52, 20, 60, 28, 35, 3, 43, 11, 51, 19, 59, 27,
    34, 2, 42, 10, 50, 18, 58, 26, 33, 1, 41, 9,  49, 17, 57, 25)

  val expansion  = List(
    32,  1,  2,  3,  4,  5,  4,  5,  6,  7,  8,  9,
    8,  9, 10, 11, 12, 13, 12, 13, 14, 15, 16, 17,
    16, 17, 18, 19, 20, 21, 20, 21, 22, 23, 24, 25,
    24, 25, 26, 27, 28, 29, 28, 29, 30, 31, 32,  1)

  val fixedPermutation  =   List(
    16,  7 , 20, 21, 29, 12, 28, 17,  1, 15, 23, 26,  5, 18, 31, 10,
    2,  8, 24, 14, 32, 27,  3,  9, 19, 13, 30,  6, 22, 11,  4, 25)

  val pc_1   =  Seq(
    57, 49, 41, 33, 25, 17,  9,  1, 58, 50, 42, 34, 26, 18,
    10,  2, 59, 51, 43, 35, 27, 19, 11,  3, 60, 52, 44, 36,
    63, 55, 47, 39, 31, 23, 15,  7, 62, 54, 46, 38, 30, 22,
    14,  6, 61, 53, 45, 37, 29, 21, 13,  5, 28, 20, 12,  4)

  val pc_2    =   Seq(
    14, 17, 11, 24,  1,  5,  3, 28, 15,  6, 21, 10,
    23, 19, 12,  4, 26,  8, 16,  7, 27, 20, 13,  2,
    41, 52, 31, 37, 47, 55, 30, 40, 51, 45, 33, 48,
    44, 49, 39, 56, 34, 53, 46, 42, 50, 36, 29, 32)

  /* SBox definition  */
  val sBox_1   = List(
    14,  4, 13, 1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9, 0,  7,
    0, 15,  7, 4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5, 3,  8,
    4,  1, 14, 8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10, 5,  0,
    15, 12,  8, 2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0, 6, 13)

  val sBox_2   = List(
    15,  1,  8, 14,  6, 11,  3,  4,  9, 7,  2, 13, 12, 0,  5, 10,
    3, 13,  4,  7, 15,  2,  8, 14, 12, 0,  1, 10,  6, 9, 11,  5,
    0, 14,  7, 11, 10,  4, 13,  1,  5, 8, 12,  6,  9, 3,  2, 15,
    13,  8, 10,  1,  3, 15,  4,  2, 11, 6,  7, 12,  0, 5, 14,  9)

  val sBox_3   = List(
    10,  0,  9, 14, 6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8,
    13,  7,  0,  9, 3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1,
    13,  6,  4,  9, 8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7,
    1, 10, 13,  0, 6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12)

  val sBox_4   = List(
    7, 13, 14, 3,  0,  6,  9, 10,  1, 2, 8,  5, 11, 12,  4, 15,
    13,  8, 11, 5,  6, 15,  0,  3,  4, 7, 2, 12,  1, 10, 14,  9,
    10,  6,  9, 0, 12, 11,  7, 13, 15, 1, 3, 14,  5,  2,  8,  4,
    3, 15,  0, 6, 10,  1, 13,  8,  9, 4, 5, 11, 12,  7,  2, 14)

  val sBox_5   = List(
    2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13, 0, 14, 9,
    14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3, 9,  8,  6,
    4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6, 3,  0, 14,
    11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10, 4,  5,  3)

  val sBox_6   = List(
    12,  1, 10, 15, 9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11,
    10, 15,  4,  2, 7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8,
    9, 14, 15,  5, 2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6,
    4,  3,  2, 12, 9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13)

  val sBox_7   = List(
    4, 11,  2, 14, 15, 0,  8, 13,  3, 12, 9,  7,  5, 10, 6,  1,
    13,  0, 11,  7,  4, 9,  1, 10, 14,  3, 5, 12,  2, 15, 8,  6,
    1,  4, 11, 13, 12, 3,  7, 14, 10, 15, 6,  8,  0,  5, 9,  2,
    6, 11, 13,  8,  1, 4, 10,  7,  9,  5, 0, 15, 14,  2, 3, 12)

  val sBox_8   = List(
    13,  2,  8, 4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7,
    1, 15, 13, 8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2,
    7, 11,  4, 1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8,
    2,  1, 14, 7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11)

  /* Data width */
  val blockWidth     = 64 bits
  val keyWidth       = 56 bits
  val keyWidthParity = 8  bits

  /* Number of Round used by DES */
  val nbrRound       = 16

  /* Used by the key scheduling */
  val oneShiftRound = List(1,2,9,16)  // 1 left rotation for round 1,2,9,16 and others 2 left shift rotation
}


/**
  * Define some usefull funtion
  */
object DESBlock{

  /** Permutation, Compression and expansion
    *  These functions permute a vector thanks to the table (!! The table is given for a software application !!)
    */
  def permutation(table:Seq[Int], vector:Bits): Bits = expansion(table.toList, vector)

  def compression(table:Seq[Int], vector:Bits): Bits = expansion(table.toList,vector)

  def expansion(table:List[Int], vector:Bits): Bits = Cat(table.reverse.map(index => vector(vector.getWidth - index)))
}


/**
  * Data Encryption Standard (DES)
  *
  *                      _________
  *                     |         |
  *    -- Plaintext --->|   DES   |-- Ciphertext -->
  *       (64 bits)     |_________|   (64 bits)
  *                          |
  *                      Key (56 bits)
  *                          |
  *                 Key + parity (64 bits)
  *
  *
  */
class DESBlock(g: DESBlockGenerics = DESBlockGenerics()) extends Component{

  val gIO  = SymmetricCryptoBlockGeneric(keyWidth    = g.keyWidth + g.keyWidthParity,
                                         blockWidth  = g.blockWidth,
                                         useEncDec   = true)

  val io = new SymmetricCryptoBlockIO(gIO)


  val roundNbr    = UInt(log2Up(g.nbrRound) + 1 bits)
  val lastRound   = io.cmd.enc ? (roundNbr === (g.nbrRound-2)) | (roundNbr === 2)
  val init        = io.cmd.valid.rise(False)
  val nextRound   = Reg(Bool) init(False) setWhen(init) clearWhen(lastRound)
  val rspValid    = Reg(Bool) init(False) setWhen(lastRound) clearWhen(init)


  /**
    * Count the number of round
    *   - Encryption 0 -> 15
    *   - Decryption 16 -> 1
    */
  val ctnRound = new Area{
    val round = Reg(UInt(log2Up(g.nbrRound) + 1 bits))

    when(init){
      round := io.cmd.enc ? U(0) | g.nbrRound
    }

    when(nextRound){
      round := io.cmd.enc ? (round + 1) | (round - 1)
    }
  }

  roundNbr := ctnRound.round


  /**
    * Initial permutation
    */
  val initialBlockPermutation = new Area{
    val perm = DESBlock.permutation(g.initialPermutation, io.cmd.block)
  }


  /**
    * Key scheduling
    *   For encryption :
    *                          Key 64 bits
    *                              |
    *                   -----------------------
    *                  |       Parity drop     |   (remove 8 bits => 56 bits)
    *                   -----------------------
    *                     |                 |       (2 x 28 bits)
    *               ------------     ------------
    *              | Shift left |   | Shift left |  Round key Generator 1
    *               ------------     ------------
    *                  |    |          |      |             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    *                  |   --------------     |             !!!  Shifting : 1 shift left for round 1,2,9,16,  !!!
    * K1 (48 bits)  <--|--| compression  |    |             !!!    others rounds 2 shift left                 !!!
    *                  |   --------------     |             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    *                 ...       ....         ...
    *               ------------     ------------
    *              | Shift left |   | Shift left | Round key Generator 16
    *               ------------     ------------
    *                       |          |
    *                      --------------
    * K16 (48bits) <------| compression  |
    *                      --------------
    *
    * For decryption :
    *     Replace the Shift left by a shift right
    */
  val keyScheduling = new Area{

    val shiftKey   = Reg(Bits(g.keyWidth))

    // parity drop : 64bits -> 56 bits
    when(init){ shiftKey := DESBlock.compression(g.pc_1, io.cmd.key) }

    // rotate the key (left for encryption and right for decryption)(key is divided into two groups of 28 bits)
    val shiftRes   = Bits(g.keyWidth)

    when(g.oneShiftRound.map(index => ctnRound.round === (index-1)).reduce(_ || _) ){
      when(io.cmd.enc){
        shiftRes  := shiftKey(55 downto 28).rotateLeft(1) ## shiftKey(27 downto 0).rotateLeft(1)
      }otherwise{
        shiftRes  := shiftKey(55 downto 28).rotateRight(1) ## shiftKey(27 downto 0).rotateRight(1)
      }
    }otherwise{
      when(io.cmd.enc){
        shiftRes  := shiftKey(55 downto 28).rotateLeft(2) ## shiftKey(27 downto 0).rotateLeft(2)
      }otherwise{

        shiftRes  := shiftKey(55 downto 28).rotateRight(2) ## shiftKey(27 downto 0).rotateRight(2)

        when(ctnRound.round === g.nbrRound){
          shiftRes  := shiftKey
        }
      }
    }

    // update key shift
    when(nextRound){ shiftKey := shiftRes }

    // compression : (56bits -> 48 bits)
    val keyRound = DESBlock.compression(g.pc_2, shiftRes)
  }


  /**
    * DES function
    *            In 32 bits
    *                |
    *       ---------------------
    *      |     Expansion       | (32 -> 48bits)
    *       ---------------------
    *                |
    *               XOR <--------------- Ki (48 bits)
    *                |
    *      ----   ---        ---
    *     | S1 |-| S2 |-...-| S8 | (sBox)
    *      ----   ---        ---
    *                | (32 bits)
    *       ----------------------
    *      |     Permutation      |
    *       ----------------------
    *                |
    *             Out 32 bits
    */
  val funcDES = new Area{

    // list of SBox ROM 1 to 8
    val sBox     = List(Mem(Bits(4 bits), g.sBox_8.map(B(_, 4 bits))),
                        Mem(Bits(4 bits), g.sBox_7.map(B(_, 4 bits))),
                        Mem(Bits(4 bits), g.sBox_6.map(B(_, 4 bits))),
                        Mem(Bits(4 bits), g.sBox_5.map(B(_, 4 bits))),
                        Mem(Bits(4 bits), g.sBox_4.map(B(_, 4 bits))),
                        Mem(Bits(4 bits), g.sBox_3.map(B(_, 4 bits))),
                        Mem(Bits(4 bits), g.sBox_2.map(B(_, 4 bits))),
                        Mem(Bits(4 bits), g.sBox_1.map(B(_, 4 bits))))

    val rightRound   = Bits(32 bits) // set in feistelNetwork Area

    // xor the key with the right block expanded(32 bits -> 48 bits)
    val xorRes = keyScheduling.keyRound ^ DESBlock.expansion(g.expansion, rightRound)

    // sBox stage
    val boxRes   = Bits(32 bits)
    for(i <- 0 until sBox.size){
      val addrSBox = xorRes(i*6+6-1 downto i*6)
      boxRes(i*4+4-1 downto i*4) := sBox(i).readAsync( (addrSBox(5) ## addrSBox(0) ## addrSBox(4 downto 1)).asUInt )
    }

    // fixed permutation
    val rResult = DESBlock.permutation(g.fixedPermutation, boxRes)
  }


  /**
    * Feistel network
    *
    *    --------------------------------
    *   |   Li-1        |      Ri-1      |  (2 x 32 bits) (inBlock)
    *    --------------------------------
    *        |                     |
    *       XOR<---(Des function)--|
    *        |          \__________|_______ Ki
    *        |                     |
    *        \       ____________ /
    *         \_____/___________
    *              /            \
    *    --------------------------------
    *   |   Li          |      Ri        | (2 x 32 bits) (outBlock)
    *    --------------------------------
    */
  val feistelNetwork = new Area{

    val inBlock  = Reg(Bits(g.blockWidth))

    val outBlock = inBlock(31 downto 0) ## (inBlock(63 downto 32) ^ funcDES.rResult)

    when(init){ inBlock := initialBlockPermutation.perm }
    when(nextRound){ inBlock := outBlock }
  }

  funcDES.rightRound  := feistelNetwork.inBlock(31 downto 0)


  /**
    * Final Permutation of the Block
    *    ( swap outBlock in order to have the same feistel network for each round )
    */
  val finalBlockPermutation = new Area{
    val perm = DESBlock.permutation(g.finalPermutation, feistelNetwork.outBlock(31 downto 0) ## feistelNetwork.outBlock(63 downto 32) )
  }


  /*
   * Update the output
   */
  val cmdReady  = RegNext(rspValid.rise())
  io.rsp.block := RegNext(finalBlockPermutation.perm)
  io.rsp.valid := cmdReady

  io.cmd.ready  := cmdReady
}
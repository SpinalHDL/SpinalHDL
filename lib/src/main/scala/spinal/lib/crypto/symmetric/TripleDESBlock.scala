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
import spinal.lib.fsm._


/**
  * Triple DES (3DES)
  *
  *           Encrpytion :                Decrytpion :
  *
  *           plaintext                   plaintext      (64 bits)
  *               |                           |
  *       -----------------            ----------------
  *      |   DES encrypt   |<-- K1 -->|  DES decrypt   |
  *       -----------------            ----------------
  *               |                           |
  *       -----------------            ----------------
  *      |   DES decrypt   |<-- K2 -->|  DES encrypt   |
  *       -----------------            ----------------
  *               |                           |
  *       -----------------            ----------------
  *      |   DES encrypt   |<-- K3 -->|  DES decrypt   |
  *       -----------------            ----------------
  *               |                           |
  *           ciphertext                   ciphertext      (64 bits)
  *
  *
  *    key = Concatenation(k1 , k2 , k3) = 3*64 bits = 192 bits
  *
  */
class TripleDESBlock() extends Component{

  val gDES = DESBlockGenerics()
  val gIO  = SymmetricCryptoBlockGeneric(keyWidth    = ((gDES.keyWidth.value + gDES.keyWidthParity.value) * 3) bits,
                                         blockWidth  = gDES.blockWidth,
                                         useEncDec   = true)

  val io = new SymmetricCryptoBlockIO(gIO)

  val block    = Reg(Bits(gDES.blockWidth))

  val blockDES = new DESBlock()

  /**
    * Triple DES state machine
    */
  val sm3DES = new StateMachine{

    val desCmdValid = False
    val desEncDec   = False
    val desKey      = B(0, gDES.keyWidthParity + gDES.keyWidth)
    val inSel       = False
    val cmdReady    = False

    val sIdle: State = new State with EntryPoint{
      whenIsActive{
        when(io.cmd.valid && !io.cmd.ready){
          goto(sStage1)
        }
      }
    }

    val sStage1: State = new State{
      whenIsActive{
        desEncDec   := io.cmd.enc
        desCmdValid := True
        desKey      := io.cmd.enc ? io.cmd.key(191 downto 128) | io.cmd.key(63 downto 0)

        when(blockDES.io.rsp.valid){
          desCmdValid := False
          block       := blockDES.io.rsp.block
          goto(sStage2)
        }
      }
    }

    val sStage2: State = new State{
      whenIsActive{
        inSel       := True
        desEncDec   := !io.cmd.enc
        desKey      := io.cmd.key(127 downto 64)
        desCmdValid := True

        when(blockDES.io.rsp.valid){
          desCmdValid := False
          block       := blockDES.io.rsp.block
          goto(sStage3)
        }
      }
    }

    val sStage3: State = new State{
      whenIsActive{
        inSel       := True
        desEncDec   := io.cmd.enc
        desKey      := io.cmd.enc ? io.cmd.key(63 downto 0) | io.cmd.key(191 downto 128)
        desCmdValid := True

        when(blockDES.io.rsp.valid){
          desCmdValid := False
          cmdReady    := True
          block       := blockDES.io.rsp.block
          goto(sIdle)
        }
      }
    }
  }


  /*
   * Des block connection
   */
  blockDES.io.cmd.valid  <> sm3DES.desCmdValid
  blockDES.io.cmd.key    <> sm3DES.desKey
  blockDES.io.cmd.enc    <> sm3DES.desEncDec
  blockDES.io.cmd.block  <> (sm3DES.inSel ? block | io.cmd.block)


  /*
   * Output
   */
  val cmdReady = RegNext(sm3DES.cmdReady, False)
  io.rsp.block := block
  io.rsp.valid := cmdReady
  io.cmd.ready := cmdReady
}




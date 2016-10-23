/******************************************************************************
  * Triple DES (3DES)
  *
  *           Encrpytion :                Decrytpion :
  *
  *           plaintext                   ciphertext      (64 bits)
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
  *           ciphertext                   plaintext      (64 bits)
  *
  *
  *    key = Concatenation(k1 , k2 , k3) = 3*64 bits = 192 bits
  *
  */

package spinal.lib.crypto.symmetric

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


case class TripleDESBlockCmd(g : DESBlockGenerics) extends Bundle{
  val block  = Bits(g.blockWidth)
  val key    = Bits(3 * (g.keyWidth + g.keyWidthParity))
  val encDec = Bool // enc=1, dec=0
}


case class TripleDESBlockRsp(g : DESBlockGenerics) extends Bundle{
  val block = Bits(g.blockWidth)
}


class TripleDESBlock() extends Component{

  val g = DESBlockGenerics()

  val io = new Bundle{
    val cmd = slave Stream(TripleDESBlockCmd(g))
    val rsp = master Flow(TripleDESBlockRsp(g))
  }

  val block    = Reg(Bits(64 bits))
  val rspValid = Reg(Bool) init(False)

  // DES Block
  val blockDES = new DESBlock()


  /**
    * Triple DES state machine
    */
  val sm3DES = new StateMachine{

    val desCmdValid = False
    val desEncDec   = False
    val desKey      = B(0, 64 bits)
    val inSel       = False
    val cmdReady    = False

    val sIdle : State = new State with EntryPoint{
      whenIsActive{
        when(io.cmd.valid){
          rspValid.clear()
          goto(sStage1)
        }
      }
    }

    val sStage1 : State = new State{
      whenIsActive{
        desEncDec   := io.cmd.encDec ? True | False
        desCmdValid := True
        desKey      := io.cmd.key(191 downto 128)

        when(blockDES.io.rsp.valid){
          desCmdValid := False
          block       := blockDES.io.rsp.block
          goto(sStage2)
        }
      }
    }

    val sStage2 : State = new State{
      whenIsActive{
        inSel       := True
        desEncDec   := io.cmd.encDec ? False | True
        desKey      := io.cmd.key(127 downto 64)
        desCmdValid := True

        when(blockDES.io.rsp.valid){
          desCmdValid := False
          block       := blockDES.io.rsp.block
          goto(sStage3)
        }
      }
    }

    val sStage3 : State = new State{
      whenIsActive{
        inSel       := True
        desEncDec   := io.cmd.encDec ? True | False
        desKey      := io.cmd.key(63 downto 0)
        desCmdValid := True

        when(blockDES.io.rsp.valid){
          desCmdValid := False
          cmdReady    := True
          block       := blockDES.io.rsp.block
          rspValid.set()
          goto(sIdle)
        }
      }
    }
  }


  /**
    * Des block connection
    */
  blockDES.io.cmd.valid  <> sm3DES.desCmdValid
  blockDES.io.cmd.key    <> sm3DES.desKey
  blockDES.io.cmd.encDec <> sm3DES.desEncDec
  blockDES.io.cmd.block  <> (sm3DES.inSel ? block | io.cmd.block)



  /**
    * Output
    */
  io.rsp.valid := rspValid
  io.rsp.block := block
  io.cmd.ready := sm3DES.cmdReady

}




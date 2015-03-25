package spinal.lib.serdes

import spinal.core._
import spinal.lib._

object SerialLinkConst {
  def cData = b"01"
  def cClose = b"02"
  def cOpen = b"03"

  def chunkDataSizeMax = 32
  def bitsWidth = 8
}


object SerialLinkRxState extends SpinalEnum {
  val eType, eFromPtr, eToPtr, eData = Value
}


class SerialLinkRx extends Component {

  import SerialLinkConst._
  import SerialLinkRxState._

  val io = new Bundle {
    val input = slave Handshake Fragment(Bits(bitsWidth bit))
    val output = master Handshake Fragment(Bits(bitsWidth bit))
  }

  val softReset = RegInit(True)
  val state = RegInit(eType)
  when(io.input.valid) {
    val data = io.input.data
    switch(state) {
      is(eType) {
        switch(data){
          is(cClose){
            softReset := True
          }
          is(cOpen){
            softReset := False
          }
          is(cData){

          }
        }
      }
    }
  }


}
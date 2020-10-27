package spinal.lib.com.uart.sim

import spinal.core.sim._
import spinal.core.{Bool, assert}
import spinal.sim._

object UartDecoder {
  def apply(uartPin : Bool, baudPeriod : Long) = fork{
    sleep(1) //Wait boot signals propagation
    waitUntil(uartPin.toBoolean == true)

    while(true) {
      waitUntil(uartPin.toBoolean == false)
      sleep(baudPeriod/2)

      if(uartPin.toBoolean != false) {
        println("UART FRAME ERROR")
      }
      else {
        sleep(baudPeriod)

        var buffer = 0
        (0 to 7).foreach { bitId =>
          if (uartPin.toBoolean)
            buffer |= 1 << bitId
          sleep(baudPeriod)
        }

        if (uartPin.toBoolean != true) println("UART FRAME ERROR") else if (buffer.toChar != '\r') print(buffer.toChar)
      }
    }
  }
}

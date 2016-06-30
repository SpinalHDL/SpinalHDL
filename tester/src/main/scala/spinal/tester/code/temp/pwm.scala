package spinal.tester.temp

import spinal.core._

class PwmModulator extends Component {
    val io = new Bundle {
        val duty = in UInt(8 bits);
        val pwm = out Bool;
    }
    
    val counter = Reg(UInt(8 bits));
    counter := counter + 1;    
    io.pwm := RegNext(counter < io.duty); 
}

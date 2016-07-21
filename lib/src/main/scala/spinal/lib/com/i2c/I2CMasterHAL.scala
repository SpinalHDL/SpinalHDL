/******************************************************************************
  * I2C Master HAL
  *                ________                       _______
  *               |        |<------- I2C ------->|       |
  *               | Master |                     | Slave |
  *  CMD Stream ->|________|-> RSP Flow          |_______|
  *
  * Write sequence :
  *
  *   CMD    : START   WRITE           WRITE         STOP
  *   Master :   | START | WRITE |     | WRITE |     | STOP |
  *   Slave  :   |       |       | ACK |       | ACK |      |
  *   RSP    :                  DATA  ACK     DATA  ACK
  *
  * Read sequence :
  *
  *   CMD    : START   READ     ACK   READ   NACK   STOP
  *   Master :   | START |      | ACK |      | NACK | STOP |
  *   Slave  :   |       | READ |     | READ |      |      |
  *   RSP    :                DATA        DATA
  *
  * Restart sequence :
  *
  *   CMD    : START   READ   NACK   RESTART  WRITE         STOP
  *   Master :   | START |      | NACK | START | WRITE |     | STOP |
  *   Slave  :   |       | READ |      |       |       | ACK |      |
  *   RSP    :                 DATA                   DATA  ACK
  */

// @TOTO Synch/Filter inputs signals..

package spinal.lib.com.i2c


import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


/**
  * Global configuration of the I2C Master
  *
  * @param dataWidth                 : Width of the data send
  * @param samplingSize              : Number of sampler to generate a bit
  * @param clockDividerSamplingWidth : Width of the clockDivider value
  * @param clockDividerSCLWidth      : Width of the clockDivider value
  */
case class I2CMasterHALGenerics(dataWidth                 : Int =  8,
                                samplingSize              : Int = 3,
                                clockDividerSamplingWidth : Int = 10,
                                clockDividerSCLWidth      : Int = 20){}


/**
  * Runtime configuartion of the I2C master
  */
case class I2CMasterHALConfig(g: I2CMasterHALGenerics) extends Bundle {

  val clockDividerSampling = UInt(g.clockDividerSamplingWidth bit)
  val clockDividerSCL      = UInt (g.clockDividerSCLWidth bits)
  val enCollision          = Bool

  def setSCLFrequency(sclFrequency : Double, clkFrequency : Double = ClockDomain.current.frequency.getValue) : Unit = {
    clockDividerSCL := (clkFrequency / sclFrequency * 2).toInt
  }

  def setClockDividerSampling(divider : Int): Unit = {
    clockDividerSampling := divider
  }
}


/**
  * Modes used to manage the master
  */
object I2CMasterHALCmdMode extends SpinalEnum{
  val START, WRITE, READ, ACK, NACK, STOP = newElement()
}


/**
  * Define the command interface
  */
case class I2CMasteHALCmd(g : I2CMasterHALGenerics) extends Bundle{
  val mode   = I2CMasterHALCmdMode()
  val data   = Bits(g.dataWidth bits)
}


/**
  * 4 different modes are available for a response
  *    ACK       : ACK received after writting
  *    NACK      : NACK recieved after writting
  *    DATA      : Data read on the bus
  *    COLLISION : Collision detected during a write
  */
object I2CMasterHALRspMode extends SpinalEnum{
  val ACK, NACK, DATA, COLLISION = newElement()
}


/**
  * Define the response interface
  */
case class I2CMasterHALRsp(g : I2CMasterHALGenerics) extends Bundle{
  val mode  = I2CMasterHALRspMode()
  val data  = Bits(g.dataWidth bits)
}


/**
  * Define the interface of the I2C Master HAL
  */
case class I2CMasterHALio(g : I2CMasterHALGenerics) extends Bundle{
  val i2c    = master( I2C() )
  val config = in( I2CMasterHALConfig(g) )
  val cmd    = slave  Stream( I2CMasteHALCmd(g)  )
  val rsp    = master Flow  ( I2CMasterHALRsp(g) )
}


/**
  * Definition of the component I2C Master HAL
  */
class I2CMasterHAL(g : I2CMasterHALGenerics) extends Component {

  import spinal.lib.com.i2c.{I2CMasterHALRspMode => RspMode}
  import spinal.lib.com.i2c.{I2CMasterHALCmdMode => CmdMode}


  val io = I2CMasterHALio(g)


  /**
    * Synchronize input's signals of the I2C
    */
 /* val ccIO = new Area{
    val rd_scl = BufferCC(io.i2c.scl.read)
    val rd_sda = BufferCC(io.i2c.sda.read)
  }*/


  /**
    * Clock divider used to sample sda and scl
    */
  val samplingClockDivider = new Area{

    val counter = Reg(UInt(g.clockDividerSamplingWidth bits)) init(0)
    val tick    = counter === 0

    counter := counter - 1
    when(tick){ counter := io.config.clockDividerSampling }
  }


  /**
    * Sample sda and scl
    */
  val sampler = new Area{

    val scl     = BufferCC(io.i2c.scl.read)
    val sda     = BufferCC(io.i2c.sda.read)

    val sdaSamples = History(that=sda, length=g.samplingSize, when=samplingClockDivider.tick, init=True)
    val sclSamples = History(that=scl, length=g.samplingSize, when=samplingClockDivider.tick, init=True)

    val rd_sda        = RegNext(MajorityVote(sdaSamples))
    val rd_scl        = RegNext(MajorityVote(sclSamples))
  }


  /**
    * Rising and falling edge of the scl signal detection
    */
  val sclSampling = new Area{

    val scl_prev = RegNext(sampler.rd_scl)

    val risingEdge  = sampler.rd_scl  && !scl_prev
    val fallingEdge = !sampler.rd_scl && scl_prev
  }


  /**
    * Generate and manage the scl clock
    */
  val sclGenerator = new Area{

    val cntValue        = Reg(UInt(g.clockDividerSCLWidth bits)) init(0)
    val scl             = RegInit(True)
    val scl_en          = Bool // set in the state machine "smMaster"
    val masterFreeze    = Bool // set in the state machine "smMaster"
    val stopDetected    = Bool // set in the area "detector"

    // detect if the slave freeze the bus
    val slaveFreeze = scl && !sampler.rd_scl

    // start / stop the counter clock
    when(scl_en && !masterFreeze && !slaveFreeze){

      cntValue := cntValue + 1
      when(cntValue >= io.config.clockDividerSCL ){ cntValue := 0 }

    }.elsewhen(stopDetected){
      cntValue := 0
    }

    // SCL generation
    when(cntValue === io.config.clockDividerSCL){ scl := !scl }

    // Used to indicate when to generate the start/restart/stop sequence
    val triggerStartStop = scl && cntValue >= (io.config.clockDividerSCL >> 1)
  }


  /**
    * Detect the start/restart and the stop sequence
    */
  val detector = new Area{

    val sda_prev = RegNext(sampler.rd_sda)

    val start = sclGenerator.triggerStartStop && sampler.rd_scl && !sampler.rd_sda  && sda_prev
    val stop  = sclGenerator.triggerStartStop && sampler.rd_scl && sampler.rd_sda   && !sda_prev

    sclGenerator.stopDetected := stop
  }


  /**
    * Once a start is detected
    */
  val busState = new Area{
    val busy = Reg(Bool) init(False)

    when(detector.start){ busy.set()  }
    when(detector.stop) { busy.clear()}
  }


  /**
    * Counter of bit write/read. MSB is send first on the I2C bus
    * so counter goes from dataWdith to 0
    */
  val bitCounter = new Area{
    val index  = Reg(UInt( log2Up(g.dataWidth) bits )) randBoot()
    val isDone = False
    def clear() : Unit = index := g.dataWidth - 1

    when(sclSampling.fallingEdge) {
      index  := index - 1
      isDone := index === 0
    }
  }


  /**
    * Main state machine of the Master HAL
    */
  val smMaster = new StateMachine{

    val dataReceived   = Reg(Bits(g.dataWidth bits)) randBoot()

    val wr_sda = True

    // default value
    sclGenerator.masterFreeze := False
    sclGenerator.scl_en       := False
    io.cmd.ready := False
    io.rsp.valid := False
    io.rsp.mode  := RspMode.ACK
    io.rsp.data  := dataReceived

    always{
      when(io.cmd.valid && io.cmd.mode === CmdMode.STOP){
        io.cmd.ready := True
        goto(sStop)
      }

      when(io.cmd.valid && io.cmd.mode === CmdMode.START && !busState.busy ){
        io.cmd.ready   := True
        goto(sStart)
      }
    }

    val sIdle : State = new State with EntryPoint {
      whenIsActive {
        // nothing to do
      }
    }

    val sStart : State = new State {
      whenIsActive{
        sclGenerator.scl_en := True
        wr_sda := !sclGenerator.triggerStartStop

        // end of the stop sequence
        when(sclSampling.fallingEdge){ goto(sData) }
      }
    }

    val sData : State = new State{
      onEntry{
        bitCounter.clear()
      }
      whenIsActive{
        sclGenerator.scl_en := True
        sclGenerator.masterFreeze := !io.cmd.valid

        // write data and check collision
        when(io.cmd.mode === CmdMode.WRITE){
          wr_sda := io.cmd.data(bitCounter.index)

          when(sampler.rd_sda =/= wr_sda && sclSampling.risingEdge && io.config.enCollision){
            io.rsp.mode  := RspMode.COLLISION
            io.rsp.valid := True
            goto(sIdle)
          }
        }

        // Read data
        when(sclSampling.risingEdge){ dataReceived(bitCounter.index) := sampler.rd_sda }

        // data sequence is done ?
        when(bitCounter.isDone){
          io.rsp.mode  := RspMode.DATA
          io.rsp.valid := True
          io.cmd.ready := !(io.cmd.mode === CmdMode.WRITE)
          goto(sACK)
        }
      }
    }

    val sACK : State = new State{
      whenIsActive{
        sclGenerator.scl_en       := True
        sclGenerator.masterFreeze := !io.cmd.valid

        // write ACK
        wr_sda := !(io.cmd.mode === CmdMode.ACK)

        // read ACK
        when(sclSampling.risingEdge){
          io.rsp.mode  := (sampler.rd_sda) ? RspMode.NACK | RspMode.ACK
          io.rsp.valid := True
        }

        // end of the ACK sequence ?
        when(sclSampling.fallingEdge){
          io.cmd.ready := True
          goto(sData)
        }
      }
    }

    val sStop : State = new State {
      whenIsActive{
        sclGenerator.scl_en := True
        wr_sda := False
        when(sclGenerator.triggerStartStop){ goto(sIdle) }
      }
    }
  }

  io.i2c.sda.write := RegNext(smMaster.wr_sda)
  io.i2c.scl.write := RegNext(sclGenerator.scl)
}
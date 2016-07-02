/******************************************************************************
  * I2C Master HAL
  *
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
  * @param dataWidth         : Width of the data send
  * @param clockDividerWidth : Width of the clockDivider value
  * @param multiMaster_en    : Multi-Master or Single-Master
  */
case class I2CMasterHALGenerics(dataWidth         : Int =  8,
                                clockDividerWidth : Int = 20,
                                multiMaster_en    : Boolean = true){}


/**
  * Used to config the I2C master HAL at runtime
  */
case class I2CMasterHALConfig(g: I2CMasterHALGenerics) extends Bundle {

  val clockDivider = UInt (g.clockDividerWidth bits)

  def setFrequency(sclFrequency : Double, clkFrequency : Double = ClockDomain.current.frequency.getValue) : Unit = {
    clockDivider := (clkFrequency / sclFrequency * 2).toInt
  }
}


/**
  * 6 different modes can be used to manage the master
  *    START      : Send the start/restart sequence
  *    WRITE      : Write a data
  *    READ       : Read a data
  *    ACK        : Send an ACK after reading
  *    NACK       : Send a NACK after reading
  *    STOP       : Send the stop sequence
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

  // Enable/Disable the scl signal
  val scl_en     = Bool
  // freeze the scl for clock synchronization
  val scl_freeze = Bool
  val sclFreezeByMater = Bool


  /**
    * Generate and manage the scl clock,  signals to indicate the
    * rising and falling edge of SCL as well as a signal to indicate
    * when to execute a start/stop/restart operation
    */
  val sclGenerator = new Area{

    val cntValue        = Reg(UInt(g.clockDividerWidth bits)) init(0)
    val risingEdge      = False
    val fallingEdge     = False
    val triggerSequence = False
    val scl             = RegInit(True)


    // start / stop the counter clock
    when(scl_en && !scl_freeze){

      cntValue := cntValue + 1
      when(cntValue >= io.config.clockDivider ){
        cntValue := 0
      }

    }otherwise{
      when(sclFreezeByMater){ scl := False}otherwise{scl := True}

      cntValue := 0
    }

    // Generate the scl signal
    when(cntValue === io.config.clockDivider){
      scl := !scl

      // detect rising and falling edge
      when(scl){
        fallingEdge := True
      }otherwise{
        risingEdge  := True
      }
    }

    // Used to indicate when to generate the start/restart/stop sequence
    when(scl){
      when(cntValue === (io.config.clockDivider >> 1)){
        triggerSequence := True
      }
    }
  }


  /**
    * Detect the start/restart and the stop sequence
    */
  val detector = new Area{

    val start   = False
    val stop    = False

    val sda_cur  = RegNext(io.i2c.sda.read) init(False)
    val sda_prev = RegNext(sda_cur)     init(False)

    // start = falling edge of sda while the scl is 1
    when(sclGenerator.scl && sda_cur === False && sda_prev ){
      start   := True
    }

    // stop = rising edge of sda while the scl is 1
    when(sclGenerator.scl && sda_cur && sda_prev === False ){
      stop := True
    }
  }



  val state_Bus = new StateMachine{

    val busy = False

    val sIDLE : State = new State with EntryPoint{
      whenIsActive {
        when(detector.start){
          goto(sDELAY)
        }
      }
      // @TODO cyclesCounter must be : (io.config.clockDivider >> 1).toInt
      val sDELAY : State = new StateDelay(cyclesCount=40 ){
        whenCompleted{
          goto(sBUSY)
        }
      }

      val sBUSY : State = new State{
        whenIsActive{
          busy := True
          when(detector.stop){
            goto(sIDLE)
          }
        }
      }
    }
  }




  /**
    * State machine which synchronize all SCL signals of the different master
    * in the case when several master drive the SCL.
    */
  val smSynchSCL = new StateMachine{

    val freezeSCL = RegInit(False)

    val sIDLE : State = new State with EntryPoint{
      whenIsActive{
        when(scl_en){ goto(sMONITOR) }
      }
    }

    val sMONITOR : State = new State {
      whenIsActive{
        when(sclGenerator.scl && io.i2c.scl.read === False){
          freezeSCL := True
        }
        when(sclGenerator.scl && io.i2c.scl.read){
          freezeSCL := False
        }

       // when(sclGenerator.scl && io.i2c.scl.read){
       //   freezeSCL := False
       // }
        when(scl_en === False){ goto(sIDLE) }
      }
    }
  }


  /**
    * Counter of bit write/read. MSB is send first on the I2C bus
    * so counter goes from dataWdith to 0
    */
  val counterBit = new Area{
    val index = Reg(UInt( log2Up(g.dataWidth) bits )) init(g.dataWidth-1)
    def isDone()    : Bool = index === 0
    def clear()     : Unit = index := g.dataWidth-1

    when(sclGenerator.risingEdge) {
      index := index - 1
    }
  }

  /**
    * Main state machine of the Master HAL
    */
  val smMaster = new StateMachine{

    val scl_en         = Reg(Bool) init(False)
    val wr_sda         = RegInit(True)
    @dontName  val rd_sda         =  io.i2c.sda.read   // if (g.multiMaster_en ) io.i2c.sda.read else null
    val dataReceived   = Reg(Bits(g.dataWidth bits)) init(0)
    val isStarted      = RegInit(False)
    val freezeBus      = RegInit(False)
    val isCMDAfterFreeze = RegInit(False)

    when(io.cmd.valid){ freezeBus := False }

    // default value
    io.cmd.ready := False
    io.rsp.valid := False
    io.rsp.mode  := RspMode.ACK
    io.rsp.data  := 0

    val sIDLE : State = new State with EntryPoint {
      whenIsActive {
        when(freezeBus === False){

          when(!((!isStarted) && state_Bus.busy)){

          when(io.cmd.valid) {
            switch(io.cmd.mode) {
              is(CmdMode.START) {
                io.cmd.ready := True
                when(isStarted === False) {
                  isStarted := True
                  goto(sSTART)
                } otherwise {
                  goto(sWAIT_BEFORE_START)
                }
              }
              is(CmdMode.STOP) {
                io.cmd.ready := True
                isStarted    := False
                goto(sWAIT_BEFORE_STOP)
              }
              is(CmdMode.READ) {
                io.cmd.ready := True
                counterBit.clear()
                goto(sWAIT_BEFORE_READ)
              }
              is(CmdMode.WRITE) {
                counterBit.clear()
                goto(sWRITE)
              }
              is(CmdMode.NACK, CmdMode.ACK) {
                goto(sWR_ACK)
              }
            }
          }

          }
        }

        when(sclGenerator.fallingEdge){
          freezeBus := True
          isCMDAfterFreeze := True
        }
      }
    }

    val sSTART : State = new State {
      whenIsActive{
        scl_en := True
        when(sclGenerator.triggerSequence){
          wr_sda := False
          goto(sIDLE)
        }
      }
    }

    val sREAD = new StateFsm(fsm=readSM(io.i2c.sda.read, dataReceived)){
      whenCompleted{
        goto(sNOTIFIY_NEW_DATA_R)
      }
    }

    val sWRITE = new StateParallelFsm (writeSM(wr_sda,io.cmd.data, isCMDAfterFreeze, rd_sda), readSM(io.i2c.sda.read, dataReceived)){
      whenCompleted{
        io.cmd.ready := True
        goto(sNOTIFIY_NEW_DATA_W)
      }
    }

    val sNOTIFIY_NEW_DATA_R : State = new State{
      whenIsActive{
        io.rsp.mode  := RspMode.DATA
        io.rsp.valid := True
        io.rsp.data  := dataReceived
        goto(sIDLE)
      }
    }

    val sNOTIFIY_NEW_DATA_W : State = new State{
      whenIsActive{
      io.rsp.mode  := RspMode.DATA
      io.rsp.valid := True
      io.rsp.data  := dataReceived
      goto(sRD_ACK)
      }
    }

    val sRD_ACK : State = new State {
      whenIsActive{
        when(sclGenerator.fallingEdge){
          wr_sda := True
        }
        when(sclGenerator.risingEdge){

          io.rsp.mode  := io.i2c.sda.read ? RspMode.NACK | RspMode.ACK
          io.rsp.valid := True
          io.rsp.data  := 0

          goto(sIDLE)
        }
      }
    }

    val sWR_ACK : State = new State {
      whenIsActive{
        when(isCMDAfterFreeze){
          isCMDAfterFreeze := False
          wr_sda := (io.cmd.mode === CmdMode.ACK) ? I2C.ACK | I2C.NACK
        }
        when(sclGenerator.fallingEdge){
          wr_sda := (io.cmd.mode === CmdMode.ACK) ? I2C.ACK | I2C.NACK
        }
        when(sclGenerator.risingEdge){
          io.cmd.ready := True
          goto(sIDLE)
        }
      }
    }

    val sSTOP : State = new State {
      whenIsActive{
        when(sclGenerator.fallingEdge){
          wr_sda := False
        }
        when(sclGenerator.triggerSequence){
          scl_en := False
          wr_sda := True
          goto(sIDLE)
        }
      }
    }

    val sWAIT_BEFORE_STOP : State = new State{
      whenIsActive{
        when(isCMDAfterFreeze){
          isCMDAfterFreeze := False
          wr_sda := False
          goto(sSTOP)
        }
        when (sclGenerator.triggerSequence){
          goto(sSTOP)
        }
      }
    }

    val sWAIT_BEFORE_READ : State = new State{
      whenIsActive{
        when(isCMDAfterFreeze){
          isCMDAfterFreeze := False
          wr_sda := True
          goto(sREAD)
        }
        when(sclGenerator.fallingEdge){
          wr_sda := True
          goto(sREAD)
        }
      }
    }

    val sWAIT_BEFORE_START : State = new State{
      whenIsActive{
        when(sclGenerator.fallingEdge){
          wr_sda := True
          goto(sSTART)
        }
      }
    }
  }


  /**
    * Write a data on the I2C
    *
    * @param wr_sda    : The write signal of the sda
    * @param data2Send : Data that will be sent on the I2C
    * @param rd_sda    : If not null, the data write will be read to check if collision exist on the bus (multi master)
    */
  def writeSM(wr_sda : Bool, data2Send:Bits, cmdAfterFreeze:Bool, rd_sda:Bool=null) = new StateMachine {

    val sWRITE: State = new State with EntryPoint {
      whenIsActive{
        when(cmdAfterFreeze){
          cmdAfterFreeze := False
          wr_sda := data2Send(counterBit.index)
        }
        when(sclGenerator.fallingEdge){
          wr_sda := data2Send(counterBit.index)
          if (rd_sda != null) goto(sCHECK_COLLISION)
          when(counterBit.isDone()){
            exit()
          }
        }
      }
    }

    val sCHECK_COLLISION = if(rd_sda != null) new State{
      whenIsActive{
        when(sclGenerator.risingEdge){
          when(wr_sda === rd_sda){
            goto(sWRITE)
          }otherwise{
            io.rsp.data  := 0
            io.rsp.mode  := RspMode.COLLISION
            io.rsp.valid := True
            exit()
          }
        }
      }
    }else null
  }


  /**
    * Read a data on the I2C bus
    *
    * @param sda           : The read signal of the sda
    * @param dataReceived  : Register that will contains the data receveid
    */
  def readSM(sda:Bool, dataReceived : Bits) = new StateMachine {
    val sREAD: State = new State with EntryPoint {
      whenIsActive{
        when(sclGenerator.risingEdge){
          dataReceived(counterBit.index) := sda
          when(counterBit.isDone()){
            exit()
          }
        }
      }
    }
  }


  io.i2c.sda.write := smMaster.wr_sda
  io.i2c.scl.write := sclGenerator.scl

  scl_en     := smMaster.scl_en


    scl_freeze := smSynchSCL.freezeSCL || smMaster.freezeBus
  sclFreezeByMater := smMaster.freezeBus

  //  scl_freeze := smMaster.freezeBus

}






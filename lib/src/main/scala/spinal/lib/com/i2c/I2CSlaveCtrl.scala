package spinal.lib.com.i2c


import spinal.core._
import spinal.lib._

/**
  * Define the different addresses modes
  */
trait ADDR_MODE { def value : Int }
case object ADDR_7bits  extends ADDR_MODE { def value : Int = 7  }
case object ADDR_10bits extends ADDR_MODE { def value : Int = 10 }


/**
  * Class to configure the I2C Slave
  */
case class I2CSlaveCtrConfig(addrDevice : BigInt,
                             modeAddr   : ADDR_MODE){

  assert(log2Up(addrDevice) <= modeAddr.value, "Address is bigger than the address width")

  def dataSize : Int = 8

  def addrDeviceMSB : UInt = modeAddr match{
    case ADDR_7bits  => U(addrDevice, 7 bits)
    case ADDR_10bits => (B"11110" ## B((addrDevice & 0x300l) >> 8, 2 bits)).asUInt
  }

  def addrDeviceLSB : UInt = U(addrDevice & 0xFFl, 8 bits)

  def isAddr10bits : Boolean  = modeAddr match {
    case ADDR_10bits => true
    case _           => false
  }
}


/**
  * Define all states of the state machine for the I2C controller
  */
object I2CCtrlSlaveState extends SpinalEnum {
  val IDLE, RD_ADDR_DEVICE, RD_RW, WR_ACK, CHECK_ADDR, CHECK_ADDR_LSB, WR_DATA, RD_DATA, RD_ACK = newElement()
}




/**
  * I2C : Slave controller
  */
class I2CSlaveCtrl(config: I2CSlaveCtrConfig) extends Component{

  val io = new Bundle{
    val i2c   = slave( I2C() )
    val read  = master Flow(Bits(config.dataSize bits))
    val write = slave  Stream(Bits(config.dataSize bits))
  }

  // defautl value of the payload
  io.read.payload := 0


  /**
    * Synchronize input's signals of the I2C
    */
  val ccIO = new Area{
    val i2c_scl = BufferCC(io.i2c.scl)
    val i2c_sda = BufferCC(io.i2c.sda.read)
  }


  /**
    * Rising and falling edge of the scl signal detection
    * @TODO oversampling the scl signals ???
    */
  val sclSampling = new Area{

    val risingEdge  = False
    val fallingEdge = False

    val scl_cur  = RegNext(ccIO.i2c_scl) init(False)
    val scl_prev = RegNext(scl_cur)      init(False)

    when(scl_cur && scl_prev === False){
      risingEdge := True
    }

    when(scl_cur === False && scl_prev){
      fallingEdge := True
    }
  }



  /**
    * Detect the start and the stop conditoin
    */
  val detector = new Area{

    val start   = False
    val stop    = False

    val sda_cur  = RegNext(ccIO.i2c_sda) init(False)
    val sda_prev = RegNext(sda_cur)      init(False)

    // start = falling edge of sda while the scl is 1
    when(sclSampling.scl_cur && sclSampling.scl_prev && sda_cur === False && sda_prev ){
      start   := True
    }

    // stop = rising edge of sda while the scl is 1
    when(sclSampling.scl_cur && sclSampling.scl_prev && sda_cur && sda_prev === False ){
      stop := True
    }
  }


  /**
    * Index counter (MSB is send frist )
    */
  val bitCounter = new Area {

    val index = Reg(UInt(log2Up(config.dataSize) bits)) init(config.dataSize-1)

    def clear() = index := config.dataSize-1
    def isOver : Bool = index === 0

    when(sclSampling.risingEdge) {
      index := index - 1
    }
  }


  /**
    * I2C slave state machine
    */
  val stateMachine = new Area{

    import I2CCtrlSlaveState._

    val state       = RegInit(IDLE)
    val dataShift   = Reg(Bits(config.dataSize bits)) init(0)
    val mode_rw     = Reg(Bool) init(False)
    val bit2Send    = Reg(Bool) init(True)
    val dataValid   = Reg(Bool) init(False)
    val readAddrLSB = if (config.isAddr10bits)  Reg(Bool) init(False) else null

    io.read.valid    := False
    io.write.ready   := False

    when(detector.stop){
      state := IDLE
    }

    switch(state){
      is(IDLE){
        bit2Send  := True
        dataValid := False

        if (config.isAddr10bits){
          readAddrLSB := False
        }

        when(detector.start){
          state     := RD_ADDR_DEVICE
          dataShift := 0
          bitCounter. clear()
        }
      }
      is(RD_ADDR_DEVICE){
        when(bitCounter.isOver) {
          state    := RD_RW
        }
        when(sclSampling.risingEdge){
          dataShift(bitCounter.index-1) := ccIO.i2c_sda
        }
      }
      is(RD_RW){
        when(sclSampling.risingEdge){
          mode_rw := ccIO.i2c_sda
          state   := CHECK_ADDR
        }
      }
      is(CHECK_ADDR){
        when(dataShift.asUInt === config.addrDeviceMSB){
          state := WR_ACK

          if (config.isAddr10bits){
            readAddrLSB := True
          }

        }otherwise{
          state := IDLE

          report(
            message   = "Address read doesn't match address device ",
            severity  = NOTE
          )

        }
      }
      if(config.isAddr10bits){
        is(CHECK_ADDR_LSB){

          when(dataShift.asUInt === config.addrDeviceLSB){
            state := WR_ACK

          }otherwise{
            state := IDLE

            report(
              message   = "Address read doesn't match address device ",
              severity  = NOTE
            )

          }
        }
      }
      is(WR_ACK){

        when(sclSampling.fallingEdge){

          bit2Send  := I2C.ACK
          state     := RD_ACK

          when(dataValid){
            io.read.payload := dataShift
            io.read.valid   := True
            dataValid       := False
          }
        }
      }
      is(RD_ACK){

        when(sclSampling.risingEdge){

          when(ccIO.i2c_sda === I2C.ACK){
            dataShift := 0
            bitCounter. clear()

            state := mode_rw ? WR_DATA | RD_DATA

            if (config.isAddr10bits){
              when(readAddrLSB){
                state := RD_DATA
              }
            }

          }otherwise{
            state := IDLE
          }

        }
      }
      is(WR_DATA){

        when(sclSampling.fallingEdge){
          bit2Send := True

          when(io.write.valid){
            bit2Send := io.write.payload(bitCounter.index)
          }otherwise{

            report(
              message   = "No data ready to be send",
              severity  = ERROR
            )
          }
        }

        when(sclSampling.risingEdge){
          when(bitCounter.isOver){
            io.write.ready := True
            state          := RD_ACK
          }
        }
      }
      is(RD_DATA){
        // remove the ACK on the line
        when(sclSampling.fallingEdge){
          bit2Send := True
        }

        when(sclSampling.risingEdge){

          dataShift(bitCounter.index) := ccIO.i2c_sda

          when(bitCounter.isOver){

            state     := WR_ACK
            dataValid := True

            if (config.isAddr10bits){
              when(readAddrLSB){
                readAddrLSB := False
                state       := CHECK_ADDR_LSB
                dataValid   := False
              }
            }
          }
        }
      }
    }
  }

  io.i2c.sda.write := stateMachine.bit2Send
}
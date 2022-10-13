package spinal.lib.bus.amba3.ahblite.sim.master

import spinal.lib.bus.amba3.ahblite.sim._
import spinal.lib.sim.protocolSim.master._

import spinal.core.sim._

import spinal.core.ClockDomain
import spinal.lib.bus.amba3.ahblite.AhbLite3

/** Abstraction to help driving a slave component with an AhbLite3 interface
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  *
  * @param bus
  *   the bus of the slave to control
  * @param cd
  *   the clock domain related to the bus
  */
case class AhbSlaveController(bus: AhbLite3, cd: ClockDomain) {
  // Section 2.1 Global signals
  def HCLK = cd.clockSim.toBoolean
  def HRESETn = cd.resetSim.toBoolean
  // Section 2.4 Decoder signals
  val HSEL = bus.HSEL
  // Section 2.2 Master signals
  val HADDR = bus.HADDR
  val HBURST = bus.HBURST
  val HMASTLOCK = bus.HMASTLOCK
  val HPROT = bus.HPROT
  val HSIZE = bus.HSIZE
  val HTRANS = bus.HTRANS
  val HWDATA = bus.HWDATA
  val HWRITE = bus.HWRITE
  // Section 2.5 Multiplexor signals
  // HRDATA from slave selected by the decoder
  val HREADYin = bus.HREADY
  // HRESP from slave selected by the decoder
  // Section 2.3 Slave signals
  def HRDATA = bus.HRDATA.toBigInt
  def HRESP = bus.HRESP.toBoolean
  def HREADYOUT = bus.HREADYOUT.toBoolean

  /** Assigns default address phase output values for simulation startup */
  def initAddrPhase(): Unit = {
    // Custom values
    HSEL #= false
    HADDR #= 0
    HBURST #= Hburst.SINGLE
    HMASTLOCK #= false
    HPROT #= 0
    HSIZE #= 0
    HTRANS #= Htrans.IDLE
    HWRITE #= false
  }

  // Custom value
  /** Assigns default data phase output values for simulation startup */
  def initDataPhase(): Unit = HWDATA #= 0

  // Represents the HREADYOUT of another slave (for Unready)
  private var _hreadyoutOther = true
  private def hreadyoutOther: Boolean = _hreadyoutOther
  private def hreadyoutOther_=(ready: Boolean): Unit = {
    _hreadyoutOther = ready
    HREADYin #= HREADY
  }

  // Section 4.2 Bus interconnection
  private var _dataPhaseHsel = false
  private def dataPhaseHsel: Boolean = _dataPhaseHsel
  private def dataPhaseHsel_=(hsel: Boolean): Unit = {
    _dataPhaseHsel = hsel
    HREADYin #= HREADY
  }
  // decoder to mux
  cd.onSamplings {
    if (HREADY)
      dataPhaseHsel = HSEL.toBoolean
  }
  // mux
  private def HREADY = if (dataPhaseHsel) HREADYOUT else hreadyoutOther
  forkSensitive(bus.HREADYOUT) { HREADYin #= HREADY }

  /** Assigns default output values for simulation startup */
  def init(): Unit = { HREADYin #= true; initAddrPhase(); initDataPhase() }

  trait AhbTransfer extends Transfer {
    def await(): Unit = if (!isDone) cd.waitActiveEdgeWhere(isDone)

    private var _duration = 0L
    private var _waitStates = 0L
    private var fallback: Option[AhbTransfer] = None

    /** Duration of the transfer, in clock cycles */
    final def duration = _duration

    /** Number of wait states inserted by the tested slave for this transaction
      * (address phase only)
      */
    final def waitStates = _waitStates

    /** If there is an error during data phase of this transaction, continue
      * with fallback instead of next transaction
      */
    final def onError(fallback: AhbTransfer): AhbTransfer = {
      this.fallback = Some(fallback)
      fallback
    }

    private def phaseBuilder(
        comb: => Unit,
        endCond: => Boolean,
        next: => Unit,
        onError: => Unit
    ): Unit = {
      // Section 3.1 Basic transfers
      comb
      cd.onNextSampling {
        _duration += 1
        if (HRESP == Hresp.ERROR)
          onError
        else if (endCond)
          next
        else
          phaseBuilder(comb, endCond, next, onError)
      }
    }

    // Section 3.1 Basic transfers
    final protected def addrPhase(): Unit = phaseBuilder(
      {
        initAddrPhase()
        addrPhaseComb()
      },
      addrPhaseEndCond,
      { bootNext(); if (!dropDataPhase) dataPhase() },
      { /* current dataPhase is booting another transfer */ }
    )
    final protected def dataPhase(): Unit = phaseBuilder(
      {
        initDataPhase()
        dataPhaseComb()
        if (!HREADYOUT)
          _waitStates += 1
      },
      HREADY,
      complete(),
      fallback match {
        case Some(t) => t.boot()
        case None    => bootNext()
      }
    )

    final def boot(): Unit = addrPhase()

    /** Action done when beginning an address phase clock cycle */
    protected def addrPhaseComb(): Unit

    /** Action done when beginning a data phase clock cycle */
    protected def dataPhaseComb(): Unit

    /** Condition to end transfer without starting dataPhase */
    protected def dropDataPhase: Boolean = false

    /** Condition to end address phase */
    protected def addrPhaseEndCond: Boolean = HREADY

    /** How to boot next transaction */
    protected def bootNext(): Unit = bootNextOr(Blank())

    /** Action done at the end of the successful transfer */
    protected def complete(): Unit
  }

  /** Base class for AHB transfers
    *
    * Using it directly may lead to incorrect transfers.
    *
    * @see
    *   Read
    * @see
    *   Write
    * @see
    *   ReadSeq
    * @see
    *   WriteSeq
    *
    * @param haddr
    *   address
    * @param hburst
    *   HBURST
    * @param hmastlock
    *   HMASTLOCK
    * @param hwrite
    *   HWRITE
    * @param hsize
    *   HSIZE
    * @param htrans
    *   HTRANS
    * @param busy
    *   number of busy cycles
    * @param hwdata
    *   HWDATA
    */
  abstract class AhbRwTransfer(
      haddr: BigInt,
      hburst: Int,
      hmastlock: Boolean,
      hprot: Int,
      hwrite: Boolean,
      hsize: Int,
      htrans: Int,
      var busy: Long = 0,
      hwdata: BigInt = 0
  ) extends AhbTransfer {

    private var cancel: Boolean = false

    /** Cancel the action after busy states */
    def cancelAfter(n: Long): Unit = {
      busy = n
      cancel = true
    }

    protected def addrPhaseComb = {
      HSEL #= true
      HADDR #= haddr
      HBURST #= hburst
      HMASTLOCK #= hmastlock
      HPROT #= hprot
      HWRITE #= hwrite
      HSIZE #= hsize
      HTRANS #= (if (busy > 0) Htrans.BUSY else htrans)
      busy -= 1
    }
    protected def dataPhaseComb = HWDATA #= hwdata
    override protected def dropDataPhase = cancel
    override protected def addrPhaseEndCond =
      HREADY && busy < 0 || cancel && busy == 0
  }

  /** Represents the absence of a transfer
    *
    * The duration of data phase is one clock cycle.
    */
  case class Blank() extends AhbTransfer with WriteTransfer {
    protected def dataPhaseComb = {}
    protected def addrPhaseComb = {}
    override protected def bootNext = bootNextOrDie()
    protected def complete = done()
  }

  /** IDLE transfer */
  case class Idle(addr: BigInt = 0, cond: () => Boolean = () => HREADY) extends AhbTransfer with WriteTransfer {
    protected def addrPhaseComb = {
      HSEL #= true
      HADDR #= addr
    }
    protected def dataPhaseComb = {}
    override protected def addrPhaseEndCond = cond()
    protected def complete = done
  }

  /** Represents the fact that HREADY was kept down by the previous transfer
    * with another slave
    *
    * The duration of data phase is (cycleCount + 1) cycles.
    *
    * {{{
    * HCLK   --__--__--__--__--__--__--__
    * HREADY --------____________--------
    *                ~~~~~~~~~~~~ cycleCount = 3 cycles
    *            ~~~~ address phase
    *                ~~~~~~~~~~~~~~~~ data phase
    * }}}
    *
    * @param cycleCount
    *   number of clock cycles during the which HREADY is kept down, starting
    *   with data phase
    */
  case class Unready(cycleCount: Long = 1) extends AhbTransfer with WriteTransfer {
    // Number of clock cycles with HREADY low, including current one
    var n = 0
    def checkN() = {
      hreadyoutOther = n == cycleCount
      n += 1
    }

    protected def addrPhaseComb = {}
    protected def dataPhaseComb = checkN()

    // Force to continue to data phase, else the transaction is stuck because HREADY is driven low
    override protected def addrPhaseEndCond = true
    protected def complete = done()
  }

  /** Read transfer
    *
    * @see
    *   Write
    *
    * @param addr
    *   address (HADDR)
    * @param hburst
    *   HBURST
    * @param hmastlock
    *   HMASTLOCK
    * @param hprot
    *   HPROT
    * @param hsize
    *   HSIZE
    * @param htrans
    *   HTRANS
    * @param busyCycles
    *   number of busy cycles
    */
  case class Read(
      addr: BigInt,
      hburst: Int = Hburst.SINGLE,
      hmastlock: Boolean = false,
      hprot: Int = Hprot.defaultValue,
      hsize: Int = Hsize.Word,
      htrans: Int = Htrans.NONSEQ,
      busyCycles: Long = 0
  ) extends AhbRwTransfer(
        addr,
        hburst,
        hmastlock,
        hprot,
        false,
        hsize,
        htrans,
        busyCycles
      )
      with ReadTransfer {
    protected def complete = done(HRDATA)
  }

  /** Write transfer
    *
    * @see
    *   Read
    *
    * @param addr
    *   HADDR
    * @param data
    *   HWDATA
    * @param hburst
    *   HBURST
    * @param hmastlock
    *   HMASTLOCK
    * @param hprot
    *   HPROT
    * @param hsize
    *   HSIZE
    * @param htrans
    *   HTRANS
    * @param busyCycles
    *   number of cycles BUSY
    */
  case class Write(
      addr: BigInt,
      data: BigInt,
      hburst: Int = Hburst.INCR,
      hmastlock: Boolean = false,
      hprot: Int = Hprot.defaultValue,
      hsize: Int = Hsize.Word,
      htrans: Int = Htrans.NONSEQ,
      busyCycles: Long = 0
  ) extends AhbRwTransfer(
        addr,
        hburst,
        hmastlock,
        hprot,
        true,
        hsize,
        htrans,
        busyCycles,
        data
      )
      with WriteTransfer {
    protected def complete = done()
  }

  // Section 3.5 Burst operation
  private def buildBurstAddr(
      addrStart: BigInt,
      burst: Hburst,
      hsize: Int
  ): Seq[BigInt] = {
    val size = Hsize.toBytes(hsize)

    for (i <- 0 until burst.beats) yield {
      val incrAddr = addrStart + i * size
      if (burst.isWrapping) {
        val boundary = burst.beats * size
        val mask = boundary - 1
        addrStart & ~mask | incrAddr & mask
      } else {
        incrAddr
      }
    }
  }

  /** Builds a sequence of ReadSeq
    *
    * @param addrStart
    *   first address
    * @param burst
    *   Hburst (contains burst type and length)
    * @param hmastlock
    *   HMASTLOCK
    * @param hprot
    *   HPROT
    * @param hsize
    *   HSIZE
    * @return
    *   sequence of all ReadSeq, chained by ascending index
    */
  def burstReadSeq(
      addrStart: BigInt,
      burst: Hburst,
      hmastlock: Boolean = false,
      hprot: Int = Hprot.defaultValue,
      hsize: Int = Hsize.Word
  ): Seq[Read] = {
    val addrs = buildBurstAddr(addrStart, burst, hsize)
    val reads = addrs.zipWithIndex.map { case (addr, i) =>
      if (i == 0)
        Read(addr, burst, hmastlock, hprot, hsize, Htrans.NONSEQ)
      else
        Read(addr, burst, hmastlock, hprot, hsize, Htrans.SEQ)
    }
    Transfer.chainSeq(reads)
    reads
  }

  /** Builds a sequence of WriteSeq
    *
    * @param addrStart
    *   first address
    * @param data
    *   sequence of data to write, must be at least the number of bursts
    * @param burst
    *   Hburst (contains burst type and length)
    * @param hmastlock
    *   HMASTLOCK
    * @param hprot
    *   HPROT
    * @param hsize
    *   HSIZE
    * @return
    *   sequence of all WriteSeq, chained by ascending index
    */
  def burstWriteSeq(
      addrStart: BigInt,
      data: Seq[BigInt],
      burst: Hburst,
      hmastlock: Boolean = false,
      hprot: Int = Hprot.defaultValue,
      hsize: Int = Hsize.Word
  ): Seq[Write] = {
    val addrs = buildBurstAddr(addrStart, burst, hsize)
    val writes = addrs.zipWithIndex.map { case (addr, i) =>
      if (i == 0)
        Write(
          addr,
          data(i),
          burst.hburst,
          hmastlock,
          hprot,
          hsize,
          Htrans.NONSEQ
        )
      else
        Write(addr, data(i), burst, hmastlock, hprot, hsize, Htrans.SEQ)
    }
    Transfer.chainSeq(writes)
    writes
  }
}

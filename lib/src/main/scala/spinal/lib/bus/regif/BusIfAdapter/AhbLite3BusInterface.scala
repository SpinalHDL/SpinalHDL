package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba3.ahblite.AhbLite3
import spinal.lib.bus.misc.SizeMapping

/* Written to be compliant with:
 * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
 */
case class AhbLite3BusInterface(bus: AhbLite3, sizeMap: SizeMapping, regPre: String = "")(implicit
    moduleName: ClassName
) extends BusIf {
  import bus._

  HREADYOUT := True
  HRESP := False

  /** Delays addr phase signal to get data phase signal.
    *
    * See Section 3.1 Basic transfers
    */
  private def dataPhase[T <: Data](what: T, init: T): T =
    RegNextWhen(what, HREADY) init init

  val askWrite: Bool = HSEL && HTRANS(1) && HWRITE

  val askRead: Bool = HSEL && HTRANS(1) && !HWRITE

  val doWrite: Bool = dataPhase(askWrite, False)

  val doRead: Bool = dataPhase(askRead, False)

  val readData: Bits = HRDATA

  val writeData: Bits = dataPhase(HWDATA, 0)

  val readError: Bool = Bool

  // Error management described in section 5.1
  val error = new Area {
    val cycle1, cycle2 = Bool

    cycle1 := readError
    cycle2 := RegNext(cycle1) init False

    when(cycle1) {
      HREADYOUT := False
      HRESP := True
    }
    when(cycle2) {
      HREADYOUT := True
      HRESP := True
    }
  }

  // TODO: How is it used? when askXxx or doXxx?
  def readAddress(): UInt = ???

  // TODO: How is it used? when askXxx or doXxx?
  def writeAddress(): UInt = ???

  def readHalt(): Unit = HREADYOUT := False

  def writeHalt(): Unit = HREADYOUT := False

  def busDataWidth: Int = bus.config.dataWidth

  def getModuleName: String = moduleName.name
}

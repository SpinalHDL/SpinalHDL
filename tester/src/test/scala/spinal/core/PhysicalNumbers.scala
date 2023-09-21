package spinal.core

import org.scalatest.funsuite.AnyFunSuite

class PhysicalNumbers extends AnyFunSuite {
  val hz100 = 100 Hz
  val hz100_2 = 100 Hz
  val hz200 = 200 Hz
  val ms2 = 100 sec

  test("Comparisons") {
    assert(hz100 == hz100)
    assert(hz100 == hz100_2)
    assert(hz100_2 == hz100)
    assert(hz100 != hz200)
    assert(hz100 != null)

    assert(hz100 < hz200)
    assert(!(hz200 < hz100))
    assert(!(hz100 < hz100_2))

    assert(hz200 > hz100)
    assert(!(hz100 > hz200))
    assert(!(hz100 < hz100_2))

    assert(hz100 <= hz200)
    assert(hz100 <= hz100_2)
    assert(!(hz200 <= hz100))

    assert(hz100 >= hz100_2)
    assert(hz200 >= hz100)
    assert(!(hz100 >= hz200))

    assert(hz100.hashCode() == hz100_2.hashCode())
  }

  test("formatting") {
    assert(f"${HertzNumber(5123000)}" == "5.123 MHz")
    assert(f"${HertzNumber(5123000)}%s" == "5.123 MHz")
    assert(f"${HertzNumber(5123000)}%S" == "5.123MHz")
    assert(f"${HertzNumber(5123000)}%#s" == "5.123")
    assert(f"${HertzNumber(5123000)}%.5s" == "5.12300 MHz")
    assert(f"${HertzNumber(5123000)}%15.5s" == "    5.12300 MHz")
    assert(f"${HertzNumber(5123000)}%-15.5s" == "5.12300 MHz    ")
  }
}

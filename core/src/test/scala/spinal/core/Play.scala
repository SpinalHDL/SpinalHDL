package spinal.core

/**
 * Created by PIC32F_USER on 14/08/2017.
 */
object Play1 {
  class TopLevel extends Component{
//    for(i <- 0 until 1000000) {
      val a, b, c, d, e, f, g, h, i, j = Bool()
      b := a || c
      c := b

      when2(c) {
        e := d
        when2(d) {
          f := e
        }
        b := f
      }.elsewhen(a) {
        val x = Bool()
        x := a || b
        i := g || x

      } otherwise {
        b := j
      }
//    }
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
     print(toplevel)
  }
}

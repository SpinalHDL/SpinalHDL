package spinal.tester.code

import spinal.core.SFix

/**
 * Created by PIC32F_USER on 19/07/2017.
 */
package object temp {
  implicit class SFixPimper(sfix : SFix){
    def miaou(x : Int) = println(sfix + " " + x)
  }
  def a  = 3
}
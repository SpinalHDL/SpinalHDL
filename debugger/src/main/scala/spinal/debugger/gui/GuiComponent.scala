//package spinal.debugger.gui
//
//
//import scalafx.Includes._
//import scalafx.scene.control._
//import scalafx.scene.input.KeyEvent
//
///**
// * Created by PIC on 22.04.2015.
// */
//class NumberTextField extends TextField {
//  filterEvent(KeyEvent.Any)((t: KeyEvent) => {
//    val ar = t.getCharacter().toCharArray();
//    val ch = ar(t.getCharacter().toCharArray().length - 1);
//    if (!(ch >= '0' && ch <= '9')) {
//      //System.out.println("The char you entered is not a number");
//      t.consume();
//    }
//  })
//
//}
package spinal.lib.bus.amba3.apb

import spinal.lib.bus.misc.SizeMapping


object Apb3Interconnect{
   def apply(master : Apb3,slaves : Iterable[(Apb3,SizeMapping)]): Apb3Decoder ={
    val decoder = new Apb3Decoder(master.config,slaves.map(_._2))
    val router = new Apb3Router(decoder.io.output.config)
    decoder.io.input <> master
    router.io.input <> decoder.io.output
    (slaves.map(_._1),router.io.outputs).zipped.map(_ <> _)
    decoder.setPartialName(master,"decoder")
  }
}

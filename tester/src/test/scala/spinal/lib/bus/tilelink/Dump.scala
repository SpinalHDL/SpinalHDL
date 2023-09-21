package spinal.lib.bus.tilelink


//class CPU() extends Area{
//  val node = InterconnectNode.master()
//
//  val logic = Elab build new Area{
//    node.m2s.parameters.load(
//      M2sParameters(
//        addressWidth = 32,
//        dataWidth    = 32,
//        masters = List(M2sAgent(
//          name = this,
//          mapping = List(M2sSource(
//            id = SizeMapping(0, 4),
//            emits = M2sTransfers(
//              get = SizeRange.upTo(0x40),
//              putFull = SizeRange.upTo(0x40)
//            )
//          ))
//        ))
//      )
//    )
//    node.m2s.setProposedFromParameters()
//    slave(node.bus)
//  }
//}
//
//
//class Adapter(canCrossClock : Boolean = false,
//              canUpsize : Boolean = false,
//              canDownSize : Boolean = false,
//              canSplit : Boolean = false) extends Area{
//  val input  = InterconnectNode.slave()
//  val output = InterconnectNode.master()
//
//  hardFork{
//    output.m2s.proposed.load(input.m2s.proposed)
//    input.m2s.supported.load(output.m2s.supported)
//    output.m2s.parameters.load(input.m2s.parameters)
//    input.s2m.parameters.load(output.s2m.parameters)
//    output.bus << input.bus
//  }
//}
//
//
//class VideoIn() extends Area{
//  val node = InterconnectNode.master()
//  node.m2s.parameters.load(
//    M2sParameters(
//      addressWidth = 32,
//      dataWidth    = 32,
//      masters = List(M2sAgent(
//        name = this,
//        mapping = List(M2sSource(
//          id = SizeMapping(0, 4),
//          emits = M2sTransfers(
//            putFull = SizeRange.upTo(0x40)
//          )
//        ))
//      ))
//    )
//  )
//  hardFork(slave(node.bus))
//}
//
//class VideoOut() extends Area{
//  val node = InterconnectNode.master()
//  node.m2s.parameters.load(
//    M2sParameters(
//      addressWidth = 32,
//      dataWidth    = 32,
//      masters = List(M2sAgent(
//        name = this,
//        mapping = List(M2sSource(
//          id = SizeMapping(0, 4),
//          emits = M2sTransfers(
//            get = SizeRange.upTo(0x40)
//          )
//        ))
//      ))
//    )
//  )
//  hardFork(slave(node.bus))
//}
//
//
//
////class Bridge() extends Area{
////  val node = ic.createNode()
////}
//
//class UART() extends Area{
//  val node = InterconnectNode.slave()
//  node.s2m.parameters.load(
//    S2mParameters.none
//  )
//  node.m2s.supported.loadAsync(
//    M2sSupport(
//      transfers = node.m2s.proposed.transfers.intersect(
//        M2sTransfers(
//          get = SizeRange.upTo(0x1000),
//          putFull = SizeRange.upTo(0x1000)
//        )
//      ),
//      dataWidth    = 32,
//      addressWidth = 8,
//      allowExecute = false
//    )
//  )
//  hardFork(master(node.bus))
//}
//
//class ROM() extends Area{
//  val node = InterconnectNode.slave()
//  node.s2m.parameters.load(
//    S2mParameters.none
//  )
//  node.m2s.supported.loadAsync(
//    M2sSupport(
//      transfers = node.m2s.proposed.transfers.intersect(
//        M2sTransfers(
//          get = SizeRange.upTo( 0x1000)
//        )
//      ),
//      dataWidth    = 32,
//      addressWidth = 7,
//      allowExecute = false
//    )
//  )
//  hardFork(master(node.bus))
//}
//
//class StreamOut() extends Area{
//  val node = InterconnectNode.slave()
//  node.s2m.parameters.load(
//    S2mParameters.none
//  )
//  node.m2s.supported.loadAsync(
//    M2sSupport(
//      transfers = node.m2s.proposed.transfers.intersect(
//        M2sTransfers(
//          putFull = SizeRange.upTo( 0x1000)
//        )
//      ),
//      dataWidth    = 32,
//      addressWidth = 6,
//      allowExecute = false
//    )
//  )
//  hardFork(master(node.bus))
//}
//
//class CoherentCpu() extends Area{
//  val node = InterconnectNode.master()
//  node.m2s.parameters.load(
//    M2sParameters(
//      addressWidth = 32,
//      dataWidth    = 32,
//      masters = List(M2sAgent(
//        name = this,
//        mapping = List(M2sSource(
//          id = SizeMapping(0, 4),
//          emits = M2sTransfers(
//            acquireT = SizeRange(0x40),
//            acquireB = SizeRange(0x40),
//            probeAckData = SizeRange(0x40)
//          )
//        ))
//      ))
//    )
//  )
//  hardFork(slave(node.bus))
//}
//
//
//
//
//
//
//
//object TopGen extends App{
//  SpinalVerilog(new Component{
//    val slowCd = ClockDomain.external("slow")
//    val fastCd = ClockDomain.external("fast")
//
//    val system = fastCd on new Area {
//      val cpu0 = new CPU()
//      val cpu1 = new CPU()
//
//      val busA = InterconnectNode()
//      busA << cpu0.node
//      busA << cpu1.node
//    }
//    val peripheral = slowCd on new Area {
//      val peripheralBus = InterconnectNode()
//      peripheralBus at(0x10000000, 16 MiB) of system.busA
//
//      val adapter = new Adapter()
//      adapter.input << peripheralBus
//
//      val uart0 = new UART()
//      uart0.node at 0x100 of adapter.output
//
//      val uart1 = new UART()
//      uart1.node at 0x200 of adapter.output
//    }
//
//    //    uart0.node.mapping.load(SizeMapping(0x100, 0x100))
//
//    //    val uart1 = new UART()
//    //    interconnect.connect(bridgeA.node, uart1.node)
//    //    uart1.node.mapping.load(SizeMapping(0x200, 0x100))
//
//
//    //    val cpu0 = new CPU()
//    //    val cpu1 = new CPU()
//    //
//    //    val bridgeA = new Bridge()
//    //    bridgeA.node.mapping.load(DefaultMapping)
//    //    interconnect.connect(cpu0.node, bridgeA.node)
//    //    interconnect.connect(cpu1.node, bridgeA.node)
//    //
//    //    val bridgeB = new Bridge()
//    //    bridgeB.node.mapping.load(SizeMapping(0x100, 0x300))
//    //    interconnect.connect(bridgeA.node, bridgeB.node)
//
//    //    val videoIn0 = new VideoIn()
//    //    interconnect.connect(videoIn0.node, bridgeA.node)
//    //
//    //    val videoOut0 = new VideoOut()
//    //    interconnect.connect(videoOut0.node, bridgeA.node)
//
//    //    val uart0 = new UART()
//    //    interconnect.connect(bridgeB.node, uart0.node)
//    //    uart0.node.mapping.load(SizeMapping(0x100, 0x100))
//    //
//    //    val uart1 = new UART()
//    //    interconnect.connect(bridgeB.node, uart1.node)
//    //    uart1.node.mapping.load(SizeMapping(0x200, 0x100))
//
//    //    val bridgeC = new Bridge()
//    //    bridgeC.node.mapping.load(DefaultMapping)
//    //    interconnect.connect(bridgeB.node, bridgeC.node)
//
//    //    val bridgeD = new Bridge()
//    //    bridgeD.node.mapping.load(DefaultMapping)
//    //    interconnect.connect(bridgeC.node, bridgeD.node)
//    //
//    //    val rom0 = new ROM()
//    //    rom0.node.mapping.load(SizeMapping(0x300, 0x100))
//    //    interconnect.connect(bridgeD.node, rom0.node)
//    //
//    //    val streamOut = new StreamOut()
//    //    streamOut.node.mapping.load(SizeMapping(0x400, 0x100))
//    //    interconnect.connect(bridgeD.node, streamOut.node)
//
//    //    val hub = new CoherencyHubIntegrator()
//    //    interconnect.connect(hub.memGet, bridgeA.node)
//    //    interconnect.connect(hub.memPut, bridgeA.node)
//    //
//    //    val ccpu0 = new CoherentCpu()
//    //    val c0 = hub.createCoherent()
//    //    interconnect.connect(ccpu0.node, c0)
//    //
//    //    val ccpu1 = new CoherentCpu()
//    //    val c1 = hub.createCoherent()
//    //    interconnect.connect(ccpu1.node, c1)
//
//    //    val rom0 = new ROM()
//    //    rom0.node.mapping.load(SizeMapping(0x300, 0x100))
//    //    interconnect.connect(cpu0.node, rom0.node)
//
//    //    val streamOut = new StreamOut()
//    //    streamOut.node.mapping.load(SizeMapping(0x400, 0x100))
//    //    interconnect.connect(cpu0.node, streamOut.node)
//
//    //    interconnect.connect(cpu0.node, uart0.node)
//    //    interconnect.connect(cpu1.node, uart0.node)
//    //    interconnect.connect(cpu0.node, uart1.node)
//    //    interconnect.connect(cpu1.node, uart1.node)
//    //    uart0.node.mapping.load(SizeMapping(0x100, 0x100))
//    //    uart1.node.mapping.load(SizeMapping(0x200, 0x100))
//  })
//}
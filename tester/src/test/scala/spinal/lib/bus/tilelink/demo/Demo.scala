package spinal.lib.bus.tilelink.demo

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.M2sTransfers
import spinal.lib.io.TriStateArray

object Demo extends App {
  SpinalVerilog(new Component{

    import spinal.lib.bus.tilelink
    val param = tilelink.BusParameter.simple(
      addressWidth = 32,
      dataWidth    = 64,
      sizeBytes    = 64,
      sourceWidth  = 4
    )
    val busA, busB = tilelink.Bus(param)
    busA << busB
  })

  SpinalVerilog(new Component {

    import spinal.lib.bus.tilelink
    val param = tilelink.BusParameter(
      addressWidth = 32,
      dataWidth = 64,
      sizeBytes = 64,
      sourceWidth = 4,
      sinkWidth = 0,
      withBCE = false,
      withDataA = true,
      withDataB = false,
      withDataC = false,
      withDataD = true,
      node = null
    )
    val busA, busB = tilelink.Bus(param)
    busA << busB
  })

  {
    import spinal.lib.bus.tilelink
    import spinal.core.fiber.Fiber

    class CpuFiber extends Area {
      // Define a node facing downward (toward slaves only)
      val down = tilelink.fabric.Node.down()

      val fiber = Fiber build new Area {
        // Here we force the bus parameters to a specific configurations
        down.m2s forceParameters tilelink.M2sParameters(
          addressWidth = 32,
          dataWidth = 64,
          // We define the traffic of each master using this node. (one master => one M2sAgent)
          // In our case, there is only the CpuFiber.
          masters = List(
            tilelink.M2sAgent(
              name = CpuFiber.this, // Reference to the original agent.
              // A agent can use multiple sets of source ID for different purposes
              // Here we define the usage of every sets of source ID
              // In our case, let's say we use ID [0-3] to emit get/putFull requests
              mapping = List(
                tilelink.M2sSource(
                  id = SizeMapping(0, 4),
                  emits = M2sTransfers(
                    get = tilelink.SizeRange(1, 64), //Meaning the get access can be any power of 2 size in [1, 64]
                    putFull = tilelink.SizeRange(1, 64)
                  )
                )
              )
            )
          )
        )

        // Lets say the CPU doesn't support any slave initiated requests (memory coherency)
        down.s2m.supported load tilelink.S2mSupport.none()

        val mappings = spinal.lib.system.tag.MemoryConnection.getMemoryTransfers(down)
        for(mapping <- mappings){
          println(s"- ${mapping.where} -> ${mapping.transfers}")
        }

        // Then we can generate some hardware (nothing usefull in this example)
        down.bus.a.setIdle()
        down.bus.d.ready := True
      }
    }

    import spinal.lib._
    class GpioFiber extends Area {
      // Define a node facing upward (toward masters only)
      val up = tilelink.fabric.Node.up()

      // Define a elaboration thread to specify the "up" parameters and generate the hardware
      val fiber = Fiber build new Area {
        // Here we first define what our up node support. m2s mean master to slave requests
        up.m2s.supported load tilelink.M2sSupport(
          addressWidth = 12,
          dataWidth = 32,
          // Transfers define which kind of memory transactions our up node will support.
          // Here it only support 4 bytes get/putfull
          transfers = tilelink.M2sTransfers(
            get = tilelink.SizeRange(4),
            putFull = tilelink.SizeRange(4)
          )
        )
        // s2m mean slave to master requests, those are only use for memory coherency purpose
        // So here we specify we do not need any
        up.s2m.none()

        // Then we can finally generate some hardware
        // Starting by defining a 32 bits TriStateArray (Array meaning that each pin has its own writeEnable bit
        val pins = master(TriStateArray(32 bits))

        // tilelink.SlaveFactory is a utility allowing to easily generate the logic required
        // to control some hardware from a tilelink bus.
        val factory = new tilelink.SlaveFactory(up.bus, allowBurst = false)

        // Use the SlaveFactory API to generate some hardware to read / drive the pins
        val writeEnableReg = factory.drive(pins.writeEnable, 0x0) init (0)
        val writeReg = factory.drive(pins.write, 0x4) init(0)
        factory.read(pins.read, 0x8)
      }
    }

    class RamFiber() extends Area {
      val up = tilelink.fabric.Node.up()

      val thread = Fiber build new Area {
        // Here the supported parameters are function of what the master would like us to idealy support.
        // The tilelink.Ram support all addressWidth / dataWidth / burst length / get / put accesses
        // but doesn't support atomic / coherency. So we take what is proposed to use and restrict it to
        // all sorts of get / put request
        up.m2s.supported load up.m2s.proposed.intersect(M2sTransfers.allGetPut)
        up.s2m.none()

        // Here we infer how many bytes our ram need to be, by looking at the memory mapping of the connected masters
        val bytes = up.ups.map(e => e.mapping.value.highestBound - e.mapping.value.lowerBound + 1).max.toInt

        // Then we finaly generate the regular hardware
        val logic = new tilelink.Ram(up.bus.p.node, bytes)
        logic.io.up << up.bus
      }
    }

//    SpinalVerilog(new Component {
//      val cpu = new CpuFiber()
//
//      val ram = new RamFiber()
//      ram.up at(0x10000, 0x200) of cpu.down // map the ram at [0x10000-0x101FF], the ram will infer its own size from it
//
//      val gpio = new GpioFiber()
//      gpio.up at 0x20000 of cpu.down // map the gpio at [0x20000-0x20FFF], its range of 4KB being fixed internally
//    })

    SpinalVerilog(new Component {
      val cpu = new CpuFiber()

      val ram = new RamFiber()
      ram.up at(0x10000, 0x200) of cpu.down

      // Create a peripherals namespace to keep things clean
      val peripherals = new Area{
        // Create a intermediate node in the interconnect
        val access = tilelink.fabric.Node()
        access at 0x20000 of cpu.down

        val gpioA = new GpioFiber()
        gpioA.up at 0x0000 of access

        val gpioB = new GpioFiber()
        gpioB.up at 0x1000 of access
      }
    })


  }


}

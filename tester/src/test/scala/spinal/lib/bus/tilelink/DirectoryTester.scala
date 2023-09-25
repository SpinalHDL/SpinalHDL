package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink
import tilelink._
import tilelink.fabric.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.Component
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.coherent.DirectoryFiber
import spinal.lib.bus.tilelink.fabric.{MasterBus, SlaveBus}
import spinal.lib.system.tag.PMA

class DirectoryTester extends AnyFunSuite{

  test("directed"){
    val tester = new TilelinkTester(
      simConfig = SimConfig.withFstWave,
      cGen = new Component {
        val m0 = new MasterBus(
          M2sParameters(
            addressWidth = 32,
            dataWidth = 64,
            masters = List.tabulate(4)(mid => M2sAgent(
              name = null,
              mapping = List(M2sSource(
                id = SizeMapping(mid * 4, 4),
                emits = M2sTransfers(
                  acquireT = SizeRange(64),
                  acquireB = SizeRange(64),
                  get = SizeRange(1, 64),
                  putFull = SizeRange(1, 64),
                  putPartial = SizeRange(1, 64)
                )
              ))
            ))
          )
        )

        val directory = new DirectoryFiber()
        directory.parameter.cacheWays = 4
        directory.parameter.cacheBytes = 4096
        directory.parameter.allocateOnMiss = (op, src, addr, size) => True
        directory.up << m0.node

        val s0 = new SlaveBus(
          M2sSupport(
            transfers = M2sTransfers.all,
            dataWidth = 64,
            addressWidth = 13,
            allowExecute = false
          ),
          S2mParameters.none()
        )
        s0.node at 0x10000 of directory.down
        s0.node.addTag(PMA.MAIN)

        val s1 = new SlaveBus(
          M2sSupport(
            transfers = M2sTransfers.all,
            dataWidth = 64,
            addressWidth = 10,
            allowExecute = false
          ),
          S2mParameters.none()
        )
        s1.node at 0x20000 of directory.down
      }
    )
    tester.doSimDirected("manual"){tb =>
      tb.coverPutFullData(4)
    }
    
    tester.doSimDirected("get"){_.coverGet(4)}
    tester.doSimDirected("putFull") {_.coverPutFullData(4)}
    tester.doSimDirected("putPartial") {_.coverPutPartialData(4)}

    tester.checkErrors()
  }
}

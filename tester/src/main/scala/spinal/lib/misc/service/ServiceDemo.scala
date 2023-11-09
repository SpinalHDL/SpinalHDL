package spinal.lib.misc.service

import spinal.core._
import spinal.lib.misc.service._


object Example1 extends App{

  // Let's define a Component with a ServiceHost instance
  class SubComponent extends Component {
    val host = new ServiceHost()
  }

  // Let's define a plugin which create a register
  class StatePlugin extends FiberPlugin {
    // during build new Area { body } will run the body of code in the Fiber build phase, in the context of the ServiceHost
    val logic = during build new Area {
      val signal = Reg(UInt(32 bits))
    }
  }

  // Let's define a plugin which will make the StatePlugin's register increment
  class DriverPlugin extends FiberPlugin {
    // We define how to get the instance of StatePlugin in the ServiceHost
    lazy val spl = host[StatePlugin].logic.get

    val logic = during build new Area {
      // Generate the increment hardware
      spl.signal := spl.signal + 1
    }
  }

  class TopLevel extends Component {
    val sub = new SubComponent()

    // Here we create plugins and embed them in sub.host
    new DriverPlugin().setHost(sub.host)
    new StatePlugin().setHost(sub.host)
  }

  SpinalVerilog(new TopLevel).printRtl()
}

object Example2 extends App{
  class SubComponent extends Component {
    val host = new ServiceHost()
  }

  class StatePlugin extends FiberPlugin {
    val logic = during build new Area {
      val signal = Reg(UInt(32 bits))
    }
  }

  class DriverPlugin extends FiberPlugin {
    lazy val spl = host[StatePlugin].logic.get

    // incrementBy will be set by others plugin at elaboration time
    var incrementBy = 0
    val logic = during build new Area {
      // Generate the incrementer hardware
      spl.signal := spl.signal + incrementBy
    }
  }

  // Let's define a plugin which will modify the DriverPlugin.incrementBy variable because letting it elaborate its hardware
  class IncrementerPlugin extends FiberPlugin {
    def dp = host[DriverPlugin]
    // during setup { body } will run the body of code in the Fiber setup phase (it is before the Fiber build phase)
    during setup {
      // Prevent the DriverPlugin from executing its build's body (until release() is called)
      dp.retain()
    }
    val logic = during build new Area {
      // Let's mutate DriverPlugin.incrementBy
      dp.incrementBy += 1

      // Allows the DriverPlugin to execute its build's body
      dp.release()
    }
  }

  class TopLevel extends Component {
    val sub = new SubComponent()

    sub.host.asHostOf(
      new DriverPlugin(),
      new StatePlugin(),
      new IncrementerPlugin(),
      new IncrementerPlugin()
    )
  }

  SpinalVerilog(new TopLevel).printRtl()
}
package spinal.lib.bus.simple

import spinal.core._
import spinal.lib.bus.misc._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import spinal.lib.bus.bmb.BmbParameter

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class PipelinedMemoryBusConfig(addressWidth : Int, dataWidth : Int){
//  def toBmbConfig() = BmbParameter(
//    addressWidth = addressWidth,
//    dataWidth = dataWidth,
//    lengthWidth = log2Up(dataWidth/8),
//    sourceWidth = 0,
//    contextWidth = 0,
//    canRead = true,
//    canWrite = true,
//    alignment     = BmbParameter.BurstAlignement.LENGTH,
//    maximumPendingTransactionPerId = Int.MaxValue
//  )
}

case class PipelinedMemoryBusCmd(config : PipelinedMemoryBusConfig) extends Bundle{
  val write = Bool
  val address = UInt(config.addressWidth bits)
  val data = Bits(config.dataWidth bits)
  val mask = Bits(config.dataWidth / 8 bit)
}

case class PipelinedMemoryBusRsp(config : PipelinedMemoryBusConfig) extends Bundle{
  val data = Bits(config.dataWidth bits)
}

object PipelinedMemoryBus{
  def apply(addressWidth : Int, dataWidth : Int) = new PipelinedMemoryBus(PipelinedMemoryBusConfig(addressWidth, dataWidth))
}
case class PipelinedMemoryBus(config : PipelinedMemoryBusConfig) extends Bundle with IMasterSlave {
  val cmd = Stream(PipelinedMemoryBusCmd(config))
  val rsp = Flow(PipelinedMemoryBusRsp(config))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

  def <<(m : PipelinedMemoryBus) : Unit = {
    val s = this
    assert(m.config.addressWidth >= s.config.addressWidth)
    assert(m.config.dataWidth == s.config.dataWidth)
    s.cmd.valid := m.cmd.valid
    s.cmd.write := m.cmd.write
    s.cmd.address := m.cmd.address.resized
    s.cmd.data := m.cmd.data
    s.cmd.mask := m.cmd.mask
    m.cmd.ready := s.cmd.ready
    m.rsp.valid := s.rsp.valid
    m.rsp.data := s.rsp.data
  }
  def >>(s : PipelinedMemoryBus) : Unit = s << this

  def cmdM2sPipe(): PipelinedMemoryBus = {
    val ret = cloneOf(this)
    this.cmd.m2sPipe() >> ret.cmd
    this.rsp           << ret.rsp
    ret
  }

  def cmdS2mPipe(): PipelinedMemoryBus = {
    val ret = cloneOf(this)
    this.cmd.s2mPipe() >> ret.cmd
    this.rsp << ret.rsp
    ret
  }

  def rspPipe(): PipelinedMemoryBus = {
    val ret = cloneOf(this)
    this.cmd >> ret.cmd
    this.rsp << ret.rsp.stage()
    ret
  }

//  def toBmb() : Bmb = {
//    val bmb = Bmb(config.toBmbConfig)
//    bmb.cmd.
//  }
}





object PipelinedMemoryBusArbiter{
  def apply(inputs : Seq[PipelinedMemoryBus], pendingRspMax : Int, rspRouteQueue : Boolean, transactionLock : Boolean): PipelinedMemoryBus = {
    val c = PipelinedMemoryBusArbiter(inputs.head.config, inputs.size, pendingRspMax, rspRouteQueue, transactionLock)
    (inputs, c.io.inputs).zipped.foreach(_ <> _)
    c.io.output
  }
}

case class PipelinedMemoryBusArbiter(pipelinedMemoryBusConfig : PipelinedMemoryBusConfig, portCount : Int, pendingRspMax : Int, rspRouteQueue : Boolean, transactionLock : Boolean = true) extends Component{
  val io = new Bundle{
    val inputs = Vec(slave(PipelinedMemoryBus(pipelinedMemoryBusConfig)), portCount)
    val output = master(PipelinedMemoryBus(pipelinedMemoryBusConfig))
  }
  val logic = if(portCount == 1) new Area{
    io.output << io.inputs(0)
  } else new Area {
    val arbiterFactory = StreamArbiterFactory.lowerFirst
    if(transactionLock) arbiterFactory.transactionLock else arbiterFactory.noLock
    val arbiter = arbiterFactory.build(PipelinedMemoryBusCmd(pipelinedMemoryBusConfig), portCount)
    (arbiter.io.inputs, io.inputs).zipped.foreach(_ <> _.cmd)

    val rspRouteOh = Bits(portCount bits)

    val rsp = if(!rspRouteQueue) new Area{
      assert(pendingRspMax == 1)
      val pending = RegInit(False) clearWhen(io.output.rsp.valid)
      val target = Reg(Bits(portCount bits))
      rspRouteOh := target
      when(io.output.cmd.fire && !io.output.cmd.write){
        target  := arbiter.io.chosenOH
        pending := True
      }
      io.output.cmd << arbiter.io.output.haltWhen(pending && !io.output.rsp.valid)
    } else new Area{
      val (outputCmdFork, routeCmdFork) = StreamFork2(arbiter.io.output)
      io.output.cmd << outputCmdFork

      val rspRoute = routeCmdFork.translateWith(arbiter.io.chosenOH).throwWhen(routeCmdFork.write).queueLowLatency(size = pendingRspMax, latency = 1)
      rspRoute.ready := io.output.rsp.valid
      rspRouteOh := rspRoute.payload
    }

    for ((input, id) <- io.inputs.zipWithIndex) {
      input.rsp.valid := io.output.rsp.valid && rspRouteOh(id)
      input.rsp.payload := io.output.rsp.payload
    }
  }
}

class PipelinedMemoryBusSlaveFactory(bus: PipelinedMemoryBus) extends BusSlaveFactoryDelayed{
  bus.cmd.ready := True

  val readAtCmd = Flow(Bits(bus.config.dataWidth bits))
  val readAtRsp = readAtCmd.stage()

  val askWrite = (bus.cmd.valid && bus.cmd.write).allowPruning()
  val askRead  = (bus.cmd.valid && !bus.cmd.write).allowPruning()
  val doWrite  = (askWrite && bus.cmd.ready).allowPruning()
  val doRead   = (askRead  && bus.cmd.ready).allowPruning()

  bus.rsp.valid := readAtRsp.valid
  bus.rsp.data := readAtRsp.payload

  readAtCmd.valid := doRead
  readAtCmd.payload := 0

  def readAddress() : UInt = bus.cmd.address
  def writeAddress() : UInt = bus.cmd.address

  override def readHalt(): Unit = bus.cmd.ready := False
  override def writeHalt(): Unit = bus.cmd.ready := False

  override def build(): Unit = {
    super.doNonStopWrite(bus.cmd.data)

    def doMappedElements(jobs : Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs = jobs,
      askWrite = askWrite,
      askRead = askRead,
      doWrite = doWrite,
      doRead = doRead,
      writeData = bus.cmd.data,
      readData = readAtCmd.payload
    )

    switch(bus.cmd.address) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedElements(jobs)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(bus.cmd.address)){
        doMappedElements(jobs)
      }
    }
  }

  override def busDataWidth: Int = bus.config.dataWidth
  override def wordAddressInc: Int = busDataWidth / 8
}

case class PipelinedMemoryBusDecoder(busConfig : PipelinedMemoryBusConfig, mappings : Seq[AddressMapping], pendingMax : Int = 3) extends Component{
  val io = new Bundle {
    val input = slave(PipelinedMemoryBus(busConfig))
    val outputs = Vec(master(PipelinedMemoryBus(busConfig)), mappings.size)
  }
  val hasDefault = mappings.contains(DefaultMapping)
  val logic = if(hasDefault && mappings.size == 1){
    io.outputs(0) <> io.input
  } else new Area {
    val hits = Vec(Bool, mappings.size)
    for ((slaveBus, memorySpace, hit) <- (io.outputs, mappings, hits).zipped) yield {
      hit := (memorySpace match {
        case DefaultMapping => !hits.filterNot(_ == hit).orR
        case _ => memorySpace.hit(io.input.cmd.address)
      })
      slaveBus.cmd.valid := io.input.cmd.valid && hit
      slaveBus.cmd.payload := io.input.cmd.payload.resized
    }
    val noHit = if (!hasDefault) !hits.orR else False
    io.input.cmd.ready := (hits, io.outputs).zipped.map(_ && _.cmd.ready).orR || noHit

    val rspPendingCounter = Reg(UInt(log2Up(pendingMax + 1) bits)) init (0)
    rspPendingCounter := rspPendingCounter + U(io.input.cmd.fire && !io.input.cmd.write) - U(io.input.rsp.valid)
    val rspHits = RegNextWhen(hits, io.input.cmd.fire)
    val rspPending = rspPendingCounter =/= 0
    val rspNoHit = if (!hasDefault) !rspHits.orR else False
    io.input.rsp.valid := io.outputs.map(_.rsp.valid).orR || (rspPending && rspNoHit)
    io.input.rsp.payload := io.outputs.map(_.rsp.payload).read(OHToUInt(rspHits))

    val cmdWait = (io.input.cmd.valid && rspPending && hits =/= rspHits) || rspPendingCounter === pendingMax
    when(cmdWait) {
      io.input.cmd.ready := False
      io.outputs.foreach(_.cmd.valid := False)
    }
  }
}

object PipelinedMemoryBusConnectors{
  def direct(m : PipelinedMemoryBus, s : PipelinedMemoryBus) : Unit = m >> s
}

case class PipelinedMemoryBusInterconnect(){
  case class MasterModel(var connector : (PipelinedMemoryBus,PipelinedMemoryBus) => Unit = PipelinedMemoryBusConnectors.direct)
  case class SlaveModel(mapping : AddressMapping, var connector : (PipelinedMemoryBus,PipelinedMemoryBus) => Unit = PipelinedMemoryBusConnectors.direct, var transactionLock : Boolean = true)
  case class ConnectionModel(m : PipelinedMemoryBus, s : PipelinedMemoryBus, var connector : (PipelinedMemoryBus,PipelinedMemoryBus) => Unit = PipelinedMemoryBusConnectors.direct)

  val masters = mutable.LinkedHashMap[PipelinedMemoryBus, MasterModel]()
  val slaves = mutable.LinkedHashMap[PipelinedMemoryBus, SlaveModel]()
  val connections = ArrayBuffer[ConnectionModel]()
  var arbitrationPendingRspMaxDefault = 1
  var arbitrationRspRouteQueueDefault = false

  def perfConfig(): Unit ={
    arbitrationPendingRspMaxDefault = 7
    arbitrationRspRouteQueueDefault = true
  }

  def areaConfig(): Unit ={
    arbitrationPendingRspMaxDefault = 1
    arbitrationRspRouteQueueDefault = false
  }

  def setConnector(bus : PipelinedMemoryBus)( connector : (PipelinedMemoryBus,PipelinedMemoryBus) => Unit): Unit = (masters.get(bus), slaves.get(bus)) match {
    case (Some(m), _) =>    m.connector = connector
    case (None, Some(s)) => s.connector = connector
    case _ => ???
  }

  def setConnector(m : PipelinedMemoryBus, s : PipelinedMemoryBus)(connector : (PipelinedMemoryBus,PipelinedMemoryBus) => Unit): Unit = connections.find(e => e.m == m && e.s == s) match {
    case Some(c) => c.connector = connector
    case _ => ???
  }

  def addSlave(bus: PipelinedMemoryBus,mapping: AddressMapping) : this.type = {
    slaves(bus) = SlaveModel(mapping)
    this
  }

  def addSlaves(orders : (PipelinedMemoryBus,AddressMapping)*) : this.type = {
    orders.foreach(order => addSlave(order._1,order._2))
    this
  }

  def noTransactionLockOn(slave : PipelinedMemoryBus) : Unit = slaves(slave).transactionLock = false
  def noTransactionLockOn(slaves : Seq[PipelinedMemoryBus]) : Unit = slaves.foreach(noTransactionLockOn(_))


  def addMaster(bus : PipelinedMemoryBus, accesses : Seq[PipelinedMemoryBus] = Nil) : this.type = {
    masters(bus) = MasterModel()
    for(s <- accesses) connections += ConnectionModel(bus, s)
    this
  }

  def addMasters(specs : (PipelinedMemoryBus,Seq[PipelinedMemoryBus])*) : this.type = {
    specs.foreach(spec => addMaster(spec._1,spec._2))
    this
  }

  def addConnection(m : PipelinedMemoryBus, s : PipelinedMemoryBus) : this.type = {
    connections += ConnectionModel(m, s)
    this
  }

  def build(): Unit ={
    val connectionsInput  = mutable.HashMap[ConnectionModel,PipelinedMemoryBus]()
    val connectionsOutput = mutable.HashMap[ConnectionModel,PipelinedMemoryBus]()
    for((bus, model) <- masters){
      val busConnections = connections.filter(_.m == bus)
      val busSlaves = busConnections.map(c => slaves(c.s))
      val decoder = new PipelinedMemoryBusDecoder(bus.config, busSlaves.map(_.mapping))
      decoder.setCompositeName(bus, "decoder")
      model.connector(bus, decoder.io.input)
      for((connection, decoderOutput) <- (busConnections, decoder.io.outputs).zipped) {
        connectionsInput(connection) = decoderOutput
      }
    }

    for((bus, model) <- slaves){
      val busConnections = connections.filter(_.s == bus)
      val busMasters = busConnections.map(c => masters(c.m))
      val arbiter = new PipelinedMemoryBusArbiter(bus.config, busMasters.size, arbitrationPendingRspMaxDefault, arbitrationRspRouteQueueDefault, model.transactionLock)
      arbiter.setCompositeName(bus, "arbiter")
      model.connector(arbiter.io.output, bus)
      for((connection, arbiterInput) <- (busConnections, arbiter.io.inputs).zipped) {
        connectionsOutput(connection) = arbiterInput
      }
    }

    for(connection <- connections){
      val m = connectionsInput(connection)
      val s = connectionsOutput(connection)
      if(m.config == s.config) {
        connection.connector(m, s)
      }else{
        val tmp = cloneOf(s)
        m >> tmp //Adapte the bus kind.
        connection.connector(tmp,s)
      }
    }
  }

  //Will make SpinalHDL calling the build function at the end of the current component elaboration
  Component.current.addPrePopTask(build)
}


case class PipelinedMemoryBusToApbBridge(apb3Config: Apb3Config, pipelineBridge : Boolean, pipelinedMemoryBusConfig : PipelinedMemoryBusConfig) extends Component{
  assert(apb3Config.dataWidth == pipelinedMemoryBusConfig.dataWidth)

  val io = new Bundle {
    val pipelinedMemoryBus = slave(PipelinedMemoryBus(pipelinedMemoryBusConfig))
    val apb = master(Apb3(apb3Config))
  }

  val pipelinedMemoryBusStage = PipelinedMemoryBus(pipelinedMemoryBusConfig)
  pipelinedMemoryBusStage.cmd << (if(pipelineBridge) io.pipelinedMemoryBus.cmd.halfPipe() else io.pipelinedMemoryBus.cmd)
  pipelinedMemoryBusStage.rsp >-> io.pipelinedMemoryBus.rsp

  val state = RegInit(False)
  pipelinedMemoryBusStage.cmd.ready := False

  io.apb.PSEL(0) := pipelinedMemoryBusStage.cmd.valid
  io.apb.PENABLE := state
  io.apb.PWRITE  := pipelinedMemoryBusStage.cmd.write
  io.apb.PADDR   := pipelinedMemoryBusStage.cmd.address.resized
  io.apb.PWDATA  := pipelinedMemoryBusStage.cmd.data

  pipelinedMemoryBusStage.rsp.valid := False
  pipelinedMemoryBusStage.rsp.data  := io.apb.PRDATA
  when(!state) {
    state := pipelinedMemoryBusStage.cmd.valid
  } otherwise {
    when(io.apb.PREADY){
      state := False
      pipelinedMemoryBusStage.rsp.valid := !pipelinedMemoryBusStage.cmd.write
      pipelinedMemoryBusStage.cmd.ready := True
    }
  }
}
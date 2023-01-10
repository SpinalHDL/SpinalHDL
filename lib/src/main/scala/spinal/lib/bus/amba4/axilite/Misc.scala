package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4IdRemover, Axi4ReadOnly, Axi4Unburster, Axi4WriteOnly}

object AxiLite4Utils {

  private def toLiteConfig(config: Axi4Config) = {
    AxiLite4Config(
      addressWidth = config.addressWidth,
      dataWidth = config.dataWidth,
      readIssuingCapability = config.readIssuingCapability,
      writeIssuingCapability = config.writeIssuingCapability,
      combinedIssuingCapability = config.combinedIssuingCapability,
      readDataReorderingDepth = config.readDataReorderingDepth
    )
  }

  private def toAxiConfig(config: AxiLite4Config) = {
    Axi4Config(
      addressWidth = config.addressWidth,
      dataWidth = config.dataWidth,
      useId = false,
      useRegion = false,
      useBurst = false,
      useLock = false,
      useCache = false,
      useSize = false,
      useQos = false,
      useLen = false,
      useResp = true,
      useProt = true,
      useStrb = true,
      readIssuingCapability = config.readIssuingCapability,
      writeIssuingCapability = config.writeIssuingCapability,
      combinedIssuingCapability = config.combinedIssuingCapability,
      readDataReorderingDepth = config.readDataReorderingDepth
    )
  }

  // Axi ---> AxiLite
  implicit class Axi4Rich(axi: Axi4) {
    def toLite(): AxiLite4 = {
      val axiLite = AxiLite4(toLiteConfig(axi.config))
      axiLite << axi.toReadOnly().toLite()
      axiLite << axi.toWriteOnly().toLite()
      axiLite
    }

    def fromLite(): AxiLite4 = {
      val axiLite = AxiLite4(toLiteConfig(axi.config))
      axiLite.toAxi() >> axi
      axiLite
    }
  }

  implicit class Axi4ReadyOnlyRich(axi: Axi4ReadOnly) {
    def toLite(): AxiLite4ReadOnly = {
      val axiNoId = if (axi.config.useId) {
        Axi4IdRemover(axi)
      } else {
        axi
      }
      val axiUnburst = if (axi.config.useLen) {
        Axi4Unburster(axiNoId)
      } else {
        axiNoId
      }
      val axiNoSize = if (axi.config.useSize) {
        val axiNoSize = Axi4ReadOnly(axiUnburst.config.copy(useSize = false))
        axiNoSize.ar.arbitrationFrom(axiUnburst.ar)
        axiNoSize.ar.payload.assignSomeByName(axiUnburst.ar.payload)

        axiUnburst.r.arbitrationFrom(axiNoSize.r)
        axiUnburst.r.payload.assignSomeByName(axiNoSize.r.payload)
        axiNoSize
      } else {
        axiUnburst
      }

      val axiLite = AxiLite4ReadOnly(toLiteConfig(axi.config))
      val axiMinimal = Axi4ReadOnly(toAxiConfig(axiLite.config))

      axiNoSize >> axiMinimal

      axiLite.ar.arbitrationFrom(axiMinimal.ar)
      axiLite.ar.payload.assignSomeByName(axiMinimal.ar.payload)

      axiMinimal.r.arbitrationFrom(axiLite.r)
      axiMinimal.r.payload.assignSomeByName(axiLite.r.payload)
      axiMinimal.r.last := True

      axiLite
    }
  }

  implicit class Axi4WriteOnlyRich(axi: Axi4WriteOnly) {
    def toLite(): AxiLite4WriteOnly = {
      val axiNoId = if (axi.config.useId) {
        Axi4IdRemover(axi)
      } else {
        axi
      }
      val axiUnburst = if (axi.config.useLen) {
        Axi4Unburster(axiNoId)
      } else {
        axiNoId
      }
      val axiNoSize = if (axi.config.useSize) {
        val axiNoSize = Axi4WriteOnly(axiUnburst.config.copy(useSize = false))
        axiNoSize.aw.arbitrationFrom(axiUnburst.aw)
        axiNoSize.aw.payload.assignSomeByName(axiUnburst.aw.payload)

        axiNoSize.w.arbitrationFrom(axiUnburst.w)
        axiNoSize.w.payload.assignSomeByName(axiUnburst.w.payload)

        axiUnburst.b.arbitrationFrom(axiNoSize.b)
        axiUnburst.b.payload.assignSomeByName(axiNoSize.b.payload)
        axiNoSize
      } else {
        axiUnburst
      }

      val axiLite = AxiLite4WriteOnly(toLiteConfig(axi.config))
      val axiMinimal = Axi4WriteOnly(toAxiConfig(axiLite.config))

      axiNoSize >> axiMinimal

      axiLite.aw.arbitrationFrom(axiMinimal.aw)
      axiLite.aw.payload.assignSomeByName(axiMinimal.aw.payload)

      axiLite.w.arbitrationFrom(axiMinimal.w)
      axiLite.w.payload.assignSomeByName(axiMinimal.w.payload)

      axiMinimal.b.arbitrationFrom(axiLite.b)
      axiMinimal.b.payload.assignSomeByName(axiLite.b.payload)

      axiLite
    }
  }

  // AxiLite ---> Axi
  implicit class AxiLite4Rich(axiLite: AxiLite4) {
    def toAxi(): Axi4 = {
      val axi = Axi4(toAxiConfig(axiLite.config))

      axi.aw.arbitrationFrom(axiLite.aw)
      axi.aw.payload.assignAllByName(axiLite.aw.payload)
      axi.w.arbitrationFrom(axiLite.w)
      axi.w.payload.assignSomeByName(axiLite.w.payload)
      axi.w.last := True
      axiLite.b.arbitrationFrom(axi.b)
      axiLite.b.payload.assignAllByName(axi.b.payload)

      axi.ar.arbitrationFrom(axiLite.ar)
      axi.ar.payload.assignAllByName(axiLite.ar.payload)
      axiLite.r.arbitrationFrom(axi.r)
      axiLite.r.payload.assignSomeByName(axi.r.payload)

      axi
    }

    def fromAxi(): Axi4 = {
      val axi = Axi4(toAxiConfig(axiLite.config))
      axi.toLite() >> axiLite
      axi
    }
  }

  implicit class AxiLite4ReadOnlyRich(axiLite: AxiLite4ReadOnly) {
    def toAxi(): Axi4ReadOnly = {
      val axi = Axi4ReadOnly(toAxiConfig(axiLite.config))

      axi.ar.arbitrationFrom(axiLite.ar)
      axi.ar.payload.assignSomeByName(axiLite.ar.payload)

      axiLite.r.arbitrationFrom(axi.r)
      axiLite.r.payload.assignSomeByName(axi.r.payload)

      axi
    }
  }

  implicit class AxiLite4WriteOnlyRich(axiLite: AxiLite4WriteOnly) {
    def toAxi(): Axi4WriteOnly = {
      val axi = Axi4WriteOnly(toAxiConfig(axiLite.config))

      axi.aw.arbitrationFrom(axiLite.aw)
      axi.aw.payload.assignSomeByName(axiLite.aw.payload)

      axi.w.arbitrationFrom(axiLite.w)
      axi.w.payload.assignSomeByName(axiLite.w.payload)

      axiLite.b.arbitrationFrom(axi.b)
      axiLite.b.payload.assignSomeByName(axi.b.payload)

      axi
    }
  }
}

package spinal.lib.memory.sdram.dfi

/**
  * Independent DDR4 command decode — a SECOND, structurally-different implementation of the
  * JESD79-4 Table 16 opcode→command mapping, used to cross-check [[DfiMemoryAgent]]'s decode
  * on live DFI traffic (see DfiControllerTester's [XDECODE] check).
  *
  * Why a second implementation helps: the agent decodes with seven boolean expressions
  * (`wrCmd = actN && ras && !cas && !weN`, ...). This object decodes with a single `match` on
  * the opcode tuple, authored from the hand-transcribed golden in [[SdramCmdTruthTable16Spec]]
  * (the in-repo transcription of the PDF) — NOT by copying the agent's boolean chain. A
  * transcription slip in one form is unlikely to replicate in the other, and the two must agree
  * on every live command or the cross-check fails.
  *
  * Scope of the independence (stated plainly, not oversold): this catches agent-decode drift,
  * phase/CS skews, and illegal opcodes appearing on real traffic. It does NOT substitute for
  * the pin-level golden ([[SdramCmdTruthTable16Spec]] drives CmdTxd and samples the raw pins,
  * with no decoder in the loop) — that suite is the defence against a shared misconception of
  * Table 16 itself. Together they cover encode (golden) and decode (this cross-check).
  *
  * DDR4 only: the modern command bus (ACT_n present) is DDR4. Older generations use a
  * CS-strobed legacy bus and are out of scope here (the agent gates the same way via
  * `modernCmdBus`).
  */
object DfiCmdDecoder {

  sealed trait Kind
  object Kind {
    case object ACT     extends Kind
    case object WR      extends Kind
    case object RD      extends Kind
    // PRE and PREA share one label: the agent prints both as "PRE", distinguishing them by the
    // exported bank field (-1 for PREA). Keeping the same label here makes the cross-check
    // compare like-for-like.
    case object PRE     extends Kind
    case object REF     extends Kind
    case object MRS     extends Kind
    case object ZQCL    extends Kind
    case object NOP     extends Kind // (actN,ras,cas,we)=(1,1,1,1): no command
    case object ILLEGAL extends Kind // active-CS opcode not in the MC's emittable Table-16 set

    val labels: Map[Kind, String] = Map(
      ACT -> "ACT", WR -> "WR", RD -> "RD", PRE -> "PRE",
      REF -> "REF", MRS -> "MRS", ZQCL -> "ZQCL", NOP -> "NOP", ILLEGAL -> "ILLEGAL"
    )
  }

  /**
   * Decode one command-slot opcode.
   *
   * @param actN 1 = ACT_n HIGH, 0 = LOW (active-low pin)
   * @param rasN RAS_n / A16
   * @param casN CAS_n / A15
   * @param weN  WE_n / A14
   * @param a10  A10 / AP
   *
   * NOTE 1 (JESD79-4): when ACT_n = H the RAS/CAS/WE pins are command pins; when ACT_n = L they
   * become address pins A16/A15/A14 (a bank activate), so the ACT case only needs actN = 0.
   */
  def decode(actN: Int, rasN: Int, casN: Int, weN: Int, a10: Int): Kind =
    (actN, rasN, casN, weN) match {
      case (0, _, _, _)                    => Kind.ACT    // ACT_n LOW -> bank activate
      // ACT_n HIGH: RAS_n / CAS_n / WE_n are the command pins (Table 16).
      case (1, 1, 0, 0)                    => Kind.WR     // (ras,cas,we) = (1,0,0)
      case (1, 1, 0, 1)                    => Kind.RD     // (1,0,1)
      case (1, 0, 1, 0)                    => Kind.PRE    // (0,1,0); a10=H -> PREA (same label)
      case (1, 0, 0, 1)                    => Kind.REF    // (0,0,1)
      case (1, 0, 0, 0)                    => Kind.MRS    // (0,0,0)
      case (1, 1, 1, 0) if a10 == 1        => Kind.ZQCL   // (1,1,0) + A10=H
      case (1, 1, 1, 1)                    => Kind.NOP    // (1,1,1)
      // Everything else on an ACTIVE CS is not a command the MC may emit:
      //   (1,1,1,0)+a10=0 = ZQCS (short) — MC never emits (golden mcOp=None)
      //   (1,0,1,1)        = RFU / SRE / SRX — CKE-transition commands, MC never emits
      // Flagging them (rather than silently dropping, as the agent does) turns a spurious
      // appearance into a loud cross-check failure instead of a silent no-op.
      case _                               => Kind.ILLEGAL
    }
}

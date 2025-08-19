package spinal.lib.sim.protocolSim.master

import scala.language.implicitConversions
import spinal.core.sim.SimMutex

object Transfer {

  /** Get the first element from a sequence of chained transfers
    *
    * Transfers are used on the first element to boot or chain. So the first
    * transfer is returned.
    *
    * Transfers of s should be chained by increasing index.
    *
    * @see
    *   chainSeq
    *
    * @param ts
    *   chained transfer sequence
    * @return
    *   first transfer of the sequence
    */
  implicit def seq2transfer[T <: Transfer](ts: Seq[T]): T = {
    for (i <- 0 until ts.length - 1)
      require(ts(i).nextTransfer == Some(ts(i + 1)))
    ts(0)
  }

  /** Chains transfers by increasing index
    *
    * Transfers of ts must be independent.
    *
    * @param ts
    *   sequence of independent transfers
    */
  def chainSeq[T <: Transfer](ts: Seq[T]): Unit = {
    ts.foreach(t => require(t.nextTransfer.isEmpty))
    for (i <- 1 until ts.length)
      ts(0).append(ts(i))
  }
}

/** Management of a transfer by a master
  *
  * Allows to define and chain transfers into sequences.
  *
  *   - The sequence is equivalent to its first transfer.
  *   - A transfer is equivalent to the sequence starting from it.
  *
  * A transfer should not be used twice: for two equivalent transfers to happen,
  * build the transfer twice. Transfers have internal states.
  */
trait Transfer {

  /** Use callback API to run sequence in an async way
    *
    * Boots current transfer, which will boot the subsequent transfer of the
    * sequence, etc.
    */
  def boot(): Unit

  /** Blocking function to run first transfer
    *
    * Boots current transfer and waits until it endsI (may have started
    * subsequent transfers of the sequence in the mid-time)
    */
  final def run(): Unit = { boot(); await() }

  /** Blocking function to run full sequence
    *
    * Boots currents transfer and waits for the last transfer of the sequence to
    * end.
    */
  final def runAll(): Unit = { boot(); awaitAll() }

  // This mutex defines when the transfer is done
  private val mutex = SimMutex()
  // The transfer is not done by default
  mutex.lock()

  /** Blocking function to wait until transfer completion
    *
    * Waits for the first transfer to end (may have started subsequent transfers
    * of the sequence in the mid-time)
    */
  final def await(): Unit = {
    mutex.lock()
    mutex.unlock()
  }

  /** Declare the transfer as done */
  protected final def done(): Unit = mutex.unlock()

  /** Blocking function to wait until transfer chain completion
    *
    * Waits until the last transfer of the sequence to ends.
    */
  final def awaitAll(): Unit = lastTransfer.await()

  /** Is the transfer done?
    *
    * @return
    *   true if the transfer is done
    */
  def isDone: Boolean = !mutex.locked

  /** Is the sequence done?
    *
    * @return
    *   true if all the chain is done else false
    */
  final def allDone: Boolean = lastTransfer.isDone

  private var nextTransfer: Option[Transfer] = None
  private def lastTransfer: Transfer = nextTransfer match {
    case Some(next) => next.lastTransfer
    case None       => this
  }

  /** Is it the last transfer?
    *
    * @return
    *   true if there is no next transfer
    */
  final def isLast: Boolean = nextTransfer.isEmpty

  /** Boots the next transfer to run if there is one, else run the default given
    * transfer
    */
  protected final def bootNextOr(default: Transfer) =
    nextTransfer.getOrElse(default).boot()

  /** Boots the next transfer to run if there is one, else die */
  protected final def bootNextOrDie() =
    nextTransfer.foreach { t => t.boot() }

  /** Adds a transfer at the end of the sequence */
  final def append(transfer: Transfer): Unit =
    lastTransfer.nextTransfer = Some(transfer)

  /** Adds a transfer at the end of the sequence (to be chained)
    *
    * It returns the first transfer but allDone can be used to check if the
    * whole chain completed.
    *
    * {{{
    * val t1, t2, t3 = newTransfer()
    * val sequence = t1 >> t2 >> t3
    * // sequence is t1
    * // so sequence.boot() is t1.boot()
    * // and sequence.allDone() is t1.allDone(), which is t3.done()
    * }}}
    *
    * @param transfer
    *   the transfer to append
    * @return
    *   the first transfer
    */
  final def >>(transfer: Transfer): this.type = {
    append(transfer)
    this
  }
}

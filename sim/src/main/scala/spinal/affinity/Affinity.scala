package spinal.affinity

object Affinity {
  var warningFired = false
  def apply(cpuId : Int) {
    try {
      if(!warningFired) net.openhft.affinity.Affinity.setAffinity(cpuId)
    } catch{
      case _ : Throwable => if(!warningFired){
        warningFired = true
        println("[Warning] net.openhft.affinity.Affinity.setAffinity isn't working (this reduce performances)\n")
      }
    }
  }


  def apply(set : java.util.BitSet) {
    try {
      if(!warningFired) net.openhft.affinity.Affinity.setAffinity(set)
    } catch{
      case _ : Throwable => if(!warningFired){
        warningFired = true
        println("[Warning] net.openhft.affinity.Affinity.setAffinity isn't working (this reduce performances)\n")
      }
    }
  }
}

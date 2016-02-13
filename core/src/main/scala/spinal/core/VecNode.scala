package spinal.core

/**
 * Created by PIC32F_USER on 12/02/2016.
 */
class VecNode extends Node{
  override private[core] def calcWidth: Int = WidthInfer.inputMaxWidth(this)*inputs.length
  override private[core] def normalizeInputs: Unit = InputNormalize.inputWidthMax(this)
}

package spinal.idslplugin

trait PostInitCallback {
  def postInitCallback(): this.type
}

trait ValCallback {
  def valCallback[T](ref: T, name: String): T
}

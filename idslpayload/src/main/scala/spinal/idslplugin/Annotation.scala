package spinal.idslplugin

trait PostInitCallback {
  def postInitCallback(): this.type
  def postInitCallbackCaller(): this.type = postInitCallback()
}

trait ValCallback {
  def valCallback[T](ref: T, name: String): T
}

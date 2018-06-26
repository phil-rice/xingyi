package one.xingyi.core.optics


object Validators{
  def noValidator[T,Issue]: Validator[T,Issue] = _ => _ => List()
}

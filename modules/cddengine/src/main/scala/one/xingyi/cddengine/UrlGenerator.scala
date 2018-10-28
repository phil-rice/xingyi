package one.xingyi.cddengine
import one.xingyi.core.strings.Strings



object RenderingConfig {
  implicit val default = RenderingConfig()
}
case class RenderingConfig(rootTraceDirectory: String = "target/cdd/trace", rootPrintDirectory: String = "target/cdd/print")

trait UrlGenerator[T] extends (T => String) {
  def trace(prefix: String, t: T)(implicit renderConfig: RenderingConfig) = Strings.uri(renderConfig.rootTraceDirectory, prefix, apply(t))
  def print(prefix: String, t: T)(implicit renderConfig: RenderingConfig) = Strings.uri(renderConfig.rootPrintDirectory, prefix, apply(t))
}


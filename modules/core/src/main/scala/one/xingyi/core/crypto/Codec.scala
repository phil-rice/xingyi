package one.xingyi.core.crypto

import java.util.Base64

trait Codec[From, To] {
  def forwards: From => To
  def backwards: To => From
}
trait Base64Codec extends Codec[Array[Byte], String]
object Base64Codec {
  implicit def defaultBase64Codec = new SimpleBase64Codec
}
class SimpleBase64Codec extends Base64Codec {
  val encoder = Base64.getUrlEncoder()
  val decoder = Base64.getUrlDecoder()
  override def forwards: Array[Byte] => String = from => new String(encoder.encode(from), "UTF-8")
  override def backwards: String => Array[Byte] = from => decoder.decode(from.getBytes("UTF-8"))
}





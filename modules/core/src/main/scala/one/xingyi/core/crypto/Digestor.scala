package one.xingyi.core.crypto

import java.security.MessageDigest

trait Digestor extends (String => String)
object Digestor {
  implicit def default(implicit toBase64: Base64Codec): Digestor = new Sha256Digestor

  def constant[T](string: String): Digestor = { _ => string }
}
class Sha256Digestor(implicit toBase64: Base64Codec) extends Digestor {
  def toBytes: String => Array[Byte] = s => s.getBytes("UTF-8")
  def digest: Array[Byte] => Array[Byte] = MessageDigest.getInstance("SHA-256").digest

  override def apply(s: String): String = toBase64.forwards(digest(toBytes(s)))
}




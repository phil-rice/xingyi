/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.reflection

class AddedFinderNotActuallAnException extends Exception

trait IsDefinedInSourceCodeAt[T] extends (T => DefinedInSourceCodeAt)

object DefinedInSourceCodeAt {
  protected val defaultStackTraceOffset = 5

  def apply[T](t: T)(implicit definedInSourceCodeAt: IsDefinedInSourceCodeAt[T]) = definedInSourceCodeAt(t)
  def definedInSourceCodeAt(stackTraceOffset: Int = defaultStackTraceOffset) =
    new SingleDefinedInSourceCodeAt(new AddedFinderNotActuallAnException().getStackTrace()(stackTraceOffset))

//  def toSeq: DefinedInSourceCodeAt => Seq[SingleDefinedInSourceCodeAt] = {case s: SingleDefinedInSourceCodeAt => List(s); case CompositeDefinedInSourceCodeAt(defined) => defined}
//  implicit val semiGroupForDefinedAt: SemiGroup[DefinedInSourceCodeAt] = (t1, t2) => CompositeDefinedInSourceCodeAt(toSeq(t1) ++ toSeq(t2))
}

object DefinedInSourceCodeAtLanguage extends DefinedInSourceCodeAtLanguage
trait DefinedInSourceCodeAtLanguage {
  implicit class DefinedInSourceCodeAtOps[T](t: T)(implicit definedAt: IsDefinedInSourceCodeAt[T]) {
    def definedInSourceCodeAt = definedAt(t)
  }
}

trait DefinedInSourceCodeAt

class SingleDefinedInSourceCodeAt(val st: StackTraceElement) extends DefinedInSourceCodeAt {
  override lazy val toString = {
    val s = st.toString
    val i = s.lastIndexOf("(")
    s.substring(i)
  }

  override def equals(other: Any) = other match {
    case d: DefinedInSourceCodeAt => d.toString == toString && d.getClass == getClass
    case _ => false
  }

  override def hashCode = toString.hashCode
}

case class CompositeDefinedInSourceCodeAt(defined: Seq[SingleDefinedInSourceCodeAt]) extends DefinedInSourceCodeAt {
  override def toString: String = s"Defined at[${defined.map(_.toString).mkString(",")}]"
}

/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.closable


trait Closer[T] extends (T => Unit)
object Closer {
  implicit def tupleCloser[T1, T2](implicit closer1: Closer[T1], closer2: Closer[T2]): Closer[(T1, T2)] = tuple => try closer2(tuple._2) finally closer1(tuple._1)
}

class ClosableFunction1[T1, T2](raw: T1 => T2)(implicit closer: Closer[T1]) extends (T1 => T2) {
  override def apply(t1: T1): T2 = try raw(t1) finally closer(t1)
  def and[T3](fn: T2 => T3)(implicit closer2: Closer[T2]) = new ClosableFunction2[T1, T2, T3](raw, fn)
  case class makeBoth[T3](fna: T2 => T3)(implicit closer2: Closer[T2]) {
    def and[T4](fnb: T2 => T4) = new ClosableFunction2[T1, T2, (T3, T4)](raw, { t2: T2 => (fna(t2), fnb(t2)) })

  }
  def thenDo[T3](fn: T2 => T3)(implicit closer2: Closer[T2]): T1 => T3 = and(fn)
}
class ClosableFunction2[T1, T2, T3](raw1: T1 => T2, raw2: T2 => T3)(implicit closer1: Closer[T1], closer2: Closer[T2]) extends (T1 => T3) {
  override def apply(t1: T1): T3 = try {
    val t2 = raw1(t1)
    try raw2(t2) finally closer2(t2)
  } finally closer1(t1)
  def and[T4](fn: T3 => T4)(implicit closer3: Closer[T3]) = new ClosableFunction3(raw1, raw2, fn)
  def thenDo[T4](fn: T3 => T4)(implicit closer3: Closer[T3]): T1 => T4 = and(fn)
}

class ClosableFunction3[T1, T2, T3, T4](raw1: T1 => T2, raw2: T2 => T3, raw3: T3 => T4)(implicit closer1: Closer[T1], closer2: Closer[T2], closer3: Closer[T3]) extends (T1 => T4) {
  override def apply(t1: T1): T4 = try {
    val t2: T2 = raw1(t1)
    try {
      val t3: T3 = raw2(t2)
      try raw3(t3) finally closer3(t3)
    } finally closer2(t2)
  } finally closer1(t1)
  def and[T5](fn: T4 => T5)(implicit closer4: Closer[T4]) = new ClosableFunction4[T1, T2, T3, T4, T5](raw1, raw2, raw3, fn)
  def thenDo[T5](fn: T4 => T5)(implicit closer4: Closer[T4]): T1 => T5 = and(fn)
}


class ClosableFunction4[T1, T2, T3, T4, T5](raw1: T1 => T2, raw2: T2 => T3, raw3: T3 => T4, raw4: T4 => T5)(implicit closer1: Closer[T1], closer2: Closer[T2], closer3: Closer[T3], closer4: Closer[T4]) extends (T1 => T5) {
  override def apply(t1: T1): T5 = try {
    val t2: T2 = raw1(t1)
    try {
      val t3: T3 = raw2(t2)
      try {
        val t4: T4 = raw3(t3)
        try raw4(t4) finally closer4(t4)
      } finally closer3(t3)
    } finally closer2(t2)
  } finally closer1(t1)
}

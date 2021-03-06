/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.optics

object Lens {
  implicit def identity[X] = Lens[X, X](a => a, (a, b) => b)
  def cast[X, Y] = Lens[X, Y](a => a.asInstanceOf[Y], (a, b) => b.asInstanceOf[X])

  def firstItemL[T]: Lens[List[T], T] = Lens(_.head, (ts, t) => t :: ts.tail)
def apply[A,B](get: A=>B, set: (A,B)=>A): Lens[A,B] = SimpleLens(get,set)
}
trait Lens[A, B] {
  def get: A => B
  def set: (A, B) => A
  def setFn: B => A => A = { b => a => set(a, b) }
  def apply(whole: A): B = get(whole)
  def map(a: A, f: B => B): A = set(a, f(get(a)))
  def map(f: B => B): A => A = a => set(a, f(get(a)))
  def compose[C](that: Lens[C, A]) = SimpleLens[C, B](c => get(that.get(c)), (c, b) => that.map(c, set(_, b)))
  def andThen[C](that: Lens[B, C]) = that compose this
  def andGet[C](fn: B => C) = get andThen fn
}


trait DelegateLens[A,B] extends Lens[A,B]{
  def lens: Lens[A,B]
  override def get: A => B = lens.get
  override def set: (A, B) => A = lens.set
}
case class SimpleLens[A, B](get: A => B, set: (A, B) => A) extends Lens[A,B]


/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.local

import one.xingyi.core.reflection.ClassTags

import scala.annotation.implicitNotFound
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.DynamicVariable

class LocalOpsForScalaFuture extends LocalOps[Future] {
  val localValues: DynamicVariable[Map[Class[_], Any]] = new DynamicVariable(Map())
  override def get[V: ClassTag]() = localValues.value.get(ClassTags.clazz).asInstanceOf[Option[V]]
  override def put[V: ClassTag](v: V) = localValues.value = localValues.value + (ClassTags.clazz -> v)
  override def clear[V: ClassTag]() = localValues.value = Map()
}

object LocalOpsForScalaFuture{
  implicit val localOpsForScalaFuture = new LocalOpsForScalaFuture
}

class ExecutionContextWithLocal(executionContext: ExecutionContext)(implicit localOps: LocalOpsForScalaFuture) extends ExecutionContext {
  override def execute(task: Runnable) {
    val copyValue = localOps.localValues.value
//    println(s" in execute. Local is $localOps CopyValue is $copyValue")
    executionContext.execute(new Runnable {
      override def run = localOps.localValues.withValue(copyValue)(task.run)

    })
  }
  override def reportFailure(cause: Throwable) = executionContext.reportFailure(cause)
}


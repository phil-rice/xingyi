package org.validoc.utils.local

import java.util.concurrent.atomic.AtomicInteger

import org.validoc.utils.reflection.ClassTags

import scala.annotation.implicitNotFound
import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag
import scala.util.DynamicVariable
import scala.language.implicitConversions

class LocalOpsForScalaFuture extends LocalOps {
  val localValues: DynamicVariable[Map[Class[_], Any]] = new DynamicVariable(Map())
  override def get[V: ClassTag]() = localValues.value.get(ClassTags.clazz).asInstanceOf[Option[V]]
  override def put[V: ClassTag](v: V) = localValues.value = localValues.value + (ClassTags.clazz -> v)
  override def clear[V: ClassTag]() = localValues.value = Map()
}

object LocalOpsForScalaFuture {
  implicit val localOps = new LocalOpsForScalaFuture
  implicit def executionContext(implicit executionContext: ExecutionContext, localOps: LocalOpsForScalaFuture) = new ExecutionContextWithLocal(executionContext)
}

@implicitNotFound("""ExectuionWithLocal not found. Usually this is because there isn't an execution context is scope.""")
class ExecutionContextWithLocal(executionContext: ExecutionContext)(implicit localForScalaFuture: LocalOpsForScalaFuture) extends ExecutionContext {
  override def execute(task: Runnable) {
    val copyValue = localForScalaFuture.localValues.value
    executionContext.execute(new Runnable {
      override def run = {
        localForScalaFuture.localValues.value = copyValue
        task.run
      }
    })
  }
  override def reportFailure(cause: Throwable) = executionContext.reportFailure(cause)
}

object ExecutionContextWithLocal {
  implicit def default(executionContext: ExecutionContext) = new ExecutionContextWithLocal(executionContext)
}
package org.validoc.utils.local

import org.validoc.utils.reflection.ClassTags

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

@implicitNotFound("""ExecutionContextWithLocal not found. Usually this is because there isn't an execution context is scope.""")
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

object ExecutionContextWithLocal {
  implicit def default(executionContext: ExecutionContext) = new ExecutionContextWithLocal(executionContext)
}
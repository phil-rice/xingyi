package org.validoc.utils.functions

import org.validoc.utils.monads.Monad

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.higherKinds
import scala.language.postfixOps

object EriksQuestions extends App with NestedMonadLanguage {

  import Monad._

  import scala.concurrent.ExecutionContext.Implicits._

  val multiBoxedA = Future(Option(5))
  val multiBoxedB = Future(Option(3))

  val result1 = (multiBoxedA, multiBoxedB) merge (_ + _)
  val result = (multiBoxedA ~~> (_ + 2) ~~> (_ + 3), multiBoxedB) merge (_ + _)

  println(Await.result(multiBoxedA ~==> (_ => multiBoxedB), 5 seconds))

  println(Await.result(result, 5 seconds))

}

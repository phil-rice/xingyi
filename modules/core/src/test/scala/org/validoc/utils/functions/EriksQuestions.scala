package org.validoc.utils.functions

import org.validoc.utils.monads.Monad

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.higherKinds


object EriksQuestions extends App with NestedMonadLanguage {

  import Monad._

  import scala.concurrent.ExecutionContext.Implicits._

  val multiBoxedA = Future(Option(5))
  val multiBoxedB = Future(Option(3))

  val result = (multiBoxedA ~~> (_ + 2) ~~> (_ + 3), multiBoxedB) merge (_ + _)

  println(Await.result(result, 5 seconds))

}

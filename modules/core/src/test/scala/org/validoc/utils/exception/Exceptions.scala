package org.validoc.utils.exception

case class MyFirstException(msg: String, cause: Throwable) extends Exception(msg, cause)

case class MySecondException(msg: String, cause: Throwable) extends Exception(msg, cause)


abstract class MyRemoteException(msg: String) extends Exception(msg)

case class MyFirstRemoteException(msg: String, cause: Throwable) extends MyRemoteException(msg)

object someObject {

  def someMEthod() = {
    class SomeNewException extends Exception
  }

  val myFunction: (Int, String) => String = {
    (theInt, theString) => theInt + theString
  }

  def myMethod(a: Int, s: String): String = a + s


  def myMethodCurried(a: Int)(s: String): String = a + s
  val x = myMethodCurried(1)
  println(x("someString"))

  val myFunctionCurried: Int => (String => String) = {
    a: Int => { s: String => a + s }
  }

  myFunction(1, "someString")

  myMethod(1, "someString")


  def function(map: Map[Int, String])(id: Int) = map(id)

  val specificFunction: (Int => String) = function(Map(1-> "one", 2-> "two"))


}
package org.validoc.utils.exception

case class MyFirstException(msg: String, cause: Throwable) extends Exception(msg, cause)

case class MySecondException(msg: String, cause: Throwable) extends Exception(msg, cause)


abstract class MyRemoteException(msg: String) extends Exception(msg)

case class MyFirstRemoteException(msg: String, cause: Throwable) extends MyRemoteException(msg)

package org.validoc.utils.aggregate

import org.validoc.utils.UtilsSpec
import org.validoc.utils.concurrency.Async
import org.validoc.utils.concurrency.Async._
import scala.language.implicitConversions
import scala.concurrent.Future

class EnrichParentChildServiceSpec extends UtilsSpec {

  behavior of "EnrichParentChildService"


  def parentService(input: String) = Future.successful(input.split(",").map(_.toInt).toList)

  def childService(input: Int) = Future.successful(s"C$input")

  it should "send the parentId to a parent service, work out the ids and go get the children and then combine them to one glorious result" in {
    implicit val hasChildren = new HasChildren[List[Int], Int] {
      override def apply(p: List[Int]): Seq[Int] = p
    }
    implicit val enricher = new Enricher[String, List[Int], String] {
      override def apply(p: List[Int],children: Seq[String]): String = s"parent($p)children(${children.mkString(",")}"
    }
    val enrichParentChildService = new EnrichParentChildService[Future, String, List[Int], Int, String, String](parentService, childService)

    enrichParentChildService("1,2,3").await shouldBe "parent(List(1, 2, 3))children(C1,C2,C3"

  }

}

class MergerServiceSpec extends UtilsSpec {
  behavior of "MergerService"

  def stringToString(s: String) = Future.successful(s"service1($s)")

  def listOfIntsToString(ints: List[Int]) = Future.successful(s"service2(${ints.mkString(",")})")


  implicit def stringToListInt(s: String) = s.split(",").map(_.toInt).toList

  it should "Call the two child services and then combine the results " in {
    val mergerService = new MergeService[Future, String, String, String, String, List[Int], String](stringToString, listOfIntsToString, (left, right) => s"$left+$right")

    mergerService("1,2,3").await shouldBe "service1(1,2,3)+service2(1,2,3)"
  }
}

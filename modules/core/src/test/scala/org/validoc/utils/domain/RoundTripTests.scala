package org.validoc.utils.domain

import org.validoc.utils.http.ContentType
import org.validoc.utils.parser.{FoundResult, ParserFinder}
import org.validoc.utils.{FromServiceResponse, ToServiceResponse, UtilsSpec}

import scala.reflect.ClassTag


/**
  * Not all domain objects will have this (or need it), but implementing this constraint (that you can round trip to and from service responses) allows the mocking services and other tests  to be written more easily
  */
abstract class ServiceRequestResponseRoundTripTests[T](implicit classTag: ClassTag[T], fromServiceResponse: FromServiceResponse[T], toServiceResponse: ToServiceResponse[T]) extends UtilsSpec {

  private val className = classTag.runtimeClass.getSimpleName
  behavior of className

  def makeSample: T


  it should s" be movable from a ${className} to a ServiceResponse and back again " in {
    val sample = makeSample
    (toServiceResponse andThen fromServiceResponse) (sample) shouldBe sample
  }

}

/**
  * Not all domain objects will have this (or need it), but implementing this constraint allows other tests to be written more easily
  */
abstract class ToServiceResponseAndParserFinderRoundTripTests[T](implicit classTag: ClassTag[T], parserFinder: ParserFinder[T], toServiceResponse: ToServiceResponse[T]) extends UtilsSpec {
  val className = classTag.runtimeClass.getSimpleName
  behavior of className

  def makeSample: T

  def contentTypes: Seq[ContentType]


  it should s" be movable from a ${className} to a ServiceResponse and back again through the parser finder " in {
    val sample = makeSample
    contentTypes.foreach {
      ct =>
        withClue(ct) {
          val serviceResponse = toServiceResponse(sample)
          parserFinder(ct, serviceResponse.body.s) shouldBe FoundResult(ct, sample)
        }
    }
  }
}

/**
  * Not all domain objects will have this (or need it),
  */
abstract class RoundTripTests[T](implicit classTag: ClassTag[T], parserFinder: ParserFinder[T], fromServiceResponse: FromServiceResponse[T], toServiceResponse: ToServiceResponse[T])
  extends ToServiceResponseAndParserFinderRoundTripTests {

  it should s" be movable from a ${className} to a ServiceResponse and back again " in {
    val sample = makeSample
    (toServiceResponse andThen fromServiceResponse) (sample) shouldBe sample
  }

}

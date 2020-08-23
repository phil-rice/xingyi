/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.scientist

import one.xingyi.core.http.{Header, ServiceResponse}
import one.xingyi.core.json.{JsonObject, JsonWriter, JsonWriterLanguage, ToJsonLib}
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.language.MonadLanguage._
import one.xingyi.core.monad.MonadCanFail

import scala.language.higherKinds

trait ResultComparator[Req, Res] {
  def compareAndReport(raw: Req, req1: Req, res1: Res, req2: Req, res2: Res)
}
object ResultComparator {
  def detailedResultsComparator[J, Req: ToJsonLib, Res: ToJsonLib](implicit resultsPrinter: ResultsPrinter, jsonWriter: JsonWriter[J]): ResultComparator[Req, Res] =
    new DetailedResultsComparator[J, Req, Res]()
}


trait CleanResultForDisplay[Res] extends (Res => Res)
object CleanResultForDisplay {
  implicit object cleanResultForDisplayForServiceResponse extends CleanResultForDisplay[ServiceResponse] {
    override def apply(v1: ServiceResponse): ServiceResponse = v1.copy(headers =
      List(
        Header("Access-Control-Allow-Origin", "*"),
        Header("Access-Control-Allow-Headers", "*"),
        Header("Content-type", v1.findHeader("Content-type").getOrElse("text/html"))))
  }
}

trait ResultsPrinter extends (String => Unit)
object ResultsPrinter {
  implicit val defaultResultsPrinter: ResultsPrinter = resultsPrinter(true)
  object NoResultsPrinter extends ResultsPrinter {
    override def apply(v1: String): Unit = {}
  }
  def resultsPrinter(print: Boolean): ResultsPrinter = if (print) println else NoResultsPrinter
}


class DetailedResultsComparator[J, Req: ToJsonLib, Res: ToJsonLib](implicit resultsPrinter: ResultsPrinter, jsonWriter: JsonWriter[J]) extends ResultComparator[Req, Res] with JsonWriterLanguage {
  override def compareAndReport(raw: Req, req1: Req, res1: Res, req2: Req, res2: Res) =
    resultsPrinter(jsonWriter(JsonObject("raw" -> toT(raw), "req1" -> req1, "res1" -> res1, "req2" -> req2, "res2" -> res2)))
}


object ProveInProduction {
  implicit def proveInProduction[M[_], Fail, Req, Res: CleanResultForDisplay](implicit monadCanFail: MonadCanFail[M, Fail], resultComparator: ResultComparator[Req, Res]): TwoServiceProcessor[M, Fail, Req, Res] =
    ProveInProduction[M, Fail, Req, Res]
}

case class ProveInProduction[M[_], Fail, Req, Res](implicit monad: MonadCanFail[M, Fail], failer: TwoServiceFailer[M, Fail], resultComparator: ResultComparator[Req, Res], cleanResultForDisplay: CleanResultForDisplay[Res]) extends TwoServiceProcessor[M, Fail, Req, Res] {
  override def selectAndTransformRequests: Req => M[(Option[Req], Option[Req])] = { req: Req =>
    val some: Option[Req] = Some(req)
    (some, some).liftM
  }

  override def postProcessResults: TwoServiceMerger[M, Req, Res] = { (raw, optMRes1, optMRes2) =>
    (optMRes1, optMRes2) match {
      case (None, None) => monad.fail(failer.nothingSelected)
      case (Some((_, mRes1)), None) => mRes1.map(cleanResultForDisplay)
      case (None, Some((_, mRes2))) => mRes2.map(cleanResultForDisplay)
      case (Some((req1, mRes1)), Some((req2, mRes2))) => join2(mRes1, mRes2).map { case (res1, res2) =>
        resultComparator.compareAndReport(raw, req1, res1, req2, res2);
        cleanResultForDisplay(res1)
      }
    }
  }
}

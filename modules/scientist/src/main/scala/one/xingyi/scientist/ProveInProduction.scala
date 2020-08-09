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
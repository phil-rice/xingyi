package org.validoc.finatra

import com.twitter.finagle.http.Request
import com.twitter.finatra.http.Controller
import com.twitter.util.Future
import org.validoc.utils.service.html.ToHtml
import org.validoc.utils.strings.IndentAndString

//class ConfigController(initialPath: String, servicesSummary: ServicesSummary[Future], rootServiceDescriptions: List[ServiceDescription[Future]])(implicit toHtml: ToHtml[IndentAndString]) extends Controller {
//  println(s"Config controller starting initialPath is $initialPath")
//  get(s"/$initialPath/structure") { request: Request =>
//    response.ok("Made it \n" + toHtml(rootServiceDescriptions.map(_.fold[IndentAndString]((sd, depth) =>
//      IndentAndString(depth, List((depth, sd.shortToString + "__" + sd.report))), 0)).addAll)).contentType("text/html")
//  }
//}

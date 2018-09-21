/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.tagless

import one.xingyi.core.endpoint.MatchesServiceRequest
import one.xingyi.core.http._
import one.xingyi.core.monad.{Monad, MonadWithException}
import one.xingyi.core.profiling.{ProfileKleisli, ProfileService, TryProfileData}
import one.xingyi.core.strings.{IndentAnd, Strings}
import one.xingyi.core.time.NanoTimeService

import scala.collection.concurrent.TrieMap
import scala.language.{higherKinds, implicitConversions}
import scala.reflect.ClassTag

trait Profiled {
  def name: String
  def description: String
  def tryProfileData: TryProfileData
  def allChildren: Seq[Profiled]
}

class Profile2[M[_] : MonadWithException] {
  type Kleisli[Req, Res] = Req => M[Res]

  //TODO Need to add children to this so that we can do an HTML dump of it.
  // But this looks like it will work!
  //So this wraps every node in out tree, and we should be able to see the web page with the profiles of everything!
  case class ProfilingWrapper[Req: ClassTag, Res: ClassTag](name: String, description: String, kleisli: Req => M[Res], children: ProfilingWrapper[_, _]*)(implicit nanoTimeService: NanoTimeService) extends PartialFunction[Req, M[Res]] with Kleisli[Req, Res] with Profiled {
    val tryProfileData = new TryProfileData
    val profiledKleisli = new ProfileService(tryProfileData)(kleisli)

    override def apply(v1: Req) = profiledKleisli(v1)
    val allChildren: Seq[Profiled] = children.flatMap(child => Seq(child) ++ child.allChildren)
    def indents[T](fn: ProfilingWrapper[_, _] => T): IndentAnd[T] = {
      IndentAnd.merge(fn(this), children.map(_.indents(fn)): _*)
    }

    import one.xingyi.core.reflection.ClassTags._

    override def toString = s"ProfilingWrapper  ${children.size} ${allChildren.size} ($name, $description) [${nameOf[Req]}, ${nameOf[Res]}]  }"
    override def isDefinedAt(x: Req) = kleisli match {
      case pf: PartialFunction[Req, _] => pf.isDefinedAt(x)
      case _ => true
    }
  }

  import one.xingyi.core.language.Language._

  def endpointForProfiler[Req, Res](name: String, interpreter: TaglessLanguage[M, ProfilingWrapper], profilingWrapper: ProfilingWrapper[Req, Res]) =
    interpreter.endpoint[ServiceRequest, ServiceResponse](name, MatchesServiceRequest.fixedPath(Get))(
      ProfilingWrapper("endpointForProfiler", name, { serviceRequest: ServiceRequest =>
        val indentAndString = profilingWrapper.indents(pf => (s"${pf.name} ${pf.description}", pf.tryProfileData.toShortString))

        val result = "<table><tr>" + indentAndString.invertIndent.toString("</tr><tr>", { case (depth, (l, r)) => s"<td>${Strings.indent("&nbsp;&nbsp;&nbsp;", depth)}$l</td><td>$r</td>" }) + "</tr></table>"
        ServiceResponse(result).liftM[M]
      }))

  def profileTransformer = new WrapperTransformer[M, Kleisli, ProfilingWrapper] {
    override def apply[Req: ClassTag, Res: ClassTag](name: String, description: String, wrapper: Kleisli[Req, Res], children: ProfilingWrapper[_, _]*): ProfilingWrapper[Req, Res] =
      ProfilingWrapper(name, description, wrapper, children: _*)

  }
  def Language(interpreter: TaglessLanguage[M, Kleisli]) = new TransformTaglessLanguage[M, Kleisli, ProfilingWrapper](interpreter, profileTransformer) {
    override def debugEndpoints(endpoints: Map[String, String])(original: ProfilingWrapper[ServiceRequest, Option[ServiceResponse]]) = {
      val endpointPath = endpoints.get("profiles").getOrElse("/debug/profiles")
      val kleisli = { serviceRequest: ServiceRequest =>
        val indentAndString = original.indents(pf => (s"${pf.name} ${pf.description}", pf.tryProfileData.toShortString))
        val result = "<table><tr>" + indentAndString.invertIndent.toString("</tr><tr>", { case (depth, (l, r)) => s"<td>${Strings.indent("&nbsp;&nbsp;&nbsp;", depth)}$l</td><td>$r</td>" }) + "</tr></table>"
        ServiceResponse(result).liftM[M]
      }
      ProfilingWrapper(endpointPath, "", interpreter.endpoint[ServiceRequest, ServiceResponse](endpointPath, MatchesServiceRequest.fixedPath(Get))(kleisli))
    }
  }

  def makeSystemAndProfileEndpoint[X](interpreter: TaglessLanguage[M, Kleisli],
                                      name: String,
                                      maker: TaglessLanguage[M, ProfilingWrapper] => X,
                                      endPoints: X => ProfilingWrapper[ServiceRequest, Option[ServiceResponse]]): (X, ProfilingWrapper[ServiceRequest, Option[ServiceResponse]]) = {
    val profiledAllLanguage: TaglessLanguage[M, ProfilingWrapper] = Language(interpreter)
    val result: X = maker(profiledAllLanguage)
    (result, endpointForProfiler(name, profiledAllLanguage, endPoints(result)))
  }

}

abstract class WrapperTransformer[M[_], Wrapper[_, _], Wrapper2[_, _]](implicit evidence: Wrapper2[_, _] <:< Wrapper[_, _]) {
  def apply[Req: ClassTag, Res: ClassTag](name: String, description: String, wrapper: Wrapper[Req, Res], children: Wrapper2[_, _]*): Wrapper2[Req, Res]

}


class ProfileEachEndpointLanguage[M[_] : Monad, Wrapper[_, _]](interpreter: TaglessLanguage[M, Wrapper]) extends DelegatesTaglessLanguage[M, Wrapper](interpreter) {
  //TODO So here we have an interesting example of where a State monad would actually help. I think it would mean we didn't have mutable code and the interpreter wasn't stateless
  //This code is remarkably easier though...
  val map = TrieMap[String, TryProfileData]()
  override def endpoint[Req: ClassTag, Res: ClassTag](normalisedPath: String, matchesServiceRequest: MatchesServiceRequest)(raw: Wrapper[Req, Res])(implicit fromServiceRequest: FromServiceRequest[M, Req], toServiceResponse: ToServiceResponse[Res]) = {
    val data = map.getOrElseUpdate(normalisedPath + matchesServiceRequest, new TryProfileData)
    interpreter.endpoint(normalisedPath, matchesServiceRequest)(raw |+| profile(data))
  }

  def dump = ServiceResponse(Status(200), Body(map.map { case (k, v) => (f"$k%-40s" + " " + v.toShortString).replaceAll(" ", "&nbsp;") }.mkString("<br />")), ContentType("text/html "))

  def profileMetricsEndpoint(profileEndpoint: String) = function[ServiceRequest, ServiceResponse]("profileMetricsEndpoint")(_ => dump) |+| endpoint[ServiceRequest, ServiceResponse](profileEndpoint, MatchesServiceRequest.fixedPath(Get))

}


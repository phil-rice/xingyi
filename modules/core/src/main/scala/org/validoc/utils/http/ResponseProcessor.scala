package org.validoc.utils.http

import scala.language.higherKinds

trait ResponseProcessor[M[_], Fail, Req, Res] extends (ResponseState[Req] => M[Res])
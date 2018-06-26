/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core

import one.xingyi.core.http.ServiceResponse

import scala.language.existentials

//case class GatewayException(requestDetails: RequestDetails[_], serviceResponse: ServiceResponse) extends
//  Exception(s" RequestDetails $requestDetails\nResponse $serviceResponse")
//
//case class NotFoundException(requestDetails: RequestDetails[_], serviceResponse: ServiceResponse) extends Exception(s"Status 404 for $requestDetails")
//
//
//case class UnexpectedException(requestDetails: RequestDetails[_], t: Throwable) extends
//  Exception(s" RequestDetails $requestDetails\nNested: $t", t)

//class ParserException(val parserResult: ParserResult[_]) extends Exception(parserResult.toString)
//class ParserNotFoundException(val parserResult: FailedParserResult[_]) extends Exception(parserResult.toString)

case class UnexpectedParserException(serviceResponse: ServiceResponse, t: Throwable) extends Exception(serviceResponse.toString, t)

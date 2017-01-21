package org.validoc.utils

import org.validoc.utils.http.{HostName, RequestDetails, ServiceResponse, Status}

case class GatewayException(requestDetails: RequestDetails[_], serviceResponse: ServiceResponse) extends
  Exception(s" RequestDetails $requestDetails\nResponse $serviceResponse")


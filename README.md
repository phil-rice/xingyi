This project is for experimenting.

There are a few things I'm trying to do in a 'serious' application. 

Firstly: I am constantly rewriting the same code when making microservices. The frameworks change, which forces 
awkwardness, and even more challenging for reuse, the Monad used for futures changes. In the last three projects
I've had Scala Futures, Twitter Futures, Scalaz Futures and Scalaz Tasks. Code that works for one needs cut and 
pasting for another. 

This library is 'common things': Metrics/Profiling/ServeWhileStaleWithADeadTimeCutOff_Caching/Accessing other microservices
without actually tying down 'what Http library do I use', 'What parsing library do I use', or even 'What Aync strategy 
do I use'


Secondly: can I do more 'pipline based programming'. 

Before:

```
  def requestDetails(req: Req) = RequestDetails(req, s"Calling $name with $req")
  override def apply(req: Req): M[Res] = {
    val request: ServiceRequest = toRequest.toRequest(req)
    val httpReq = toHttpReq(request)
    rawClient(httpReq).transform[Res] {
      tryRes =>

        tryRes match {
          case Success(httpRes) => {
            val serviceResponse = toServiceResponse.response(httpRes)
            serviceResponse.status match {
              case Status.Ok => responseProcessor.statusOk(serviceResponse)
              case Status.NotFound => responseProcessor.statusNotFound(requestDetails(req), serviceResponse)
              case _ => responseProcessor.statusUnexpected(requestDetails(req), serviceResponse)
            }
          }.lift
          case Failure(t) => responseProcessor.exception(requestDetails(req), t).lift
        }
    }
  }
```

After (and I'd like to 'pipeline' this a little more even)
```$
  def requestDetails(req: Req) = RequestDetails(req, s"Calling $name with $req")

  def processServiceResponse(req: Req) = { serviceResponse: ServiceResponse =>
    serviceResponse.status match {
      case Status.Ok => responseProcessor.statusOk(serviceResponse)
      case Status.NotFound => responseProcessor.statusNotFound(requestDetails(req), serviceResponse)
      case _ => responseProcessor.statusUnexpected(requestDetails(req), serviceResponse)
    }
  }

  override def apply(req: Req): M[Res] = {
    (toRequest andThen toHttpReq andThen rawClient andTransformAndLift(
      responseProcessor.exception(requestDetails(req), _),
      toServiceResponse andThen processServiceResponse(req)
    )) (req)
  }
```

# Goal
So to demonstrate this I'm working on recreating (approximately) a real world microservice. Expressing what it does
and separating that from the framework that actually implements it. I want to be able to 'run it in Finatra', 
'run it in Play' and 'run it in Akka-Http' with an absolutely minimum of code in the different frameworks



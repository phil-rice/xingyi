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

After (and I'd like to find an arrow notation for transformAndList and get rid of the `(req)` at the end)
```
  override def apply(req: Req): M[Res] = {
    (toRequest ~> toHttpReq ~> rawClient transformAndLift(
      responseProcessor.exception(req),
      toServiceResponse ~> processServiceResponse(req))
      ) (req)
  }

```
The notation `~>` is just 'andThen'

Both bits of code do the following.  'turn req into a request' andThen 'turn that into whatever real world HttpRequest we have' 
then send that real HttpRequest (from 'some framework') to the raw framework client. The result that comes
back could be an exception or a value. If it's an exception send the exception to the response processor. If it's
a result turn the framework HttpResponse to our http response and then process it


# Goal
So to demonstrate this I'm working on recreating (approximately) a real world microservice. Expressing what it does
and separating that from the framework that actually implements it. I want to be able to 'run it in Finatra', 
'run it in Play' and 'run it in Akka-Http' with an absolutely minimum of code in the different frameworks



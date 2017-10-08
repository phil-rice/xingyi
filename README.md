# This project is for experimenting.

There are a few things I'm trying to do in a 'serious' application.  This library is 'common things': Metrics / Profiling / ServeWhileStaleWithADeadTimeCutOff_Caching / Accessing other microservices
without actually tying down 'what Http library do I use', 'What parsing library do I use', or even 'What Aync strategy 
do I use'

I hope that I can fully define an application and then 'lift' it into a framework such as Finatra,
AkkaHttp or Play.

##Code Reuse
I am constantly rewriting the same code when making microservices. The frameworks change, which forces 
awkwardness, and even more challenging for reuse, the Monad used for futures changes. In the last three projects
I've had Scala Futures, Twitter Futures, Scalaz Futures and Scalaz Tasks. Code that works for one needs cut and 
pasting for another. 

### What makes code reusable
My current idea of 'best practice' seems to be the idea of composing Kleisli's. A Kliesli is
a function X => M[Y] where M is (almost) a Monad. I use (almost) because even though
Scala futures are a monad, they are almost Monads.

### Kleisli Arrows and Transformations
Kleisli arrows are effectively flatMap. If I take K1 and K2 and do (K1 andThen K2) where andThen
is the arrow, I end up with a function K1 that is flatMapped to then do K2. Pretty straightforwards

I find that arrows are not a suitable composition for most things I want to. For example
when I profile a klesli I need to do something at the start, then call the original kleisli
then do something at the end. This is a common pattern with logging, caching, profiling, metrics etc

So instead I have the idea of a Kleisli transformer that 'wraps' a Klesli. It does some code
at the start, then the delegate and in the future of the delegate it does some more code. Importantly
it is common for the code in the 'after' to use values defined or created in the initial code

## Error Handling
I'm not convinced of the value of Either for error handling. As far as I can see Future already 
perfectly well defines two states: It went well and it went badly. On the last nearly ten
microservices I've not seen one occasion where it mattered very much what the type of the exception
was. Either adds extra pain to programming, and so far I've seen no benefit

But... when I work on a project, each project has it's own error handling. I would like to 
separate out the way that error handling is done from the composition itself.

So for example if I change my mind about to do some some error handling, I would like to avoid that
rippling out through the entire code base, and only impact on the place where I create the error
and the place where I consume and deal with the error.

## Not exploding too early
Runar's video https://www.youtube.com/watch?v=GqmsQeSzMdw has been very influential on me. 
I am exploring here with not actually composing services (Kleisli's) but instead creating a
ADT that represents them. So far the results are positive. I'm exploring with making that
ADT a functor, and being able to hold Payloads. The initial payload is a service description,
but this is a natural way to compose HTML or JSON representations of the data.


## Pipeline programming

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

## Here is one implementation of the 'main endpoint'


     val homePage = (
         (
           promotionHttp >--< caching("Promotion", cachingStrategy, maxSize) >--< profile("Promotion") >--< objectify[PromotionQuery, Promotion]("Promotion", ResponseProcessor.parsed),
           programmeAndProductionsHttp >--< objectify[ProductionId, Production]("Production", ResponseProcessor.parsed)
         ).enrich[EnrichedPromotion],
         (
           mostPopularHttp >--< profile("Most Popular") >--< objectify[MostPopularQuery, MostPopular]("mostPopular", ResponseProcessor.parsed) >--< caching[MostPopularQuery, MostPopular]("Most Popular", cachingStrategy, maxSize),
           programmeAndProductionsHttp >--< objectify[ProgrammeId, Programme]("Programme", ResponseProcessor.parsed)
         ).enrich[EnrichedMostPopular]
       ).merge[HomePageQuery, HomePage]
   






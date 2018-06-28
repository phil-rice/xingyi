# This project is a set of tools for building sofrware, especially microservices

It is written in such a way that hopefully it can be used with most if not all Scala web frameworks.  This library is 'common things': Metrics / Profiling / ServeWhileStaleWithADeadTimeCutOff_Caching / Accessing other microservices
without actually tying down 'what Http library do I use', 'What parsing library do I use', or even 'What Aync strategy 
do I use'

The goal of the library is allow microservices to be written in a declarative fashion, and can be 'lifted' into any webframework. 
I find myself constantly rewriting the same code when making microservices. The frameworks change, which forces 
awkwardness, and even more challenging for reuse, the Monad used for futures changes. In the last three projects
I've had Scala Futures, Twitter Futures, Scalaz Futures and Scalaz Tasks. Code that works for one needs cut and 
pasting for another. 

# Presentations

* [This is a mini training course in functional scala that leads in the direction of this library](https://docs.google.com/presentation/d/e/2PACX-1vSpB3yFpYZrVIZHfJGjrtDbm6jZXh_QZSR11QcbsBuWGzFMAdNpBgiM68JwVsl1S0JZkVlvWMUy_xRA/pub?start=false&loop=false&delayms=3000)
* [This is a presentation on why kleislis and kleisli transformers are good for microservices](https://docs.google.com/presentation/d/e/2PACX-1vTSCQp2YsdnOxsqkDU9nfGt4lyC7lRxoeCZySixtMjy7L-nmZvJGt-fpgsXlChYKyKIqrxS8PDXUaYJ/pub?start=false&loop=false&delayms=3000)
* [Dependencies are just really bad , Please see this presentation for why](https://docs.google.com/presentation/d/e/2PACX-1vQ2Rfb9crw29IVh7vg9NFngFozEtkjCbe53F89ZqgY5jCFOsLhup06Oj8sO9hcHIk3Y4r9FVCoO_qaD/pub?start=false&loop=false&delayms=3000)

# Goals
## No Dependancies 

This is especially true for things like 'which web framework do I use' (typically I am working on two or three at once, and often have 
multiple versions of the same framework), but also 'what JSON library', 'what logging framework', 'what metrics framework', The 
intention is that by using typeclasses and similar tools, we can define all the behavior and then in the 'main method' bind the
code to the actual implementations.

This hopefully answers the question 'why haven't you used Cats or Scalaz'. 

### What makes code reusable
My current idea of 'best practice' seems to be the idea of composing Kleisli's. A Kliesli is
a function X => M[Y] where M is (almost) a Monad. I use (almost) because even though
Scala futures are NOT actually a monad, they are almost Monads.

### Kleisli Arrows and Transformations
Kleisli arrows are effectively flatMap. If I take K1 and K2 and do (K1 arrow K2), I end up with a function K1 that is takes the result and flatMaps it to do K2. Pretty straightforwards

I find that these arrows are not a suitable composition for about half the things I want to. For example
when I profile a klesli I need to do something at the start, then call the original kleisli
then do something at the end. This is a common pattern with logging, caching, profiling, metrics etc

So as well as arrows I have the idea of a Kleisli transformer that 'wraps' a Klesli. It does some code
at the start, then the delegate and in the future of the delegate it does some more code. Importantly
it is common for the code in the 'after' to use values defined or created in the initial code.

## Error Handling
I'm not convinced of the value of Either for error handling. As far as I can see Future already 
perfectly well defines two states: It went well and it went badly. On the last nearly ten
microservices I've not seen one occasion where it mattered very much what the type of the exception
was. Either adds significant extra pain to programming, and so far I've seen no benefit

But... when I work on a project, each project has it's own error handling. I would like to 
separate out the way that error handling is done from the composition itself. This means that if
the project is using Either, or an EitherTransformer or Futures or their own ADT, it doesn't matter to this code

SMy goal is that if I change my mind about to do some some error handling, I would like to avoid that
rippling out through the entire code base, and only impact on the place where I create the error
and the place where I consume and deal with the error.

## Not exploding too early
Runar's video https://www.youtube.com/watch?v=GqmsQeSzMdw has been very influential on me. 

This is the subproject 'tagless' that isn't actually needed, but I think offers significant benefits at the
cost of needing to explain to developers some concepts. These concepts are simple once grapsed, but like all
concepts have a learning concept associated with them. 

I am exploring here with not actually composing services (Kleisli's) but instead using Tagless
Interpreters to allow multiple interpreters. I'll expand on this more when the project is usable

## One line of code per method
I have a few that if a method has more than one line of code it's doing more than responsibility. Because of the verbosity of
scala, I have no choice sometimes. Pattern matchers are also helpful for clarity but verbose, so I'm good with those

In general though, with the exception of the 'main method' which builds everything up, I would like my code to either be functions
or just the composition of other functions.

# Sample
So to demonstrate this I'm working on recreating (approximately) a real world microservice. Expressing what it does
and separating that from the framework that actually implements it. I want to be able to 'run it in Finatra', 
'run it in Play' and 'run it in Akka-Http' with an absolutely minimum of code in the different frameworks. 

The sample module captures this.

The sampleServer lifts the sample module into the crudest web framework imaginable. I intend to have tools to lift them into 
other frameworks like Play, Finatra, Akka Http


# Why the name Xing Yi
Xing Yi is a martial arts that is based on simplicity and whole body coordination. The ideas behind it are simple, but 
take a lot of training to understand the concepts.  Functional programming is quite like this. 

Plus all the good names are gone, so I picked one that I like





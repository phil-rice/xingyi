This is an experiment with Tagless Interpreters

It isn't going very well:

The inability to 'curry' types in Scala makes using them an utter pain, and I have no idea how to test these huge blocks
of code.

The results are actually very nice. For example the ability to 'profile every node and make an endpoint' is actually really nice.
As is the ability to do things like making a debug endpoint for every endpoint. But I think the cost in Scala is currently too
high.

It might actually be easier if we used a AST where we build up the Kleislis inheriting structure.
 We can then parse the AST and either produce the real microservice or all the things we do in the tagless interpreter.
logLevel := Level.Warn

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.1")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.0")
import sbt._

object Dependencies {

  val ApacheJdbc = Seq(
    Libs.`commons-dbcp2`,
    Libs.`h2`
  )

  val CddExamples = Seq(
    Libs.`scalatest` % Test,
    Libs.`mockito` % Test
  )

  val CddMustache = Seq(Libs.mustache)

  val CddScalaTest = Seq(Libs.`scalatest`)

  val CddScenario = Seq(
    Libs.`scala-reflect`,
    Libs.`scala-compiler`
  )

  val Cep = Seq(
    Libs.`scala-reflect`,
    Libs.`scala-compiler`
  )

  val Core = Seq(
    Libs.`scalatest` % Test,
    Libs.`mockito` % Test
  )

  val Finatra = Seq(
    FinatraLibs.`finatra-http`,
    FinatraLibs.`finatra-http` % Test,
    FinatraLibs.`inject-server` % Test,
    FinatraLibs.`inject-app` % Test,
    FinatraLibs.`inject-core` % Test,
    FinatraLibs.`inject-modules` % Test,
    FinatraLibs.`finatra-jackson` % Test,
    Libs.`guice` % Test,
    FinatraLibs.`finatra-http` % Test classifier "tests",
    FinatraLibs.`inject-server` % Test classifier "tests",
    FinatraLibs.`inject-app` % Test classifier "tests",
    FinatraLibs.`inject-core` % Test classifier "tests",
    FinatraLibs.`inject-modules` % Test classifier "tests",
    FinatraLibs.`finatra-jackson` % Test classifier "tests",
    Libs.`guice` % Test classifier "tests"
  )

  val Jdbc = Seq(
    Libs.`scalatest` % Test,
    Libs.`mockito` % Test
  )

  val Json4s = Seq(Libs.`json4s`)

  val Sample = Seq(
    Libs.`scalapact-scalatest` % Test,
    Libs.`junit` % Test
  )

  val Tagless = Seq(
    Libs.`scalatest` % Test,
    Libs.`mockito` % Test
  )
}

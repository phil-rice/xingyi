package one.xingyi.core.orm

import javax.sql.DataSource
import one.xingyi.core.UtilsSpec
import one.xingyi.core.closable.ClosableM
import one.xingyi.core.jdbc.{DatabaseSourceFixture, Jdbc, JdbcOps}
import one.xingyi.core.json.{JsonObject, JsonParser}
import org.scalatest.Matchers

trait OrmStrategyChecker extends Matchers {
  def checkStrategy(name: String, block: List[(OrmEntity, String)], expected: List[(OrmEntity, String)]): Unit = {
    val actual = block
    val fullDescription = List(name, actual.map(t => t._1.tableName + "->" + '"' + t._2 + '"').mkString(",\n"), "\n")
    val diffs = actual.zip(expected).collect { case (a, e) if a != e => s"Actual   ${a._1.tableName} -> ${a._2}\nExpected ${e._1.tableName} -> ${e._2}\n" }
    withClue((fullDescription ::: diffs).mkString("\n")) {
      actual.zip(expected).foreach { case (a, e) =>
        a._1.tableName shouldBe e._1.tableName
        a._2 shouldBe e._2
      }
      actual shouldBe expected
    }
  }

}

abstract class SharedFastOrmTests[M[_] : ClosableM, J: JsonParser, DS <: DataSource] extends UtilsSpec  with DatabaseSourceFixture[DS]  with Jdbc {

  def setupPerson[M[_] : ClosableM](ds: DataSource)(block: => Unit)(implicit jdbcOps: JdbcOps[DataSource]): Unit
  def main: MainEntity

  implicit def maker: OrmMaker[Person]
  behavior of getClass.getSimpleName +  " get data"

  behavior of getClass.getSimpleName +  " "+  classOf[FastReaderImpl[Person]].getSimpleName

  it should "allow the items to be read through the FastReader" in {
    val reader: FastReaderImpl[Person] = FastReader[Person](OrmBatchConfig(ds, 2))
    setupPerson(ds) {
      reader(main)(0) shouldBe List(Person("Phil", Employer("Employer1"), List(Address("Phils first address"), Address("Phils second address")), List(), "philsEmail"),
        Person("Bob", Employer("Employer2"), List(), List(), "bobsEmail"))
      reader(main)(1) shouldBe List(Person("Jill", Employer("Employer1"), List(Address("Jills first address")), List(), "jillsEmail"))
      reader(main)(2) shouldBe List()
    }
  }


  it should "allow the items to be read as a stream" in {

    setupPerson(ds) {
      main.stream[Person](OrmBatchConfig(ds, 2)).toList shouldBe
        List(Person("Phil", Employer("Employer1"), List(Address("Phils first address"), Address("Phils second address")), List(), "philsEmail"),
          Person("Bob", Employer("Employer2"), List(), List(), "bobsEmail"),
          Person("Jill", Employer("Employer1"), List(Address("Jills first address")), List(), "jillsEmail"))
    }
  }
  behavior of getClass.getSimpleName +  " "+"OrmMakerForJson"

  it should "allow the items to be read as a stream of json" ignore {
    //under development
    setupPerson(ds) {
      main.stream[JsonObject](OrmBatchConfig(ds, 2)).toList shouldBe ""
    }
  }

}

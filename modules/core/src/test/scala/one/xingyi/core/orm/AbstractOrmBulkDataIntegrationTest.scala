package one.xingyi.core.orm

import java.io.ByteArrayOutputStream

import javax.sql.DataSource
import one.xingyi.core.closable.{ClosableM, SimpleClosable}
import one.xingyi.core.jdbc.DatabaseSourceFixture

import scala.language.higherKinds

abstract class AbstractOrmBulkDataIntegrationTest[M[_] : ClosableM, DS <: DataSource] extends OrmBulkDataFixture[SimpleClosable] with DatabaseSourceFixture[DS] {
  it should "stream json" in {
    implicit val ormMaker = OrmMaker("someContext", schemaForPerson)
    setupPerson(ds) {
      val List(phil, bob, jill) = main.stream[String](OrmBatchConfig(ds, 2)).toList
      checkStrings(phil,
        """{"employer":{"Employer/name":"Employer1"},
          |"email":{"ContactEmail/email":"philsEmail"},
          |"address":[{"Address/add":"Phils first address"},{"Address/add":"Phils second address"}],
          |"phone":[],
          |"Person/name":"Phil"}""".stripMargin)
      checkStrings(bob,
        """{"employer":{"Employer/name":"Employer2"},
          |"email":{"ContactEmail/email":"bobsEmail"},
          |"address":[],
          |"phone":[],
          |"Person/name":"Bob"}""".stripMargin)
      checkStrings(jill,
        """{"employer":{"Employer/name":},
          |"email":{"ContactEmail/email":"jillsEmail"},
          |"address":[{"Address/add":"Jills first address"}],
          |"phone":[],
          |"Person/name":"Jill"}""".stripMargin)
    }
  }

}

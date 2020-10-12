/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.orm

import java.io.ByteArrayOutputStream
import java.sql.ResultSet

import javax.sql.DataSource
import one.xingyi.core.CoreSpec
import one.xingyi.core.closable.ClosableM
import one.xingyi.core.jdbc.{DatabaseSourceFixture, JdbcOps}
import one.xingyi.core.json.JsonParser
import one.xingyi.core.orm.FieldType.{int, string}

import scala.language.{higherKinds, implicitConversions}

abstract class AbstractFastOrmDebugSpec[M[_] : ClosableM, J: JsonParser, DS <: DataSource] extends CoreSpec with SharedOrmFixture with FastOrmFixture[M] with CheckStrategyFixture with DatabaseSourceFixture[DS] {


  behavior of "FastOrm-Debug"

  it should "return a record describing how a main id is mapped across the object graph" in {
    val factory = new OrmDataFactoryForMainEntity()
    setupPerson(ds) {
      val mapOfArrays: Map[OrmEntity, Array[List[Any]]] = FastReader.getOneBlockOfDataFromDs(ds, main, 2)(0).map { case (e, list) => (e -> list.toArray[List[Any]]) }
//      main.describeArray(mapOfArrays, 0) shouldBe ""
      fail("writeMe")
    }
  }
}

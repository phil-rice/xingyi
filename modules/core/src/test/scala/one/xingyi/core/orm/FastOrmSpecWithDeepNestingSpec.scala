package one.xingyi.core.orm

import java.io.ByteArrayOutputStream

import javax.sql.DataSource
import one.xingyi.core.closable.ClosableM
import one.xingyi.core.jdbc.{DatabaseSourceFixture, JdbcOps}
import one.xingyi.core.json.JsonParser

import scala.language.{higherKinds, implicitConversions}

abstract class AbtractFastOrmSpecWithDeepNestingSpec[M[_] : ClosableM, J: JsonParser, DS <: DataSource](implicit jdbcOps: JdbcOps[DataSource]) extends SharedOrmFixture with SetupDatabaseForOrmFixture[DS] with DatabaseSourceFixture[DS] {

  val table1 = TableName("t1", "")
  val table2 = TableName("t2", "")
  val table3 = TableName("t3", "")
  val table4 = TableName("t4", "")

  def executeIt = { s: String => jdbcOps.executeSql(s) apply ds }
  def setupSchema(s1: Boolean, s2: Boolean, s3: Boolean, s4: Boolean)(fn: (OrmKeys[SchemaForTest], List[SchemaForTest[_]], SchemaForTest[_], SchemaForTest[_], SchemaForTest[_]) => Unit): Unit = {
    implicit def stringToSchemaForTest(s: String): (SchemaForTest[String]) = SchemaItem(s)
    val schema4 = SchemaItemWithChildren("t4", s4, List[SchemaForTest[_]]("t4/i1"))
    val schema3 = SchemaItemWithChildren("t3", s3, List[SchemaForTest[_]]("t3/h1", schema4))
    val schema2 = SchemaItemWithChildren("t2", s2, List("t2/g1", "t2/g2", schema3))
    val schemaListFort1: List[SchemaForTest[_]] = List[SchemaForTest[_]]("t1/f1", "t1/f2", schema2)
    val keysForT1: OrmKeys[SchemaForTest] = OrmKeys.fromList(schemaListFort1)
    fn(keysForT1, schemaListFort1, schema2, schema3, schema4)
  }

  behavior of "deeply nested one to many"

  it should "be loadable" in {
    setupSchema(true, true, true, true) { (keysForT1, schemaListFort1, schema2, schema3, schema4) =>
      lazy val tablesAndFieldsAndPaths: TablesAndFieldsAndPaths = EntityAndPath(keysForT1)
      lazy val ormFactory = tablesAndFieldsAndPaths.ormFactory(keysForT1)

      val entity4 = ormFactory.oneToManyEntity(table4, "a4", Keys("t4id:int"), Keys("parentId:int"), List())
      val entity3 = ormFactory.oneToManyEntity(table3, "a3", Keys("t3id:int"), Keys("parentId:int"), List(entity4))
      val entity2 = ormFactory.oneToManyEntity(table2, "a2", Keys("t2id:int"), Keys("parentId:int"), List(entity3))
      val mainEntity: EntityAndFieldsAndPath[MainEntity] = ormFactory.mainEntity(table1, "a1", Keys("t1id:int"), List(entity2))

      implicit val maker: OrmMaker[Array[Any]] = ormFactory.ormDataMaker(Map(
        entity4.entity -> keysForT1.findForT(schema4).get,
        entity3.entity -> keysForT1.findForT(schema3).get,
        entity2.entity -> keysForT1.findForT(schema2).get))

      setup(ds, mainEntity.entity) {
        executeIt(s"""insert into  t1 (t1id, f1,f2 )            values (1,    't1v1','t1v2');""")
        executeIt(s"""insert into  t2 (t2id, parentId, g1, g2 ) values (2, 1, 't2v1','t2v2');""")
        executeIt(s"""insert into  t3 (t3id, parentId, h1 )     values (3, 2, 't3v1');""")
        executeIt(s"""insert into  t4 (t4id, parentId, i1 )     values (4, 3, 't4v1');""")

        val stream = new ByteArrayOutputStream()
        mainEntity.entity.stream[Array[Any]](OrmBatchConfig(ds, 3)).foreach((ar: Array[Any]) => keysForT1.putJson(ar, stream))
        checkStrings(stream.toString,
          """{"t2":[{"t3":[{"t4":[{"t4/i1":"i1:t4v1"}],"t3/h1":"h1:t3v1"}],"t2/g1":"g1:t2v1","t2/g2":"g2:t2v2"}],"t1/f1":"f1:t1v1","t1/f2":"f2:t1v2"}""".stripMargin)
      }
    }
  }
  behavior of "deeply nested many to one"

  it should "be loadable" in {
    setupSchema(false, false, false, false) { (keysForT1, schemaListFort1, schema2, schema3, schema4) =>

      lazy val tablesAndFieldsAndPaths: TablesAndFieldsAndPaths = EntityAndPath(keysForT1)
      lazy val ormFactory = tablesAndFieldsAndPaths.ormFactory(keysForT1)

      val entity4 = ormFactory.manyToOneEntity(table4, "a4", Keys("t4id:int"), Keys("childId:int"), List())
      val entity3 = ormFactory.manyToOneEntity(table3, "a3", Keys("t3id:int"), Keys("childId:int"), List(entity4))
      val entity2 = ormFactory.manyToOneEntity(table2, "a2", Keys("t2id:int"), Keys("childId:int"), List(entity3))
      val mainEntity: EntityAndFieldsAndPath[MainEntity] = ormFactory.mainEntity(table1, "a1", Keys("t1id:int"), List(entity2))

      implicit val maker: OrmMaker[Array[Any]] = ormFactory.ormDataMaker(Map())

      setup(ds, mainEntity.entity) {
        executeIt(s"""insert into  t1 (t1id, childId, f1,f2 )            values (1, 2, 't1v1','t1v2');""")
        executeIt(s"""insert into  t2 (t2id, childId, g1, g2 )           values (2, 3, 't2v1','t2v2');""")
        executeIt(s"""insert into  t3 (t3id, childId, h1 )               values (3, 4, 't3v1');""")
        executeIt(s"""insert into  t4 (t4id, i1 )                        values (4,    't4v1');""")

        checkStrings(mainEntity.entity.stream[Array[Any]](OrmBatchConfig(ds, 3)).map(keysForT1.toJson).head,
          """{"t2":{"t3":{"t4":{"t4/i1":"i1:t4v1"},"t3/h1":"h1:t3v1"},"t2/g1":"g1:t2v1","t2/g2":"g2:t2v2"},"t1/f1":"f1:t1v1","t1/f2":"f2:t1v2"}""")
      }
    }
  }

  behavior of "deeply nested one to many and many to one"

  it should "be loadable" in {
    setupSchema(false, true, false, true) { (keysForT1, schemaListFort1, schema2, schema3, schema4) =>

      lazy val tablesAndFieldsAndPaths: TablesAndFieldsAndPaths = EntityAndPath(keysForT1)
      lazy val ormFactory = tablesAndFieldsAndPaths.ormFactory(keysForT1)

      val entity4 = ormFactory.oneToManyEntity(table4, "a4", Keys("t4id:int"), Keys("parentId:int"), List())
      val entity3 = ormFactory.manyToOneEntity(table3, "a3", Keys("t3id:int"), Keys("childId:int"), List(entity4))
      val entity2 = ormFactory.oneToManyEntity(table2, "a2", Keys("t2id:int"), Keys("parentId:int"), List(entity3))
      val mainEntity: EntityAndFieldsAndPath[MainEntity] = ormFactory.mainEntity(table1, "a1", Keys("t1id:int"), List(entity2))

      implicit val maker: OrmMaker[Array[Any]] = ormFactory.ormDataMaker(Map(
        entity4.entity -> keysForT1.findForT(schema4).get,
        entity2.entity -> keysForT1.findForT(schema2).get))

      setup(ds, mainEntity.entity) {
        executeIt(s"""insert into  t1 (t1id,                    f1,f2 )    values (1,           't1v1','t1v2');""")
        executeIt(s"""insert into  t2 (t2id, parentId, childId, g1, g2 )   values (2, 1, 3,     't2v1','t2v2');""")
        executeIt(s"""insert into  t3 (t3id,                    h1 )       values (3,           't3v1');""")
        executeIt(s"""insert into  t4 (t4id, parentId,          i1 )       values (4, 3,        't4v1');""")

        checkStrings(mainEntity.entity.stream[Array[Any]](OrmBatchConfig(ds, 3)).map(keysForT1.toJson).head,
          """{"t2":[{"t3":{"t4":[{"t4/i1":"i1:t4v1"}],"t3/h1":"h1:t3v1"},"t2/g1":"g1:t2v1","t2/g2":"g2:t2v2"}],"t1/f1":"f1:t1v1","t1/f2":"f2:t1v2"}""")
      }
    }
  }
}

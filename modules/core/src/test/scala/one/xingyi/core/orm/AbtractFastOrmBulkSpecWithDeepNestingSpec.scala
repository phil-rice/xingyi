package one.xingyi.core.orm

import javax.sql.DataSource
import one.xingyi.core.closable.ClosableM
import one.xingyi.core.jdbc.{DatabaseSourceFixture, JdbcOps}
import one.xingyi.core.json.JsonParser
import one.xingyi.core.parserAndWriter.Parser

import scala.language.{higherKinds, implicitConversions}

abstract class AbtractFastOrmBulkSpecWithDeepNestingSpec[M[_] : ClosableM, J: JsonParser, DS <: DataSource](implicit jdbcOps: JdbcOps[DataSource]) extends SharedOrmFixture with SetupDatabaseForOrmFixture[DS] with DatabaseSourceFixture[DS] {

  val alias1 = Alias("t1")
  val alias2 = Alias("t2")
  val alias3 = Alias("t3")
  val alias4 = Alias("t4")

  type JContext = String

  def executeIt = { s: String => jdbcOps.executeSql(s) apply ds }
  implicit val x = ValueFromMultipleAliasFields.valueFromMultipleTableFieldsFor[JContext, String](Parser.ParserForString)

  def setupSchema(s1: Boolean, s2: Boolean, s3: Boolean, s4: Boolean)(fn: (OrmMaker[String], OrmFactory[SchemaForTest], SchemaForTest[_], SchemaForTest[_], SchemaForTest[_], SchemaForTest[_]) => Unit): Unit = {
    implicit def stringToSchemaForTest(s: String): (SchemaForTest[String]) = SchemaItem[String](s)

    val schema4 = SchemaItemWithChildren("t4", s4, List[SchemaForTest[_]]("t4/i1"))
    val schema3 = SchemaItemWithChildren("t3", s3, List[SchemaForTest[_]]("t3/h1", schema4))
    val schema2 = SchemaItemWithChildren("t2", s2, List("t2/g1", "t2/g2", schema3))
    val schemaListFort1: List[SchemaForTest[_]] = List[SchemaForTest[_]]("t1/f1", "t1/f2", schema2)
    val schema1 = SchemaItemWithChildren("s1", false, schemaListFort1)

    implicit def toTableName(e: OrmEntity): TableName = e.tableName
    implicit def toTableNameString(e: OrmEntity): String = e.tableName.tableName

    import one.xingyi.core.map.Maps._
    implicit val arrayTableName = ArrayAliasFromMap[SchemaForTest](Map[String, Alias]().
      addIf(s2, "t2" -> Alias("t2")).
      addIf(s3, "t3" -> Alias("t3")).
      addIf(s4, "t4" -> Alias("t4")))

    val maker: OrmMaker[String] = OrmMaker[String, SchemaForTest]("someContext", schema1)
    lazy val ormFactory: OrmFactory[SchemaForTest] = OrmFactory[String, SchemaForTest](schema1)

    fn(maker, ormFactory, schema1, schema2, schema3, schema4)
  }

  behavior of "deeply nested one to many"

  it should "be loadable" in {
    setupSchema(true, true, true, true) { (maker, ormFactory, schema1, schema2, schema3, schema4) =>
      implicit val ormMaker = maker

      val entity4: OneToManyEntity = ormFactory.oneToManyEntity(alias4, "a4", Keys("t4id:int"), Keys("parentId:int"), List())
      val entity3 = ormFactory.oneToManyEntity(alias3, "a3", Keys("t3id:int"), Keys("parentId:int"), List(entity4))
      val entity2 = ormFactory.oneToManyEntity(alias2, "a2", Keys("t2id:int"), Keys("parentId:int"), List(entity3))
      val mainEntity = ormFactory.mainEntity(alias1, "a1", Keys("t1id:int"), List(entity2))

      setup(ds, mainEntity) {
        executeIt(s"""insert into  t1 (t1id, f1,f2 )            values (1,    't1v1','t1v2');""")
        executeIt(s"""insert into  t2 (t2id, parentId, g1, g2 ) values (2, 1, 't2v1','t2v2');""")
        executeIt(s"""insert into  t3 (t3id, parentId, h1 )     values (3, 2, 't3v1');""")
        executeIt(s"""insert into  t4 (t4id, parentId, i1 )     values (4, 3, 't4v1');""")

        mainEntity.stream[String](OrmBatchConfig(ds, 3)).toList shouldBe List(
          """{"t2":[{"t3":[{"t4":[{"t4/i1":"t4v1"}],"t3/h1":"t3v1"}],"t2/g1":"t2v1","t2/g2":"t2v2"}],"t1/f1":"t1v1","t1/f2":"t1v2"}""")
      }
    }
  }
  behavior of "deeply nested many to one"

  def setupNestedManyToOne(fn: (MainEntity) => OrmMaker[String] => Unit): Unit = {
    setupSchema(false, false, false, false) { (maker, ormFactory, schema1, schema2, schema3, schema4) =>
      implicit val arrayTableName = ArrayAliasFromMap[SchemaForTest](Map())
      implicit val maker: OrmMaker[String] = OrmMaker[String, SchemaForTest]("someContext", schema1)

      val entity4 = ormFactory.manyToOneEntity(alias4, "a4", Keys("t4id:int"), Keys("childId:int"), List())
      val entity3 = ormFactory.manyToOneEntity(alias3, "a3", Keys("t3id:int"), Keys("childId:int"), List(entity4))
      val entity2 = ormFactory.manyToOneEntity(alias2, "a2", Keys("t2id:int"), Keys("childId:int"), List(entity3))
      val mainEntity: MainEntity = ormFactory.mainEntity(alias1, "a1", Keys("t1id:int"), List(entity2))

      setup(ds, mainEntity) {
        executeIt(s"""insert into  t1 (t1id, childId, f1,f2 )            values (1, 2, 't1v1','t1v2');""")
        executeIt(s"""insert into  t2 (t2id, childId, g1, g2 )           values (2, 3, 't2v1','t2v2');""")
        executeIt(s"""insert into  t3 (t3id, childId, h1 )               values (3, 4, 't3v1');""")
        executeIt(s"""insert into  t4 (t4id, i1 )                        values (4,    't4v1');""")
        fn(mainEntity)(maker)
      }
    }
  }

  it should "have a bulk pointer" in {
    setupNestedManyToOne { (mainEntity) =>
      implicit ormMaker =>
        val data: Map[OrmEntity, List[List[Any]]] = FastReader.getOneBlockOfDataFromDs(ds, mainEntity, 2)(0)
        lazy val mainBulkData = MainBulkData(mainEntity, data)

        checkStrings(mainBulkData.prettyPrint(data).mkString("\n"),
          """t1/a1
            |  List(t1v1, t1v2, 2, 1)
            |t2/a2
            |  List(t2v1, t2v2, 3, 2)
            |t3/a3
            |  List(t3v1, 4, 3)
            |t4/a4
            |  List(t4v1, 4)""".stripMargin)

        val pointer0 = mainBulkData.pointer(0)

        checkStrings(pointer0.prettyPrint(""),
          """Found(nth=0, bulkData=TableName(t1,),row=Some(List(t1v1, t1v2, 2, 1)),children=
            |  Found(n=0,index=0,parentId=List(1),row=Some(List(t2v1, t2v2, 3, 2)),bulkData=t2(Some(0)),children=
            |    Found(n=0,index=0,parentId=List(2),row=Some(List(t3v1, 4, 3)),bulkData=t3(Some(0)),children=
            |      Found(n=0,index=0,parentId=List(3),row=Some(List(t4v1, 4)),bulkData=t4(Some(0)),noChildren
            |""".stripMargin)

    }
  }

  it should "be loadable" in {
    setupNestedManyToOne { (mainEntity) =>
      implicit ormMaker =>
        mainEntity.stream[String](OrmBatchConfig(ds, 3)).toList shouldBe List(
          """{"t2":{"t3":{"t4":{"t4/i1":"t4v1"},"t3/h1":"t3v1"},"t2/g1":"t2v1","t2/g2":"t2v2"},"t1/f1":"t1v1","t1/f2":"t1v2"}""")
    }
  }

  behavior of "deeply nested one to many and many to one"

  it should "be loadable" in {
    setupSchema(false, true, false, true) { (maker, ormFactory, schema1, schema2, schema3, schema4) =>
      implicit val ormMaker = maker

      val entity4 = ormFactory.oneToManyEntity(alias4, "a4", Keys("t4id:int"), Keys("parentId:int"), List())
      val entity3 = ormFactory.manyToOneEntity(alias3, "a3", Keys("t3id:int"), Keys("childId:int"), List(entity4))
      val entity2 = ormFactory.oneToManyEntity(alias2, "a2", Keys("t2id:int"), Keys("parentId:int"), List(entity3))
      val mainEntity: MainEntity = ormFactory.mainEntity(alias1, "a1", Keys("t1id:int"), List(entity2))

      setup(ds, mainEntity) {
        executeIt(s"""insert into  t1 (t1id,                    f1,f2 )    values (1,           't1v1','t1v2');""")
        executeIt(s"""insert into  t2 (t2id, parentId, childId, g1, g2 )   values (2, 1, 3,     't2v1','t2v2');""")
        executeIt(s"""insert into  t3 (t3id,                    h1 )       values (3,           't3v1');""")
        executeIt(s"""insert into  t4 (t4id, parentId,          i1 )       values (4, 3,        't4v1');""")

        mainEntity.stream[String](OrmBatchConfig(ds, 3)).toList shouldBe List(
          """{"t2":[{"t3":{"t4":[{"t4/i1":"t4v1"}],"t3/h1":"t3v1"},"t2/g1":"t2v1","t2/g2":"t2v2"}],"t1/f1":"t1v1","t1/f2":"t1v2"}""")

      }
    }
  }
}

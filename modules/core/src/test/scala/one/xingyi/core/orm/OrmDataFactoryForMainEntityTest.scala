package one.xingyi.core.orm

import one.xingyi.core.UtilsSpec
import org.mockito.Mockito

class OrmDataFactoryForMainEntityTest extends UtilsSpec {

  behavior of classOf[OrmDataFactoryForMainEntityTest].getSimpleName

  val factory = new OrmDataFactoryForMainEntity
  val fn = mock[(Int, OrmEntity, List[Any]) => Int]

  val childTable = TableName("child", "")
  val mainTable = TableName("table", "description")


  def makeEntity(child: ChildEntity): MainEntity = MainEntity(mainTable, "a", Keys("pk"), List(FieldType("data")), List(child))

  def setUpFanOut[E <: ChildEntity](child: E, array: Array[List[Any]])(block: (MainEntity, FanoutOrmData[Int, E]) => Unit): Unit = {
    val main = makeEntity(child)
    val data = mock[Map[OrmEntity, Array[List[Any]]]]
    Mockito.when(data.apply(child)).thenReturn(array)
    val childData = factory.childOrmData(main, data, fn)(child).asInstanceOf[FanoutOrmData[Int, E]]
    childData.t shouldBe child
    childData.ar shouldBe array
    childData.executeWhenMatch shouldBe fn
    block(main, childData)
  }

  def setupFanIn[E <: ChildEntity](child: E, array: Array[List[Any]])(block: (MainEntity, FanInOrmData[Int, E]) => Unit): Unit = {
    val main = makeEntity(child)
    val data = mock[Map[OrmEntity, Array[List[Any]]]]
    Mockito.when(data.apply(child)).thenReturn(array)
    val childData = factory.childOrmData(main, data, fn)(child).asInstanceOf[FanInOrmData[Int, E]]
    childData.t shouldBe child
    childData.data shouldBe array
    childData.executeWhenMatch shouldBe fn
    block(main, childData)
  }

  it should "create OrmData for the " + classOf[SameIdEntity].getSimpleName in {
    val childEntity = SameIdEntity(childTable, "c", Keys("cid"), List(FieldType("childData1"), FieldType("childData2")), List())
    setUpFanOut(childEntity, Array()) { (main, fanout) => fanout.flyweightKey shouldBe KeyString(1, 2) }
  }
  it should "create OrmData for a different " + classOf[SameIdEntity].getSimpleName in {
    val childEntity = SameIdEntity(childTable, "c", Keys("cid"), List(FieldType("childData1"), FieldType("childData2"), FieldType("childData3")), List())
    setUpFanOut(childEntity, Array()) { (main, fanout) => fanout.flyweightKey shouldBe KeyString(1, 3) }
  }
  it should "create OrmData for the " + classOf[OneToZeroOneEntity].getSimpleName in {
    val childEntity = OneToZeroOneEntity(childTable, "c", Keys("cid"), Keys("pk"), List(FieldType("childData1"), FieldType("childData2")), List())
    setUpFanOut(childEntity, Array()) { (main, fanout) => fanout.flyweightKey shouldBe KeyString(1, 2) }
  }
  it should "create OrmData for the " + classOf[OneToManyEntity].getSimpleName in {
    val childEntity = OneToManyEntity(childTable, "c", Keys("cid"), Keys("parent"), List(FieldType("childData1"), FieldType("childData2")), List())
    setUpFanOut(childEntity, Array()) { (main, fanout) => fanout.flyweightKey shouldBe KeyString(1, 2) }
  }
  it should "create OrmData for the " + classOf[ManyToOneEntity].getSimpleName in {
    val childEntity = ManyToOneEntity(childTable, "c", Keys("cid"), Keys("childId"), List(FieldType("childData1"), FieldType("childData2")), List())
    setupFanIn(childEntity, Array(List(1, 2, 3), List(2, 3, 4))) { (main, fanout) =>
      fanout.map shouldBe Map(List(3) -> List(1, 2, 3), List(4) -> List(2, 3, 4))
    }
  }

}

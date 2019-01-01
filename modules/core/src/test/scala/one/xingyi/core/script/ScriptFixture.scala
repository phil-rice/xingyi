package one.xingyi.core.script
import one.xingyi.core.json._
import one.xingyi.core.optics.Lens

import scala.language.higherKinds

trait IChild
case class ChildForTest(name: String, age: Int)
object ChildForTest {
  implicit val proof: ProofOfBinding[IChild, ChildForTest] = new ProofOfBinding
  implicit val projection = ObjectProjection[IChild, ChildForTest](ChildForTest("someName", 0),
    "name" -> StringFieldProjection(XingYiDomainStringLens(Lens[ChildForTest, String](_.name, (c, n) => c.copy(name = n))))
  )
}

trait IHouse
case class HouseForTest(houseNo: Int, postCode: String)
object HouseForTest {
  implicit val proofOfBinding: ProofOfBinding[IHouse, HouseForTest] = new ProofOfBinding
  implicit val projection = ObjectProjection(HouseForTest(123, "somePostcode"),
    "postcode" -> StringFieldProjection(XingYiDomainStringLens(Lens[HouseForTest, String](_.postCode, (h, p) => h.copy(postCode = p))))
  )
}


trait IParent

@XingYiInterface(clazzes = Array(classOf[IParent]))
trait IParentNameOps[L[_, _], P <: IParent] extends IXingYiSharedOps[L, P] {
  def nameLens: L[P, String]
}
@XingYiInterface(clazzes = Array(classOf[IParent], classOf[IHouse]))
trait IParentHouseOps[L[_, _], P <: IParent, H <: IHouse] extends IXingYiSharedOps[L, P] {
  def houseLens: L[P, H]
}
@XingYiInterface(clazzes = Array(classOf[IParent], classOf[IChild]))
trait IParentChildrenOps[L[_, _], P <: IParent, C <: IChild] extends IXingYiSharedOps[L, P] {
  def childrenLens: L[P, List[C]]
}
@XingYiInterface(clazzes = Array(classOf[IParent]))
trait IParentHousePostCodeOps[L[_, _], P <: IParent] extends IXingYiSharedOps[L, P] {
  def housePostcodeLens: L[P, String]
}

case class ParentForTest(name: String, age: Int, house: HouseForTest, children: List[ChildForTest])
object ParentForTest {
  implicit val proof: ProofOfBinding[IParent, ParentForTest] = new ProofOfBinding
  val prototype = ParentForTest("someParent", 1, HouseForTest(12, "somePostcode"), List())

  object parentNameOps extends IParentNameOps[IXingYiLens, IParent] {
    override def nameLens = XingYiDomainStringLens(Lens[ParentForTest, String](_.name, (p, n) => p.copy(name = n)))
  }
  object parentHouseOps extends IParentHouseOps[IXingYiLens, IParent, IHouse] {
    override def houseLens = XingYiDomainObjectLens(Lens[ParentForTest, HouseForTest](_.house, (p, h) => p.copy(house = h)))
  }
  object parentChildrenOps extends IParentChildrenOps[IXingYiLens, IParent, IChild] {
    //    override def houseLens = XingYiDomainObjectLens(Lens[ParentForTest, HouseForTest](_.house, (p, h) => p.copy(house = h)))
    override def childrenLens = XingYiDomainObjectLens(Lens[ParentForTest, List[ChildForTest]](_.children, (p, c) => p.copy(children = c)))
  }

  implicit val parentProjection = ObjectProjection[IParent, ParentForTest](prototype,
    "name" -> StringFieldProjection(parentNameOps.nameLens),
    "house" -> ObjectFieldProjection(parentHouseOps.houseLens),
    "children" -> ListFieldProjection(parentChildrenOps.childrenLens)

  )
}
class ParentDomainForTest1 extends DomainDefn[IParent, ParentForTest](
  "someSharedPackageName",
  List("renderer1", "renderer2"),
  List(ParentForTest.parentNameOps -> ParentForTest.parentProjection,
    ParentForTest.parentHouseOps -> ParentForTest.parentProjection
    //    ParentForTest.parentChildrenOps -> ParentForTest.parentProjection
  ), List())

class ParentDomainForTest2 extends DomainDefn[IParent, ParentForTest](
  "someSharedPackageName",
  List("renderer1", "renderer2"),
  List(ParentForTest.parentNameOps -> ParentForTest.parentProjection,
    //    ParentForTest.parentHouseOps -> ParentForTest.parentProjection,
    ParentForTest.parentChildrenOps -> ParentForTest.parentProjection
  ), List(
    new IParentHousePostCodeOps[XingYiManualPath, IParent] {
      override def housePostcodeLens: XingYiManualPath[IParent, String] =
        XingYiManualPath("lens_person_postcode_string", "stringLens",
          """function lens_person_postcode_string() { return compose(xxx(), xx())}""")
    }
  ))


trait ScriptFixture {
  val details1 = DomainDefnToDetails(new ParentDomainForTest1)
  val details2 = DomainDefnToDetails(new ParentDomainForTest2)
  val domainList = DomainList[IParent, ParentForTest](details1, details2)
  val code0 = domainList.domains(0).code
  val js0Hash = code0(Javascript).hash
  val scala0Hash = code0(ScalaCode).hash
  val code1 = domainList.domains(1).code
  val js1Hash = code1(Javascript).hash
  val scala1Hash = code1(ScalaCode).hash


}

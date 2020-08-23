/** Copyright (c) 2020, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.script
import one.xingyi.core.builder.IdLens
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
  implicit val idLens = IdLens[ParentForTest, String](_.name, (p, n) => p.copy(name = n))


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
  val dom1 = new ParentDomainForTest1
  val dom2 = new ParentDomainForTest2

  val getMethod = GetMethodData[ParentForTest]("/parent/<id>", _ => ???)
  val postMethod = PostMethodData[ParentForTest]("/parent/<id>", _ => ???)
  val putMethod = PutMethodData[ParentForTest]("/parent/<id>", (a, b) => throw new RuntimeException)

  val domAndMethods1 = DomainAndMethods(List(getMethod, postMethod), dom1)
  val domAndMethods2 = DomainAndMethods(List(getMethod, postMethod), dom2)
  val listOfDomainAndMethods1 = ListofDomainAndMethods(domAndMethods1, List(domAndMethods1, domAndMethods2))
  val listofDomainAndMethods2 = ListofDomainAndMethods(domAndMethods2, List(domAndMethods1, domAndMethods2))


  val details1 = DomainDefnToDetails(dom1)
  val details2 = DomainDefnToDetails(dom2)
  val domainList = DomainList[IParent, ParentForTest](details1, details2)
  val code0 = domainList.domains(0).code
  val js0Hash = code0(Javascript).hash
  val scala0Hash = code0(ScalaCode).hash
  val code1 = domainList.domains(1).code
  val js1Hash = code1(Javascript).hash
  val scala1Hash = code1(ScalaCode).hash


  val sharedPackageName = new ParentDomainForTest1().sharedPackageName
  val domainCd1 = DomainCD("one.xingyi.core.script", sharedPackageName, "ParentDomainForTest1", DomainDefnToCodeDom.imports(sharedPackageName),
    List(EntityCD("House", "one.xingyi.core.script.IHouse"),
      EntityCD("Child", "one.xingyi.core.script.IChild"),
      EntityCD("Parent", "one.xingyi.core.script.IParent")),
    List(InterfaceCD("IParentNameOps", "ParentNameOps", List("Parent"), List(LensMethodCD("nameLens", "lens_parent_name_string", "stringLens[Parent]"))),
      InterfaceCD("IParentHouseOps", "ParentHouseOps", List("Parent", "House"), List(LensMethodCD("houseLens", "lens_parent_house_house", "objectLens[Parent,House]")))))


  val domainCd2 = DomainCD("one.xingyi.core.script", sharedPackageName, "ParentDomainForTest2", DomainDefnToCodeDom.imports(sharedPackageName),
    List(EntityCD("House", "one.xingyi.core.script.IHouse"),
      EntityCD("Child", "one.xingyi.core.script.IChild"),
      EntityCD("Parent", "one.xingyi.core.script.IParent")),
    List(InterfaceCD("IParentNameOps", "ParentNameOps", List("Parent"), List(LensMethodCD("nameLens", "lens_parent_name_string", "stringLens[Parent]"))),
      InterfaceCD("IParentChildrenOps", "ParentChildrenOps", List("Parent", "Child"), List(LensMethodCD("childrenLens", "lens_parent_children_childlist", "listLens[Parent,Child]")))))

  val domainDd1 = DomainDD("ParentDomainForTest1",
    List(MethodDD("Get", "/parent/<id>"), MethodDD("Post", "/parent/<id>")),
    List(EntityDD("House", "one.xingyi.core.script.IHouse"),
      EntityDD("Child", "one.xingyi.core.script.IChild"),
      EntityDD("Parent", "one.xingyi.core.script.IParent")),
    Map("one.xingyi.core.script.IParent" -> List(LensMethodDD("nameLens", "lens_parent_name_string"), LensMethodDD("houseLens","lens_parent_house_house"))),
    List("renderer1", "renderer2"))

  val domainDd2 =
    DomainDD("ParentDomainForTest2",
      List(MethodDD("Get", "/parent/<id>"), MethodDD("Post", "/parent/<id>")),
      List(EntityDD("House", "one.xingyi.core.script.IHouse"),
        EntityDD("Child", "one.xingyi.core.script.IChild"),
        EntityDD("Parent", "one.xingyi.core.script.IParent")),
      Map("one.xingyi.core.script.IParent" ->
        List(LensMethodDD("nameLens","lens_parent_name_string"),
          LensMethodDD("childrenLens","lens_parent_children_childlist"))),
      List("renderer1", "renderer2"))

}

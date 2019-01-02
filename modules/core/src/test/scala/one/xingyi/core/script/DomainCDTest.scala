package one.xingyi.core.script
import one.xingyi.core.UtilsSpec

class CodeDomTest extends UtilsSpec with ScriptFixture {

  behavior of "DomainCd"

  val domainDefnToCodeDom = implicitly[DomainDefnToCodeDom]
  val domainCdToScala = implicitly[ToScalaCode[DomainCD]]

  it should "have a smoke test " in {
    domainCdToScala(domainCd1).noWhiteSpace shouldBe
      """package one.xingyi.core.script
        |import someSharedPackageName._
        |import one.xingyi.core.json.IXingYiHeaderFor
        |import one.xingyi.core.optics.Lens
        |import one.xingyi.core.script.{Domain,DomainMaker,IXingYi,ServerDomain}
        |object ParentDomainForTest1 extends ServerDomain{
        |  def lens=List("lens_parent_name_string","lens_parent_house_house")
        |}
        |case class House (mirror: Object) extends Domain with one.xingyi.core.script.IHouse
        |object House {
        |  implicit object default extends DomainMaker[House] {
        |    override def create(mirror: Object): House = House(mirror)
        |  }
        |}
        |case class Child (mirror: Object) extends Domain with one.xingyi.core.script.IChild
        |object Child {
        |  implicit object default extends DomainMaker[Child] {
        |    override def create(mirror: Object): Child = Child(mirror)
        |  }
        |}
        |case class Parent (mirror: Object) extends Domain with one.xingyi.core.script.IParent
        |object Parent {
        |  implicit object default extends DomainMaker[Parent] {
        |    override def create(mirror: Object): Parent = Parent(mirror)
        |  }
        |}
        |object ParentNameOps {
        |   implicit def hasHeader: IXingYiHeaderFor[ParentNameOps] =  () => List("lens_parent_name_string")
        |}
        |class ParentNameOps(implicit val xingYi: IXingYi) extends IParentNameOps[Lens, Parent] {
        |   def nameLens = xingYi.stringLens[Parent]("lens_parent_name_string")
        |}
        |object ParentHouseOps {
        |   implicit def hasHeader: IXingYiHeaderFor[ParentHouseOps] =  () => List("lens_parent_house_house")
        |}
        |class ParentHouseOps(implicit val xingYi: IXingYi) extends IParentHouseOps[Lens, Parent,House] {
        |   def houseLens = xingYi.objectLens[Parent,House]("lens_parent_house_house")
        |}""".stripMargin.noWhiteSpace
  }
}

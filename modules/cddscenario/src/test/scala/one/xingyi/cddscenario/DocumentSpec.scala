package one.xingyi.cddscenario
import one.xingyi.core.UtilsSpec

class DocumentSpec  extends UtilsSpec{

  behavior of "Document"

  it should "have helper methods that allow easy creation of internet and paper documents" in {
    Document.internet("someRef") shouldBe InternetDocument("someRef", "someRef")
    Document.internet("someName", "someRef") shouldBe InternetDocument("someName", "someRef")
    Document.paper("someName") shouldBe PaperDocument("someName", "")
    Document.paper("someName", "someRef") shouldBe PaperDocument("someName", "someRef")
  }

  behavior of "Reference"
  it should "have a helper method that allows a reference to be easily made" in {
    val doc = InternetDocument("someName", "someRef")
    Reference(doc) shouldBe Reference(doc, None)
    Reference(doc, "internalRef") shouldBe Reference(doc, Some("internalRef"))
  }
}

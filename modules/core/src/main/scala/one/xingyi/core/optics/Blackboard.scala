package one.xingyi.core.optics

import java.util.ResourceBundle

import one.xingyi.core.builder.{HasId, No, RememberingAggregator2, Yes}
import one.xingyi.core.json.{JsonList, JsonObject, JsonValue}
import one.xingyi.core.language.AnyLanguage._
import one.xingyi.core.map.Maps._
import one.xingyi.core.misc.IdMaker
import one.xingyi.core.json.JsonLanguage._
//desired usage
//given a composite object like 'entity' being able to go entityB.financialData.profitAndLoss.nettIncome and getting a  lens to the net income would be cool
//even better entityB.financialData(2017).profitAndLoss.nettIncome
//As well as that given the string "entityB.financialData(2017).profitAndLoss.nettIncome" we would want to be able to get it
//issues are around optionals (which are important)
//we also want to consider a validater story.. so we can attack validators at several levels and have a validate report if the lens isn't working
//A string story is good as well. This lets us write CRUD websites trivially easy

//I can get a very long way with just the string story.
//Need to consider how to deal with financialData(2017)! But ignore that for now too

case class ValidateProblem(s: String)
trait ToJsonWithPrefix[T] extends ((List[String], T) => JsonObject)


case class BlackboardToJsonData[T, Issue](strings: List[String], entity: T, blackboard: Blackboard[T, Issue])

trait MessageGetter extends (String => String)
object MessageGetter {
  def bundleMessageGetter(bundle: ResourceBundle): MessageGetter = key => bundle.getString(key)
  def mapGetter(map: Map[String, String]): MessageGetter = key => map(key)
  def fnGetter(fn: String => String): MessageGetter = key => fn(key)
}

class BlackboardToJson[Issue](implicit messageGetter: MessageGetter) {
  def linkJson[T, Child](blackboardToJsonData: BlackboardToJsonData[T, Issue], name: String, link: BlackboardLink[T, Child, Issue]): JsonValue = {
    import blackboardToJsonData._
    apply(new BlackboardToJsonData[Child, Issue](strings :+ name, link.lens(entity), link.blackboard))
  }

  def apply[T](data: BlackboardToJsonData[T, Issue]) = {
    import data._
    val rawObject = JsonObject(
      "id" -> data.strings.mkString("."),
      "name" -> messageGetter(("title" :: data.strings).mkString(".")),
      "children" -> JsonList(blackboard.children.map { case (name, link) => linkJson(data, name, link) }.toList))
    blackboard.children.size match {
      case 0 => rawObject
      case _ => rawObject.|+|("leaf" -> JsonObject("value" -> blackboard.stringLens(data.strings).get(entity)))
      //        "validation" -> JsonList(validateFn(name :: prefix)(t).map(v => JsonString(v.s)))))
    }
  }
}

case class Blackboard[T, Issue](children: Map[String, BlackboardLink[T, _, Issue]], validate: Validator[T, Issue])() {
  def findLens[A](s: List[String]): Lens[T, A] = s match {
    case Nil => Lens.cast[T, A]
    case head :: tail => children.get(head) match {
      case Some(child) => child.findLens[A](tail)
      case None => throw new RuntimeException(s"Cannot find name '$head' legal values are ${children.keySet.toList.sorted}")
    }
  }
  def stringLens[A](s: List[String])(implicit sLens: Lens[A, String]): Lens[T, String] = findLens[A](s) andThen sLens
}

case class BlackboardLink[Main, Child, Issue](name: String, lens: Lens[Main, Child], blackboard: Blackboard[Child, Issue]) {
  def findLens[A](s: List[String]) = lens andThen blackboard.findLens[A](s)
}

trait BlackboardItem[Main, Child, Issue] {
  def lens: Lens[Main, Child]
  def validator: Validator[Child, Issue]
}


object BlackBoardBuilderBase {
  implicit def hasIdForLeafBuilder[M, C]: HasId[BlackBoardBuilderBase[M, C], Int] = lb => lb.id
}
trait BlackBoardBuilderBase[Main, Issue] {
  def id: Int
}
trait BlackBoardBuilderBaseWithChild[Main, Child, Issue] extends BlackBoardBuilderBase[Main, Issue] {
  def build: BlackboardLink[Main, Child, Issue]
}
case class LeafBuilder[Main, Leaf, Issue, HasSetter, HasGetter, HasValidator](id: Int, name: String, getter: Option[Main => Leaf], setter: Option[(Main, Leaf) => Main], validator: Option[Validator[Leaf, Issue]]) extends BlackBoardBuilderBaseWithChild[Main, Leaf, Issue] {
  def build: BlackboardLink[Main, Leaf, Issue] = (getter, setter, validator) match {
    case (Some(getter), Some(setter), Some(validator)) => BlackboardLink[Main, Leaf, Issue](name, Lens(getter, setter), Blackboard(Map(), validator))
    case x => throw new RuntimeException(s"Not set up properly: $x")
  }
}

case class ChildBuilder[Main, Child, Issue](id: Int, name: String, lens: Lens[Main, Child], child: Blackboard[Child, Issue]) extends BlackBoardBuilderBaseWithChild[Main, Child, Issue] {
  def build = BlackboardLink(name, lens, child)
}

class BlackboardBuilder[M] extends BlackboardBuilderAnyIssue[M, ValidateProblem]
trait BlackboardBuilderAnyIssue[M, Issue] extends IdMaker {

  implicit protected val agg: RememberingAggregator2[BlackBoardBuilderBase[M, Issue],Int] = new RememberingAggregator2()

  protected def leaf[C](name: String) = LeafBuilder[M, C, Issue, No, No, No](getNextId, name, None, None, None).sideeffect(agg.apply)

  implicit protected class LeafBuilderSetterOps[C, HasGetter, HasValidator](builder: LeafBuilder[M, C, Issue, No, HasGetter, HasValidator]) {
    import builder._
    def set(fn: (M, C) => M) = new LeafBuilder[M, C, Issue, Yes, HasGetter, HasValidator](id, name, getter, Some(fn), validator).sideeffect(agg.apply)
  }
  implicit protected class LeafBuilderGetterOps[C, HasSetter, HasValidator](builder: LeafBuilder[M, C, Issue, HasSetter, No, HasValidator]) {
    import builder._
    def get(fn: M => C) = new LeafBuilder[M, C, Issue, HasSetter, Yes, HasValidator](id, name, Some(fn), setter, validator).sideeffect(agg.apply)

  }
  implicit protected class LeafBuilderValidatorOps[C, HasSetter, HasGetter](builder: LeafBuilder[M, C, Issue, HasSetter, HasGetter, No]) {
    import builder._
    def validate(validator: Validator[C, Issue]) = new LeafBuilder[M, C, Issue, HasSetter, HasGetter, Yes](id, name, getter, setter, Some(validator)).sideeffect(agg.apply)
    def validateAs[T](validator: Validator[T, Issue])(implicit lens: Lens[C, T]) = validate { list: List[String] => c: C => validator(list)(lens(c)) }

  }

  protected def child[C](name: String, child: Blackboard[C, Issue], get: M => C, set: (M, C) => M) = ChildBuilder[M, C, Issue](getNextId, name, Lens(get, set), child) sideeffect agg.apply

  def build = Blackboard(agg.items.collect { case l: BlackBoardBuilderBaseWithChild[M, _, Issue] => l.build }.toMapFrom(_.name), Validators.noValidator) sideeffect (_ => agg.clear)

}


//class `BlackboardBuilder[Issue, Main, Child](name: String) extends IdMaker {
//  type BI[M, C] = BlackboardItem[M, C, Issue]
//  implicit val aggregator = new RememberingAggregator2[BI, Main, Child, Int]()
//  case class item(name: String) {
//    case class get[Leaf](getter: Child => Leaf) {
//      def set(setter: (Child, Leaf) => Leaf) = BlackboardLeaf()
//    }
//  }
//}

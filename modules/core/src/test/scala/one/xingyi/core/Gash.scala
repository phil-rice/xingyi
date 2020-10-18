//package one.xingyi.core
//
//import scala.language.higherKinds
//
//trait Json
//
//trait PrettyPrint[T] extends (T => String)
//trait HasPrettyPrint[T] {def prettyPrint: PrettyPrint[T]}
//
//object PrettyPrint {
//  implicit def prString: PrettyPrint[String] = s => s
//  implicit def prInt: PrettyPrint[Int] = _.toString
//  implicit class PrettyPrintOps[T](t: T)(implicit pp: PrettyPrint[T]) {def prettyPrint: String = pp(t)}
//}
//
//trait Formatters[T] extends (T => String)
//trait HasFormatters[T] {def formatters: Formatters[T]}
//
//trait ToJson[T] extends (T => Json)
//trait HasToJson[T] {def toJson: ToJson[T]}
//
//trait FromJson[T] extends (Json => T)
//trait HasFromJson[T] {def fromJson: FromJson[T]}
//
//trait GetT[Main, T] extends (Main => T)
//
//class H2[M[_], T](val held: M[T])
//
//object H2 {
//}
//class Holder[M[_], T](val held: M[T])(implicit val prettyPrint: PrettyPrint[T], val formatters: Formatters[T], val toJson: ToJson[T], val fromJson: FromJson[T])
//  extends HasPrettyPrint[T] with HasFormatters[T] with HasToJson[T] with HasFromJson[T]
//
//object Holder {
//  implicit def toHolder[M[_], T: PrettyPrint : Formatters : ToJson : FromJson](m: M[T]): Holder[M, T] = new Holder[M, T](m)
//}
//
//class HolderWithT[M[_], T: PrettyPrint : Formatters : ToJson : FromJson](val m: M[T])(implicit getT: GetT[M[T], T]) extends Holder[M, T](m) {
//  def getT: T = getT(m)
//}
//object HolderWithT {
//  implicit def toHolderWithT[M[_], T: PrettyPrint : Formatters : ToJson : FromJson](m: M[T])(implicit getT: GetT[M[T], T]): HolderWithT[M, T] = new HolderWithT[M, T](m)
//  implicit def getTFromHolder[M[_], T]: GetT[HolderWithT[M, T], T] = _.getT
//}
//
//trait Get[Main, Child] {def get(main: Main): Child}
//trait HasGet[Main, Child] {def get: Get[Main, Child]}
//
//case class C[T](t: T)
//object C {
//  implicit def holds[T]: Get[C[T], T] = new Get[C[T], T] {override def get(main: C[T]): T = main.t}
//}
//
//case class CBad[T](t: T)(implicit val prettyPrint: PrettyPrint[T]) extends HasPrettyPrint[T]
//
//object Main extends App {
//  class Holder[M[_], T](val m: M[T])(implicit val prettyPrint: PrettyPrint[T], originalGet: Get[M[T], T], val fromJson: FromJson[T], val toJson: ToJson[T])
//    extends HasPrettyPrint[T] with Get[Holder[M, T], T] with HasToJson[T] with HasFromJson[T] {
//    override def get(main: Holder[M, T]): T = originalGet.get(main.m)
//  }
//  type HolderC[T] = Holder[C, T]
//  object Holder {
//    implicit def toHolder[M[_], T: PrettyPrint](m: M[T])(implicit get: Get[M[T], T]) = new Holder(m)
//  }
//  def printMe[M[T] <: HasPrettyPrint[T] with Get[M[T], T], T](m: M[T]) = println(m.prettyPrint(m.get(m)))
//  List[HolderC[_]]((C("one")), (C(2))).map(h => printMe(h))
//
//  //  object Holder {
//  //    def apply[M[_], T: PrettyPrint](m: M[T])(implicit get: Get[M[T], T]): Holder[T] = new HolderImpl[M, T](m)
//  //  }
//  //  class HolderImpl[M[_], T](val m: M[T])(implicit val prettyPrint: PrettyPrint[T], originalGet: Get[M[T], T]) extends Holder[T] {
//  //
//  //    override def get(main: Holder[T]): T = main match {case h: HolderImpl[M, T] => originalGet.get(h.m)}
//  //  }
//
//
//  //  implicit def f[T]: Formatters[T] = _ => ???
//  //  implicit def toJ[T]: ToJson[T] = _ => ???
//  //  implicit def fr[T]: FromJson[T] = _ => ???
//  //
//  //  def printMe[M[C[_], T] <: HasPrettyPrint[T], C[_], T](m: M[C, T])(implicit get: GetT[M[C, T], T]) = println(m.prettyPrint(get(m)))
//  //  def printMe[M[T] <: HasPrettyPrint[T], T](m: M[T])(implicit get: GetT[M[T], T]) = println(m.prettyPrint(get(m)))
//  //  def printMe[C[_], T](m: HolderWithT[C, T]) = println(m.prettyPrint(m.getT))
//  //  def printMe[C[_], T](m: HolderWithT[C, T]) = println(m.prettyPrint(m.getT))
//  //  def printMe[T](m: Holder[C, T]) = println(m.prettyPrint(m.held.t))
//  //
//  //  def printMe[T](m: C[T])(implicit prettyPrint: PrettyPrint[T]) = println(prettyPrint(m.t))
//  //  def printMe[T](m: CBad[T]) = println(m.prettyPrint(m.t))
//  //
//  //
//  //  type HolderC[T] = HolderWithT[C, T]
//  //  List[HolderC[_]](C("one"), C(2)).map(printMe(_))
//  //
//  //  List(C("one"), C(2)).map(h => printMe(h))
//  //  List(C("one"), C(2)).map(h => printMe(h))
//
//}
//

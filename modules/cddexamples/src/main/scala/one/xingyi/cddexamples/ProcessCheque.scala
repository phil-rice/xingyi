package one.xingyi.cddexamples

import java.time.LocalDateTime

import one.xingyi.cddengine.{Engine, UseCase, UseCase2}

import scala.language.implicitConversions

case class World(date: LocalDateTime, thisBank: BankId, customerIdToCustomer: (CustomerId) => Customer, acceptedBanks: List[BankId] = List(BankId.hsbc, BankId.rbs, BankId.thisBank));

case class BankId(id: String)
object BankId {
  def thisBank = BankId("this")
  def hsbc = BankId("HSBC")
  def rbs = BankId("RBS")
  def dodgyBank = BankId("DodgyBank")
}

object GBP {
  implicit def intToGBP(i: Int) = GBP(i, 0)
  implicit def GBPToDouble(g: GBP) = g.pounds + g.pence / 100.0
}
case class GBP(pounds: Integer, pence: Integer)

case class CustomerId(id: String, bank: BankId)
case class Customer(id: CustomerId, balance: GBP, overdraftLimit: GBP, premiumCustomer: Boolean)
case class Cheque(refNo: String, from: CustomerId, to: CustomerId, date: LocalDateTime, amount: GBP)

object Message {
  implicit def stringToMessage(s: String) = Message(s)
  implicit def tupleToMessage(t: Tuple2[String, _]) = Message(t._1, t._2)
}
case class Message(pattern: String, keys: Any*)
case class ProcessChequeResult(pay: Boolean, message: Message)

object ProcessChequeTestMother {
  import GBP._

  val dodgyDaveId = CustomerId("12", BankId.thisBank)
  val dodgyDaveAtDodgyBankId = CustomerId("12", BankId.dodgyBank)
  val dodgyDave = Customer(dodgyDaveId, 100, 0, false)

  val richRogerId = CustomerId("34", BankId.thisBank)
  val richRoger = Customer(richRogerId, 10000, 4000, false)
  val richRogerAtHsbcId = CustomerId("34", BankId.hsbc)

  val today = LocalDateTime.now()
  val world = World(today, BankId.thisBank, Map(dodgyDaveId -> dodgyDave, richRogerId -> richRoger))

}

object ProcessCheque {
  import GBP._
  import ProcessChequeTestMother._
  //                  code((w: World, c: Cheque) => ProcessChequeResult(false, "processCheque.defaultResult.shouldntHappen")).

  type UCCheque = UseCase2[World, Cheque, ProcessChequeResult]
  val ucDifferentBank = new UCCheque("Checks from a different bank should be rejected") {
    scenario(world, Cheque("1", richRogerAtHsbcId, richRogerId, today, 1000)) when ((w, c) => c.from.bank != w.thisBank) produces ProcessChequeResult(false, "processCheque.reject.fromBankNotThisBank") comment "One thousand pounds from rich roger at HSBC to rich roger at this bank. But the 'FromBank' isn't this bank"
  }
  val ucNotOnWhiteList = new UCCheque("Cheques that are to a bank not on a white list are rejected") {
    scenario(world, Cheque("1", dodgyDaveId, dodgyDaveAtDodgyBankId, today, 50)) when ((w, c) => !w.acceptedBanks.contains(c.to.bank)) produces ProcessChequeResult(false, ("processCheque.reject.toBank.notInWhiteList", BankId.dodgyBank)) comment "Dodgy Dave is moving half his funds to a bank that isn't on the accepted list"
  }
  val ucOverdraftLimit = new UCCheque("Cheques that will take the customer over the overdraft limit will should be rejected") {
    scenario(world, Cheque("1", dodgyDaveId, richRogerId, today, 110)) when { (w, c) =>
      val customer = w.customerIdToCustomer(c.from)
      c.amount >= customer.balance && customer.overdraftLimit == GBP(0, 0)
    } produces ProcessChequeResult(false, "processCheque.reject.noOverdraft") comment "Dodgy Dave sending more money than he has"
  }
  val ucNormalAllow = new UCCheque("Cheques that are to to customers in an accepted bank, when the cheque writer has sufficient funds, should be allowed") {
    scenario(world, Cheque("1", dodgyDaveId, richRogerId, today, 50)) when ((w, c) => w.acceptedBanks.contains(c.to.bank)) produces ProcessChequeResult(true, "processCheque.accept") comment "Dodgy Dave sending an OK cheque to someone in this bank"
    scenario(world, Cheque("1", dodgyDaveId, richRogerAtHsbcId, today, 50)) produces ProcessChequeResult(true, "processCheque.accept") comment "Dodgy Dave sending an OK cheque to someone in an accepted bank"
  }

  val processCheque = Engine(ucDifferentBank or ucNotOnWhiteList or ucNormalAllow or ucOverdraftLimit)

  def main(args: Array[String]) {
    println(processCheque(world, Cheque("1", dodgyDaveId, richRogerAtHsbcId, today, 50)))
  }

}

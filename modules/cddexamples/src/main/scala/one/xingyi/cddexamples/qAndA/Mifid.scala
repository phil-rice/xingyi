/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddexamples.qAndA

import one.xingyi.cddengine.{Engine, SimpleValidation, UseCase1}
import one.xingyi.cddmustache.{RawMustache, Mustache}
import one.xingyi.core.strings.ShortPrint
import one.xingyi.json4s.Json4sWriter._
import org.json4s.JValue

import scala.language.{implicitConversions, postfixOps}

object BusinessType {
  val creditInstitute = BusinessType("credit institute")
  val investmentFirm = BusinessType("investmentFirm")
  val financialInstitution = BusinessType("financialInstitution")
  val insuranceCompany = BusinessType("insuranceCompany")
  val collectiveInvestmentScheme = BusinessType("collectiveInvestmentScheme")
  val pensionFund = BusinessType("pensionFund")
  val comodityDealer = BusinessType("comodityDealer")
  val locals = BusinessType("locals")
  val someInstitutionalDealer = BusinessType("someInstitutionalDealer")
  val authorisedForFinancialMarkets = List(creditInstitute, investmentFirm, financialInstitution, collectiveInvestmentScheme, pensionFund, comodityDealer, someInstitutionalDealer)
  //
  val nationalGovernment = BusinessType("national government")
  val regionalGovernment = BusinessType("regional government")
  val publicBodiesThatManagePublicDebt = BusinessType("publicBodiesThatManagePublicDebt")
  val someInstitutionalDealerInvestingInFinancialInstrument = BusinessType("someInstitutionalDealerInvestingInFinancialInstrument")
  val nationalRegionalBodiesOrSimilar = List(nationalGovernment, regionalGovernment, publicBodiesThatManagePublicDebt)

  val other = BusinessType("other")
  val invalid = BusinessType("")
}
case class BusinessType(s: String) extends AnyVal


sealed trait MifidConclusion
case object UnknownClient extends MifidConclusion
case object ProfessionalClient extends MifidConclusion
case object RetailClient extends MifidConclusion


case class Config(balanceSheetThreshold: GBP, netTurnoverThreshold: GBP, ownFundsThreshold: GBP)


object EntityDetails {
  implicit def shortPrintForED: ShortPrint[EntityDetails] = ed => s"Entity(${ed.entity.identity.name.toString})"
}
case class EntityDetails(entity: Entity)(implicit config: Config, blackboard: Blackboard[Entity]) {
  def bigBalanceSheet = entity.financialData.balanceSheet.total >= config.balanceSheetThreshold
  def highTurnoverSheet = entity.financialData.profitAndLoss.turnover >= config.netTurnoverThreshold
  def highOwnFunds = entity.financialData.balanceSheet.shareHoldersInterest >= config.ownFundsThreshold
  val countOfHighMoneyMakers = List(bigBalanceSheet, highTurnoverSheet, highOwnFunds).count(_ == true)
  def hasValidationIssues = {blackboard.children.flatMap(_.validate(List(), entity)).nonEmpty}
  override def toString: String = s"EntityDetails($entity, ($bigBalanceSheet, $highTurnoverSheet, $highOwnFunds), count $countOfHighMoneyMakers, validation: ${blackboard.children.flatMap(_.validate(List(), entity))})"
}


class MifidDecisionMaker extends Question {
  type MifidUC = UseCase1[EntityDetails, MifidConclusion]


  private val prototypeFinancialData = FinancialData(BalanceSheet(totalNetAssets = GBP(123), totalLiabilities = GBP(234), shareHoldersInterest = GBP(0)), ProfitAndLoss(nettIncome = GBP(12323), nettExpenditure = GBP(234)))
  private val prototypeActivities = Activities("someMainActivies", false, "someProductAndServices", "someBuisnessLine", Naics("someNaicsCode"), Nace(144))
  val prototypeEntity = Entity(
    Identity("", BusinessType(""), IndustrySector("something"), Address("someRegAdd"), Address("someOpAdd"), TeleNo("phoneNo"), TeleNo("fax"), Website(""), BIC("someBic"), Chaps("someChaps"), ClearingCode("someClearingCode")),
    prototypeFinancialData,
    prototypeActivities,
    GateKeeperThings(false)
  )
  def edWith(name: String, businessType: BusinessType, balanceSheetTotal: Long, netTurnover: Long, ownFunds: Long, mainBusinessIsFinancialTransactions: Boolean = false)(implicit config: Config, blackboard: Blackboard[Entity]) = {
    val fd = prototypeFinancialData.
             copy(balanceSheet = prototypeFinancialData.balanceSheet.copy(totalNetAssets = GBP(balanceSheetTotal), shareHoldersInterest = GBP(ownFunds))).
             copy(profitAndLoss = prototypeFinancialData.profitAndLoss.copy(nettIncome = GBP(netTurnover)))

    val identity = prototypeEntity.identity.copy(name = name, businessType = businessType)
    val activities = prototypeActivities.copy(mainBusinessIsFinancialTransactions = mainBusinessIsFinancialTransactions)
    EntityDetails(prototypeEntity.copy(identity = identity, financialData = fd, activities = activities))
  }
  implicit class BusinessTypeOps(b: BusinessType)(implicit config: Config, blackboard: Blackboard[Entity]) {
    case class name(name: String) {
      def lotsOfMoney = edWith(name, b, balanceSheetTotal = 1000000000, netTurnover = 1000000000, ownFunds = 1000000000)
      def highBalanceSheet = edWith(name, b, balanceSheetTotal = 1000000000, netTurnover = 10, ownFunds = 10)
      def highNetTurnover = edWith(name, b, balanceSheetTotal = 10, netTurnover = 1000000000, ownFunds = 10)
      def highOwnFunds = edWith(name, b, balanceSheetTotal = 10, netTurnover = 10, ownFunds = 1000000000)
      def highBSAndOwnFunds = edWith(name, b, balanceSheetTotal = 1000000000, netTurnover = 10, ownFunds = 1000000000)
      def highTurnoverAndBS = edWith(name, b, balanceSheetTotal = 10, netTurnover = 1000000000, ownFunds = 1000000000)
      def totallyBrokeAndInviolationofGAP7fold = edWith(name, b, balanceSheetTotal = 10, netTurnover = 10, ownFunds = 10)
      def mainBuisnessIsInvestment = edWith(name, b, 10, 10, 10, mainBusinessIsFinancialTransactions = true)
    }
  }
  import BusinessType._

  implicit val config = Config(balanceSheetThreshold = GBP(20000000), netTurnoverThreshold = GBP(400000000), ownFundsThreshold = GBP(2000000))

  val ucMustBeValidated = new MifidUC("No validation issues") {
    scenario(invalid name "" lotsOfMoney) produces UnknownClient when { ed =>  ed.hasValidationIssues }

  }
  val ucAuthorisedOrRegulatedEntites = new MifidUC("Authorised or Regulated entities") {
    scenario(creditInstitute name "Credit Suisse" lotsOfMoney) produces ProfessionalClient when (ed => authorisedForFinancialMarkets.contains(ed.entity.identity.businessType))
    scenario(investmentFirm name "UBS" lotsOfMoney) produces ProfessionalClient
    scenario(financialInstitution name "HSBC" lotsOfMoney) produces ProfessionalClient
    scenario(insuranceCompany name "Scottish Widow" lotsOfMoney) produces ProfessionalClient
    scenario(collectiveInvestmentScheme name "Hedgehog" lotsOfMoney) produces ProfessionalClient
    scenario(pensionFund name "Lacuna Inc" lotsOfMoney) produces ProfessionalClient
    scenario(comodityDealer name "gamblers R Us" lotsOfMoney) produces ProfessionalClient
    scenario(locals name "Pete's Sweeps" lotsOfMoney) produces ProfessionalClient
    scenario(someInstitutionalDealer name "vegas inc" lotsOfMoney) produces ProfessionalClient
  }

  val ucLargeUndertaking = new MifidUC("Has two of ' high balance sheet, hit turnover, high own funds' ") {
    scenario(other name "really rich guy inc" lotsOfMoney) produces ProfessionalClient when (ed => ed.countOfHighMoneyMakers >= 2)
    scenario(other name "HighBs High own funds" highBSAndOwnFunds) produces ProfessionalClient

    scenario(other name "High BS, nothing else" highBalanceSheet) produces RetailClient when (ed => ed.countOfHighMoneyMakers < 2)
    scenario(other name "High turnover, nothing else" highNetTurnover) produces RetailClient
    scenario(other name "High ownfunds, nothing else" highOwnFunds) produces RetailClient
  }

  val ucNationalAndRegionalBodies = new MifidUC("National and regional body") {
    scenario(nationalGovernment name "UK PLC" totallyBrokeAndInviolationofGAP7fold) produces ProfessionalClient  when (ed => nationalRegionalBodiesOrSimilar.contains(ed.entity.identity.businessType))
    scenario(regionalGovernment name "UK PLC" totallyBrokeAndInviolationofGAP7fold) produces ProfessionalClient
    scenario(publicBodiesThatManagePublicDebt name "UK PLC" totallyBrokeAndInviolationofGAP7fold) produces ProfessionalClient
  }

  val ucInstituionalInvestorsWhoseMainActivityIsToInves = new MifidUC("we invest a lot") {
    scenario(other name "invest a lot" mainBuisnessIsInvestment) produces ProfessionalClient when (ed => ed.entity.activities.mainBusinessIsFinancialTransactions)
  }
  val someOtherComplicatedThing = new MifidUC("some other complicated rules") {
  }

  val categoriser = Engine(ucMustBeValidated or ucLargeUndertaking or ucAuthorisedOrRegulatedEntites or ucNationalAndRegionalBodies or ucInstituionalInvestorsWhoseMainActivityIsToInves)
  //  println(ucMustBeValidated.allScenarios)
  //  val categoriser2 = Engine(ucAuthorisedOrRegulatedEntites or ucLargeUndertaking or ucNationalAndRegionalBodies)
  //  val categoriser3 = Engine(ucAuthorisedOrRegulatedEntites or ucLargeUndertaking or ucNationalAndRegionalBodies or someOtherComplicatedThing)


}


object Trace extends App {

  implicit def v[P, R] = new SimpleValidation[P, R]
  implicit val template = Mustache("Coolness", "question.mustache")

  private val categoriser: Engine[EntityDetails, MifidConclusion] = new MifidDecisionMaker().categoriser
  categoriser.tools.printTraceAboutAdding("mifid")
  println("Issues")
  categoriser.tools.issues.foreach(println)
}


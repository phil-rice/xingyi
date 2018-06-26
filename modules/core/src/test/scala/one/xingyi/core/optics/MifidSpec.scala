package one.xingyi.core.optics

import java.util.ResourceBundle

import one.xingyi.core.UtilsSpec
import one.xingyi.core.json.JsonWriter

import scala.language.postfixOps

trait MifidSpec {
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
  case class BusinessType(s: String)
  case class IndustrySector(s: String)
  case class Address(s: String)

  case class TeleNo(s: String)
  case class BIC(s: String)
  case class ClearingCode(s: String)
  case class Chaps(s: String)
  case class Website(s: String)
  case class Naics(code: String)
  case class Nace(code: Int)
  case class Identity(name: String, businessType: BusinessType,
                      industrySectory: IndustrySector,
                      registeredAddress: Address,
                      operationAddress: Address,
                      teleNo: TeleNo, fax: TeleNo,
                      website: Website,
                      bIC: BIC,
                      chaps: Chaps,
                      clearingCode: ClearingCode)

  case class GBP(amnt: Long) {
    def >=(other: GBP) = amnt >= other.amnt
    def -(other: GBP) = GBP(amnt - other.amnt)
  }
  case class BalanceSheet(totalNetAssets: GBP, totalLiabilities: GBP, shareHoldersInterest: GBP) {
    def total = totalNetAssets - totalLiabilities
  }
  case class ProfitAndLoss(nettIncome: GBP, nettExpenditure: GBP) {
    def turnover: GBP = nettIncome - nettExpenditure
  }
  case class FinancialData(balanceSheet: BalanceSheet, profitAndLoss: ProfitAndLoss) {}
  case class Activities(mainActivities: String, mainBusinessIsFinancialTransactions: Boolean, productAndServices: String, businessLine: String, naics: Naics, nace: Nace)
  case class GateKeeperThings(bearerShares: Boolean)
  case class Entity(identity: Identity, financialData: FinancialData, activities: Activities, gateKeeperThings: GateKeeperThings)

}


trait MifidBuilder extends MifidSpec {
  private val prototypeFinancialData = FinancialData(BalanceSheet(totalNetAssets = GBP(123), totalLiabilities = GBP(234), shareHoldersInterest = GBP(0)), ProfitAndLoss(nettIncome = GBP(12323), nettExpenditure = GBP(234)))
  private val prototypeActivities = Activities("someMainActivies", false, "someProductAndServices", "someBuisnessLine", Naics("someNaicsCode"), Nace(144))

  val prototypeEntity = Entity(
    Identity("", BusinessType(""), IndustrySector("something"), Address("someRegAdd"), Address("someOpAdd"), TeleNo("phoneNo"), TeleNo("fax"), Website(""), BIC("someBic"), Chaps("someChaps"), ClearingCode("someClearingCode")),
    prototypeFinancialData,
    prototypeActivities,
    GateKeeperThings(false)
  )
  def entityWith(name: String, businessType: BusinessType, balanceSheetTotal: Long, netTurnover: Long, ownFunds: Long, mainBusinessIsFinancialTransactions: Boolean = false) = {
    val fd = prototypeFinancialData.
      copy(balanceSheet = prototypeFinancialData.balanceSheet.copy(totalNetAssets = GBP(balanceSheetTotal), shareHoldersInterest = GBP(ownFunds))).
      copy(profitAndLoss = prototypeFinancialData.profitAndLoss.copy(nettIncome = GBP(netTurnover)))

    val identity = prototypeEntity.identity.copy(name = name, businessType = businessType)
    val activities = prototypeActivities.copy(mainBusinessIsFinancialTransactions = mainBusinessIsFinancialTransactions)
    prototypeEntity.copy(identity = identity, financialData = fd, activities = activities)
  }
  implicit class BusinessTypeOps(b: BusinessType)(implicit blackboard: Blackboard[Entity, ValidateProblem]) {
    case class name(name: String) {
      def lotsOfMoney = entityWith(name, b, balanceSheetTotal = 1000000000, netTurnover = 1000000000, ownFunds = 1000000000)
      def highBalanceSheet = entityWith(name, b, balanceSheetTotal = 1000000000, netTurnover = 10, ownFunds = 10)
      def highNetTurnover = entityWith(name, b, balanceSheetTotal = 10, netTurnover = 1000000000, ownFunds = 10)
      def highOwnFunds = entityWith(name, b, balanceSheetTotal = 10, netTurnover = 10, ownFunds = 1000000000)
      def highBSAndOwnFunds = entityWith(name, b, balanceSheetTotal = 1000000000, netTurnover = 10, ownFunds = 1000000000)
      def highTurnoverAndBS = entityWith(name, b, balanceSheetTotal = 10, netTurnover = 1000000000, ownFunds = 1000000000)
      def totallyBrokeAndInviolationofGAP7fold = entityWith(name, b, balanceSheetTotal = 10, netTurnover = 10, ownFunds = 10)
      def mainBuisnessIsInvestment = entityWith(name, b, 10, 10, 10, mainBusinessIsFinancialTransactions = true)
    }
  }

}

trait Question extends MifidSpec {
  def isDefined(list: List[String])(s: String) = if (s.isEmpty) List(ValidateProblem(s"$list Required")) else List()
  def isNonZero(list: List[String])(g: GBP) = if (g.amnt == 0) List(ValidateProblem(s"$list Required")) else List()
  def allgood[S](list: List[String])(s: S) = List()

  implicit val addressStringL = Lens[Address, String](_.s, (a, s) => a.copy(s = s))
  implicit val gbpToStringL = Lens[GBP, String](_.amnt.toString, (a, s) => a.copy(amnt = s.toInt))
  implicit val businessTypeToStringL = Lens[BusinessType, String](_.s.toString, (a, s) => a.copy(s = s))

  val identityB = new BlackboardBuilder[Identity] {
    leaf[String]("name") get (_.name) set ((m, c) => m.copy(name = c)) validateAs[String] isDefined
    leaf[BusinessType]("businessType") get (_.businessType) set ((m, c) => m.copy(businessType = c)) validateAs[String] isDefined
    leaf[Address]("operationAddress") get (_.operationAddress) set ((m, c) => m.copy(operationAddress = c)) validateAs[String] isDefined
    leaf[Address]("registeredAddress") get (_.registeredAddress) set ((m, c) => m.copy(registeredAddress = c)) validate allgood
  }.build


  val balanceSheetB = new BlackboardBuilder[BalanceSheet] {
    leaf[GBP]("totalNetAssets") get (_.totalNetAssets) set ((i, n) => i.copy(totalNetAssets = n)) validate isNonZero
    leaf[GBP]("totalLiabilities") get (_.totalLiabilities) set ((i, n) => i.copy(totalLiabilities = n)) validate isNonZero
    leaf[GBP]("shareHoldersInterest") get (_.shareHoldersInterest) set ((i, n) => i.copy(shareHoldersInterest = n)) validate isNonZero
  }.build


  val profitAndLossB = new BlackboardBuilder[ProfitAndLoss] {
    leaf[GBP]("nettExpenditure") get (_.nettExpenditure) set ((i, n) => i.copy(nettExpenditure = n)) validate isNonZero
    leaf[GBP]("nettIncome") get (_.nettIncome) set ((i, n) => i.copy(nettIncome = n)) validate isNonZero
  }.build

  val financialDataB = new BlackboardBuilder[FinancialData] {
    child[BalanceSheet]("balanceSheet", balanceSheetB, _.balanceSheet, (m, c) => m.copy(balanceSheet = c))
    child[ProfitAndLoss]("profitAndLoss", profitAndLossB, _.profitAndLoss, (m, c) => m.copy(profitAndLoss = c))
  }.build

  implicit val entityB = new BlackboardBuilder[Entity] {
    child[Identity]("identity", identityB, _.identity, (m, c) => m.copy(identity = c))
    child[FinancialData]("financialdata", financialDataB, _.financialData, (m, c) => m.copy(financialData = c))
  }.build

}

abstract class AbstractMifidBlackboardSpec[J: JsonWriter] extends UtilsSpec with MifidBuilder with Question {

  behavior of "entityB"

  it should "get things" in {
    val e = BusinessType.comodityDealer name "somename" lotsOfMoney;
    entityB.findLens[String](List("identity", "name"))(e) shouldBe "somename"
    entityB.findLens[String](List("identity", "name"))(e) shouldBe "somename"
    entityB.stringLens[String](List("identity", "name")) get e shouldBe "somename"
    entityB.findLens[Address](List("identity", "registeredAddress"))(e) shouldBe Address("someRegAdd")
  }

  it should "make json " in {
    implicit  val messageGetter = MessageGetter.fnGetter(x => s"[$x]")
    val e = BusinessType.comodityDealer name "somename" lotsOfMoney;
    new BlackboardToJson() apply new BlackboardToJsonData(List(), e, entityB) shouldBe ""

  }
}


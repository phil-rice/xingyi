package one.xingyi.cddexamples


object BusinessType {
  val creditInstitute = BusinessType("credit institute")
  val investmentFirm = BusinessType("investmentFirm")
  val financialInstitution = BusinessType("financialInstitution")
  val collectiveInvestmentScheme = BusinessType("collectiveInvestmentScheme")
  val pensionFund = BusinessType("pensionFund")
  val comodityDealer = BusinessType("comodityDealer")
  val someInstitutionalDealer = BusinessType("someInstitutionalDealer")
  val authorisedForFinancialMarkets = List(creditInstitute, investmentFirm, financialInstitution, collectiveInvestmentScheme, pensionFund, comodityDealer, someInstitutionalDealer)

  val nationalGovernment = BusinessType("national government")
  val regionalGovernment = BusinessType("regional government")

  val other = BusinessType("other")
}
case class BusinessType(s: String) extends AnyVal
case class Money(i: Int) extends AnyVal {
  def >=(other: Money): Boolean = i >= other.i
}

sealed trait MifidConclusion
case object ProfessionalClient extends MifidConclusion
case object IsItAProfessionalElective extends MifidConclusion


case class Config(balanceSheetThreshold: Money, netTurnoverThreshold: Money, ownFundsThreshold: Money)

case class Entity(businessType: BusinessType, authorised: Boolean, regulated: Boolean, balanceSheet: Money, netTurnover: Money, ownFunds: Money)

case class EntityDetails(entity: Entity)(implicit config: Config) {
  def bigBalanceSheet = entity.balanceSheet >= config.balanceSheetThreshold
  def highTurnoverSheet = entity.netTurnover >= config.netTurnoverThreshold
  def highOwnFunds = entity.ownFunds >= config.ownFundsThreshold
  val countOfHighMoneyMakers = List(bigBalanceSheet, highTurnoverSheet, highOwnFunds).count(_ == true)
}

/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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

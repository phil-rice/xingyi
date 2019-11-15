package one.xingyi.cddexamples

import one.xingyi.cddengine.{UseCase1, UseCase2}

case class ComplianceData(

                         )

class Compliance {
  type UC = UseCase1[Int, String]

  val serviceNowUC=new UC("Service Now tickets much be approved"){
    scenario(4) title "everything OK" produces "left won" when { l => l >= 2 && l >= 4 }

  }

}

object Compliance extends Compliance with App{

}
package org.validoc.domain


case class ProductionId(id: String) extends AnyVal

case class Production(id: ProductionId, info: String)

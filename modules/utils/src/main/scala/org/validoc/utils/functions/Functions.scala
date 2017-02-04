package org.validoc.utils.functions

object Functions {
  def pipelineFn[X](sideEffect: X => Unit)(x: X): X = {
    sideEffect(x)
    x
  }
}

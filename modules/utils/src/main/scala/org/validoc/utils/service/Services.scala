package org.validoc.utils.service

import org.validoc.utils.Service


trait ServiceWithChildren[M[_]] {
  def children: Seq[Service[M, _, _]]
}


package com.edofic.scrat

import com.edofic.scrat.Tokens._

/**
 * User: andraz
 * Date: 10/23/12
 * Time: 11:54 AM
 */
object Optimizer {
  def apply(exps: List[Expression]): List[Expression] = exps map apply
  def apply(exp: Expression): Expression = {
    //no optimizations available
    exp
  }
}

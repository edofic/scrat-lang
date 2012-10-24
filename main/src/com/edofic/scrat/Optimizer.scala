package com.edofic.scrat

import com.edofic.scrat.Tokens._
import TreeOps.Modification

/**
 * User: andraz
 * Date: 10/23/12
 * Time: 11:54 AM
 */
object Optimizer {
  val simplifyFunctionCalls: Modification = {
    case DotAccess(List(f: FunctionCall)) => f
  }

  val optimizations = simplifyFunctionCalls  :: Nil

  def apply(exps: List[Expression]): List[Expression] = exps map apply
  def apply(exp: Expression): Expression =
    optimizations.foldLeft(exp)((tree, func) => TreeOps.applyRecursiveModificaton(exp)(func))
}

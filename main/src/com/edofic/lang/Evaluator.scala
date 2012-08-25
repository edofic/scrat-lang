package com.edofic.lang

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:15 PM
 */
object Evaluator {

  import Tokens._

  def apply(e: Expression): Double = e match {
    case Number(n) => n
    case Add(l, r) => apply(l) + apply(r)
    case Subtract(l, r) => apply(l) - apply(r)
    case Multiply(l, r) => apply(l) * apply(r)
    case Divide(l, r) => apply(l) / apply(r)
  }
}

package com.edofic.scrat

import com.edofic.scrat.Util.Exceptions.{InvalidToken, SemanticError}



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
    case Exponent(l, r) => math.pow(apply(l), apply(r))
    case Identifier(name) => {
      //lookup standard library
      StdLib(name) match {
        case Some(v: Double) => v
        case _ => throw new SemanticError(name + " not found")
      }
    }
    case t => throw new InvalidToken(t + " not implemented in evaluator")
  }
}

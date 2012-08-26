package com.edofic.scrat

import com.edofic.scrat.Util.Exceptions._
import Tokens._
import ScratRuntime.FunctionVarArg

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:15 PM
 */
class Evaluator(runtime: ScratRuntime) {
  private def binary[L, R](l: Expression, r: Expression)(f: (L, R) => Any) = (apply(l), apply(r)) match {
    case (l: L, r: R) => f(l, r)
    case other => throw new ScratInvalidTypeError(String.format("in %s got %s", f.toString(), other.toString))
  }

  def apply(e: Expression): Any = e match {
    case Number(n) => n
    case SString(s) => s
    case Add(l, r) => binary(l, r)((_: Double) + (_: Double))
    case Subtract(l, r) => binary(l, r)((_: Double) - (_: Double))
    case Multiply(l, r) => binary(l, r)((_: Double) * (_: Double))
    case Divide(l, r) => binary(l, r)((_: Double) / (_: Double))
    case Exponent(l, r) => binary(l, r)(math.pow _)
    case Identifier(name) => {
      runtime.get(name) match {
        case Some(v) => v
        case _ => throw new ScratSemanticError(name + " not found")
      }
    }
    case ExpList(lst) => lst map apply
    case FunctionCall(name, args) => runtime.get(name.id) match {
      case Some(f: FunctionVarArg) => f.apply(apply(args))
      case None => throw new ScratSemanticError("function " + name + "not found")
    }
    case Assignment(name, exp) => {
      val e = apply(exp)
      runtime.put(name.id, e)
      e
    }
    case t => throw new ScratInvalidTokenError(t + " not implemented in evaluator")
  }
}

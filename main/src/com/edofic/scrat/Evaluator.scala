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
  private def binaryDouble(l: Expression, r: Expression)(f: (Double, Double) => Double)
                          (implicit scope: SScope): Double = (apply(l), apply(r)) match {
    case (a: Double, b: Double) => f(a, b)
    case other => throw new ScratInvalidTypeError("expected two doubles, got " + other)
  }

  def apply(e: List[Expression])(implicit scope: SScope): Any = {
    e map apply last
  }

  def apply(e: Expression)(implicit scope: SScope): Any = e match {
    case Number(n) => n
    case SString(s) => s
    case Add(l, r) => binaryDouble(l, r)(_ + _)
    case Subtract(l, r) => binaryDouble(l, r)(_ - _)
    case Multiply(l, r) => binaryDouble(l, r)(_ * _)
    case Divide(l, r) => binaryDouble(l, r)(_ / _)
    case Exponent(l, r) => binaryDouble(l, r)(math.pow)
    case Identifier(name) => {
      scope.get(name) match {
        case Some(v) => v
        case _ => throw new ScratSemanticError(name + " not found")
      }
    }
    case ExpList(lst) => lst map apply
    case FunctionCall(name, args) => scope.get(name.id) match {
      case Some(f: FunctionVarArg) => f.apply(apply(args))
      case None => throw new ScratSemanticError("function " + name + "not found")
    }
    case Assignment(name, exp) => {
      val e = apply(exp)
      scope.put(name.id, e)
      e
    }
    case IfThenElse(pred, then, els) => apply(pred) match {
      case d: Double => if (d != 0) apply(then) else apply(els)
      case other => throw new ScratInvalidTypeError("expected a number, got " + other)
    }
    case Equals(l, r) => (if (apply(l) == apply(r)) 1 else 0): Double
    case NotEquals(l, r) => (if (apply(l) != apply(r)) 1 else 0): Double
    case t => throw new ScratInvalidTokenError(t + " not implemented in evaluator")
  }
}

package com.edofic.scrat

import com.edofic.scrat.Util.Exceptions._
import Tokens._
import ScratRuntime._
import Util.Implicits._

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:15 PM
 */
class Evaluator {
  private def binaryDouble(l: Expression, r: Expression)(f: (Double, Double) => Double)
                          (implicit scope: SScope): Double = (apply(l), apply(r)) match {
    case (a: Double, b: Double) => f(a, b)
    case other => throw new ScratInvalidTypeError("expected two doubles, got " + other)
  }

  def apply(e: List[Expression])(implicit scope: SScope): Any = {
    (e map apply).lastOption match {
      case Some(a) => a
      case None => ()
    }
  }

  def createFunFromAst(arglist: List[Identifier], body: List[Expression], scope: SScope): FunctionVarArg =
    (args: Any) => args match {
      case lst: List[Any] => {
        if (lst.length != arglist.length) {
          throw new ScratInvalidTypeError("expected " + arglist.length + " arguments, but got " + lst.length)
        } else {
          val closure = new SScope(Some(scope))
          (arglist zip lst) foreach {
            t => closure.put(t._1.id, t._2)
          }
          apply(body)(closure)
        }
      }
      case other => throw new ScratInvalidTypeError("expected list of arguments but got" + other)
    }

  def apply(e: Expression)(implicit scope: SScope): Any = apply(e, None)(scope)

  def apply(e: Expression, auxScope: Option[SScope])(implicit scope: SScope): Any = e match {
    case Number(n) => n
    case SString(s) => s
    case Add(l, r) => binaryDouble(l, r)(_ + _)
    case Subtract(l, r) => binaryDouble(l, r)(_ - _)
    case Multiply(l, r) => binaryDouble(l, r)(_ * _)
    case Divide(l, r) => binaryDouble(l, r)(_ / _)
    case Exponent(l, r) => binaryDouble(l, r)(math.pow)
    case Identifier(name) => {
      val tempScope = if (auxScope.isDefined) auxScope.get else scope
      tempScope.get(name) match {
        case Some(v) => v
        case _ => throw new ScratSemanticError(name + " not found")
      }
    }
    case ExpList(lst) => lst map apply
    case FunctionCall(name, args) => {
      val tempScope = if (auxScope.isDefined) auxScope.get else scope
      tempScope.get(name.id) match {
        case Some(f: FunctionVarArg) => f.apply(apply(args))
        case _ => throw new ScratSemanticError("function " + name + "not found")
      }
    }
    case DotAccess(list) => {
      val outerScope = scope
      def step(list: List[Expression])(implicit scope: SScope): Any = list match {
        case Nil => throw new ScratInvalidTokenError("got empty list in DotAccess")
        case e :: Nil => apply(e, Some(scope))(outerScope)
        case head :: tail => apply(head) match {
          case s: SScope => step(tail)(s.unlinked)
          case other => throw new ScratInvalidTypeError("expected scope, got " + other)
        }
      }
      step(list)
    }
    case Assignment(target, exp) => {
      val e = apply(exp)
      val targetScope = target.lst match {
        case e :: Nil => scope
        case lst => lst.init --> apply match {
          case s: SScope => s
          case other => throw new ScratInvalidTypeError("expected scope, got " + other)
        }
      }
      target.lst.last match {
        case Identifier(id) => targetScope.put(id, apply(exp))
        case other => throw new ScratInvalidTypeError("expected identifier, got " + other)
      }
      e
    }
    case IfThenElse(pred, then, els) => apply(pred) match {
      case d: Double => if (d != 0) apply(then) else apply(els)
      case other => throw new ScratInvalidTypeError("expected a number, got " + other)
    }
    case Equals(l, r) => (if (apply(l) == apply(r)) 1 else 0): Double
    case NotEquals(l, r) => (if (apply(l) != apply(r)) 1 else 0): Double
    case FunctionDef(name, args, body) => {
      val fun = createFunFromAst(args, body, scope)
      name foreach (n => scope.put(n.id, fun))
      fun
    }

    case t => throw new ScratInvalidTokenError(t + " not implemented in evaluator")
  }

}

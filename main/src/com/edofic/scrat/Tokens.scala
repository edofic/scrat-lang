package com.edofic.scrat

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:14 PM
 */
object Tokens {

  sealed trait Expression

  case class Number(value: Double) extends Expression

  case class Identifier(id: String) extends Expression

  case class SString(s: String) extends Expression

  case class Add(left: Expression, right: Expression) extends Expression

  case class Subtract(left: Expression, right: Expression) extends Expression

  case class Multiply(left: Expression, right: Expression) extends Expression

  case class Divide(left: Expression, right: Expression) extends Expression

  case class Exponent(left: Expression, right: Expression) extends Expression

  case class ExpList(lst: List[Expression]) extends Expression

  case class FunctionCall(function: Expression, args: ExpList) extends Expression

  case class Assignment(to: DotAccess, from: Expression) extends Expression

  case class IfThenElse(predicate: Expression, then: List[Expression], els: List[Expression]) extends Expression

  case class Equals(left: Expression, right: Expression) extends Expression

  case class NotEquals(left: Expression, right: Expression) extends Expression

  case class FunctionDef(name: Option[Identifier], args: List[Identifier], body: List[Expression]) extends Expression

  case class DotAccess(lst: List[Expression]) extends Expression

}

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

  case class FunctionCall(name: Identifier, args: ExpList) extends Expression

  case class Assignment(to: Identifier, from: Expression) extends Expression

  case class IfThenElse(predicate: Expression, then: Expression, els: Expression) extends Expression

  case class Equals(left: Expression, right: Expression) extends Expression

  case class NotEquals(left: Expression, right: Expression) extends Expression

}

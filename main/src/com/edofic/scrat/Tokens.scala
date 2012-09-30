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

  case class BinaryOp(op: BinaryOp.Binary, left: Expression, right: Expression) extends Expression

  case class ExpList(lst: List[Expression]) extends Expression

  case class FunctionCall(function: Expression, args: ExpList) extends Expression

  case class Assignment(to: DotAccess, from: Expression) extends Expression

  case class IfThenElse(predicate: Expression, then: List[Expression], els: List[Expression]) extends Expression

  case class Equality(op: Equality.Operator, left: Expression, right: Expression) extends Expression

  case class FunctionDef(name: Option[Identifier], args: List[Identifier], body: List[Expression]) extends Expression

  case class DotAccess(lst: List[Expression]) extends Expression

  object Equality {

    sealed trait Operator

    case object |== extends Operator

    case object |!= extends Operator

    case object |< extends Operator

    case object |> extends Operator

    case object |>= extends Operator

    case object |<= extends Operator

  }

  object BinaryOp {

    sealed trait Binary

    case object Add extends Binary

    case object Subtract extends Binary

    case object Multiply extends Binary

    case object Divide extends Binary

    case object Exponent extends Binary

  }

}

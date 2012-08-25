package com.edofic.lang

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:14 PM
 */
object Tokens {

  sealed trait Expression

  case class Number(n: Double) extends Expression

  case class Add(left: Expression, right: Expression) extends Expression

  case class Subtract(left: Expression, right: Expression) extends Expression

  case class Multiply(left: Expression, right: Expression) extends Expression

  case class Divide(left: Expression, right: Expression) extends Expression

}

package com.edofic.scrat.translater

import com.edofic.scrat.Tokens._
import com.edofic.scrat.Util.Exceptions._

/**
 * User: andraz
 * Date: 9/3/12
 * Time: 2:18 PM
 */
object TranslateToScala {

  def apply(name: String, es: List[Expression]): String = {
    String.format(
      """
        |object %s extends BaseClass{
        |  def main(args: Array[String]) {
        |    %s
        |  }
        |}
      """.stripMargin, name, es map apply mkString ("\n    "))
  }

  def apply(e: Expression): String = e match {
    case Number(n) => n.toString
    case SString(s) => "\"" + s + "\""
    case Add(l, r) => String.format("(%s + %s)", apply(l), apply(r))
    case Subtract(l, r) => String.format("(%s - %s)", apply(l), apply(r))
    case Multiply(l, r) => String.format("(%s * %s)", apply(l), apply(r))
    case Divide(l, r) => String.format("(%s / %s)", apply(l), apply(r))
    case Exponent(l, r) => String.format("math.pow(%s, %s)", apply(l), apply(r))
    case Identifier(name) => String.format("getId(%s)", name)
    case ExpList(lst) => (lst map apply) mkString ", "
    case FunctionCall(name, args) => String.format("applyFunc(%s, %s)",
      "\"" + name.id + "\"", apply(args))

    case t => throw new ScratInvalidTokenError(t + " not implemented in evaluator")
  }
}

package com.edofic.scrat

import util.parsing.combinator.RegexParsers
import com.edofic.scrat.Util.Exceptions.ScratSyntaxError

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:15 PM
 */
object Parser extends RegexParsers {

  import Tokens._

  private def number: Parser[Number] = """\d+\.?\d*""".r ^^ {
    s => Number(s.toDouble)
  }

  private def identifier: Parser[Identifier] = "[a-zA-Z]\\w*".r ^^ {
    s => Identifier(s)
  }

  private def list: Parser[ExpList] = repsep(expr, ",") ^^ {
    lst => ExpList(lst)
  }

  private def arglist: Parser[ExpList] = "(" ~> list <~ ")"

  private def functionCall: Parser[FunctionCall] = identifier ~ arglist ^^ {
    case id ~ args => FunctionCall(id, args)
  }

  private def value: Parser[Expression] = number | (identifier ||| functionCall)

  private def exponent: Parser[Expression] = (value | parenExpr) ~ "^" ~ (value | parenExpr) ^^ {
    case a ~ "^" ~ b => Exponent(a, b)
  }

  private def factor: Parser[Expression] = (value ||| exponent) | parenExpr

  private def term: Parser[Expression] = factor ~ rep(("*" | "/") ~ factor) ^^ {
    case head ~ tail => {
      var tree: Expression = head
      tail.foreach {
        case "*" ~ e => tree = Multiply(tree, e)
        case "/" ~ e => tree = Divide(tree, e)
      }
      tree
    }
  }

  private def parenExpr: Parser[Expression] = "(" ~> expr <~ ")"

  private def expr: Parser[Expression] = term ~ rep(("+" | "-") ~ term) ^^ {
    case head ~ tail => {
      var tree: Expression = head
      tail.foreach {
        case "+" ~ e => tree = Add(tree, e)
        case "-" ~ e => tree = Subtract(tree, e)
      }
      tree
    }
  }

  def apply(s: String): Expression = parseAll(expr, s) match {
    case Success(tree, _) => tree
    case e: NoSuccess => throw new ScratSyntaxError("parsing error")
  }
}
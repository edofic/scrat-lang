package com.edofic.lang

import util.parsing.combinator.RegexParsers

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:15 PM
 */
object Parser extends RegexParsers {

  import Tokens._

  private def number: Parser[Expression] = """\d+\.?\d*""".r ^^ {
    s => Number(s.toDouble)
  }

  private def factor: Parser[Expression] = number | ("(" ~> expr <~ ")")

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

  def apply(s: String) = parseAll(expr, s) match {
    case Success(tree, _) => Some(tree)
    case e: NoSuccess => None
  }
}

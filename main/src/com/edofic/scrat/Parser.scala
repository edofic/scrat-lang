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


  override protected val whiteSpace = """[ \t\x0B\f\r]""".r

  private def number: Parser[Number] = """\d+\.?\d*""".r ^^ {
    s => Number(s.toDouble)
  }

  private def identifier: Parser[Identifier] = "[a-zA-Z]\\w*".r ^^ {
    s => Identifier(s)
  }

  private def string: Parser[SString] = "\".*?\"".r ^^ {
    s => SString(s.substring(1, s.length - 1))
  }

  private def commaList: Parser[ExpList] = repsep(expr, ",") ^^ {
    lst => ExpList(lst)
  }

  private def arglist: Parser[ExpList] = "(" ~> commaList <~ ")"

  private def functionCall: Parser[FunctionCall] = identifier ~ arglist ^^ {
    case id ~ args => FunctionCall(id, args)
  }

  private def value: Parser[Expression] = number | string | (identifier ||| functionCall)

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

  private def sum: Parser[Expression] = term ~ rep(("+" | "-") ~ term) ^^ {
    case head ~ tail => {
      var tree: Expression = head
      tail.foreach {
        case "+" ~ e => tree = Add(tree, e)
        case "-" ~ e => tree = Subtract(tree, e)
      }
      tree
    }
  }

  private def assignment: Parser[Assignment] = identifier ~ "=" ~ expr ^^ {
    case id ~ "=" ~ exp => Assignment(id, exp)
  }

  private def ifThenElse: Parser[IfThenElse] = "if" ~ expr ~ "then" ~ expr ~ "else" ~ expr ^^ {
    case "if" ~ predicate ~ "then" ~ then ~ "else" ~ els => IfThenElse(predicate, then, els)
  }

  private def equality: Parser[Expression] = noEqExpr ~ rep(("==" | "!=") ~ noEqExpr) ^^ {
    case head ~ tail => {
      var tree: Expression = head
      tail.foreach {
        case "==" ~ e => tree = Equals(tree, e)
        case "!=" ~ e => tree = NotEquals(tree, e)
      }
      tree
    }
  }

  private def noEqExpr: Parser[Expression] = sum ||| assignment ||| ifThenElse

  private def expr: Parser[Expression] = noEqExpr ||| equality ||| functionDef

  private def exprList: Parser[List[Expression]] = repsep(expr, "\\n+".r)

  private def block: Parser[List[Expression]] = """\{\n*""".r ~> exprList <~ """\n*\}""".r

  private def functionDef: Parser[FunctionDef] =
    "func" ~ identifier ~ ("(" ~> repsep(identifier, ",") <~ ")") ~ block ^^ {
      case "func" ~ id ~ args ~ body => FunctionDef(id, args, body)
    }

  def apply(s: String): List[Expression] = parseAll(exprList, s) match {
    case Success(tree, _) => tree
    case Failure(msg, next) => throw new ScratSyntaxError("parsing failure: " + msg + " near: " + next.rest.source)
    case Error(msg, next) => throw new ScratSyntaxError("parsing error: " + msg + " near: " + next.source)
    case NoSuccess(msg, next) => throw new ScratSyntaxError("parser NoSuccess: " + msg + " near: " + next.source)
  }
}
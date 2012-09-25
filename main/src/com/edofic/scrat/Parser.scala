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

  override protected val whiteSpace = """[ \t\x0B\f\r]+""".r

  private def number: Parser[Number] = """\d+\.?\d*""".r ^^ {
    s => Number(s.toDouble)
  }

  private def simpleIdentifier: Parser[Identifier] = "[a-zA-Z]\\w*".r ^^ {
    s => Identifier(s)
  }

  private def identifier: Parser[DotAccess] = rep1sep((functionCall | simpleIdentifier), ".") ^^ DotAccess.apply

  private def string: Parser[SString] = "\".*?\"".r ^^ {
    s => SString(s.substring(1, s.length - 1))
  }

  private def commaList: Parser[ExpList] = repsep(expr, ",") ^^ {
    lst => ExpList(lst)
  }

  private def arglist: Parser[ExpList] = "(" ~> commaList <~ ")"

  private def functionCall: Parser[FunctionCall] = simpleIdentifier ~ arglist ^^ {
    case id ~ args => FunctionCall(id, args)
  }

  private def value: Parser[Expression] = number | string | identifier | functionCall

  private def exponent: Parser[Expression] = (value | parenExpr) ~ "^" ~ (value | parenExpr) ^^ {
    case a ~ "^" ~ b => Exponent(a, b)
  }

  private def factor: Parser[Expression] = parenExpr | exponent | value

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

  private def ifThenElse: Parser[IfThenElse] = "if" ~ expr ~ "then" ~ (expr | block) ~ "else" ~ (expr | block) ^^ {
    case "if" ~ predicate ~ "then" ~ then ~ "else" ~ els => {
      val thenBlock = then match {
        case e: Expression => List(e)
        case b: List[Expression] => b
      }
      val elseBlock = els match {
        case e: Expression => List(e)
        case b: List[Expression] => b
      }
      IfThenElse(predicate, thenBlock, elseBlock)
    }
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

  private def noEqExpr: Parser[Expression] = ifThenElse | assignment | sum

  private def expr: Parser[Expression] = functionDef | equality | noEqExpr

  private def parenExpr: Parser[Expression] = "(" ~> expr <~ ")"

  def exprList: Parser[List[Expression]] = rep("\n") ~> repsep(expr, rep1("\n")) <~ rep("\n")

  def block: Parser[List[Expression]] = ("{" ~ rep("\n")) ~> repsep(expr, rep("\n")) <~ (rep("\n") ~ "}")

  private def functionDef: Parser[Expression] =
    "func" ~> opt(simpleIdentifier) ~ ("(" ~> repsep(simpleIdentifier, ",") <~ ")") ~ block ~ opt(arglist) ^^ {
      case id ~ args ~ body ~ params => {
        val f = FunctionDef(id, args, body)
        if (params.isEmpty) f else FunctionCall(f, params.get)
      }
    }

  def apply(s: String): List[Expression] = parseAll(exprList, s) match {
    case Success(tree, _) => tree
    case Failure(msg, next) => throw new ScratSyntaxError("parsing failure: " + msg + " near: " + next.rest.source)
    case Error(msg, next) => throw new ScratSyntaxError("parsing error: " + msg + " near: " + next.source)
    case NoSuccess(msg, next) => throw new ScratSyntaxError("parser NoSuccess: " + msg + " near: " + next.source)
  }
}
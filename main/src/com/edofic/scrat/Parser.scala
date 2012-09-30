package com.edofic.scrat

import util.parsing.combinator.{PackratParsers, JavaTokenParsers}
import com.edofic.scrat.Util.Exceptions.ScratSyntaxError
import com.edofic.scrat.Tokens.Equality._
import com.edofic.scrat.Tokens.BinaryOp._

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:15 PM
 */
object Parser extends JavaTokenParsers with PackratParsers {

  import Tokens._

  override protected val whiteSpace = """[ \t\x0B\f\r]+""".r

  private lazy val number: PackratParser[Number] = decimalNumber ^^ {
    s => Number(s.toDouble)
  }

  private lazy val simpleIdentifier: PackratParser[Identifier] = ident ^^ {
    s => Identifier(s)
  }

  private lazy val identifier: PackratParser[DotAccess] = rep1sep((functionCall | simpleIdentifier), ".") ^^ DotAccess.apply

  private lazy val string: PackratParser[SString] = "\".*?\"".r ^^ {
    s => SString(s.substring(1, s.length - 1))
  }

  private lazy val commaList: PackratParser[ExpList] = repsep(expr, ",") ^^ {
    lst => ExpList(lst)
  }

  private lazy val arglist: PackratParser[ExpList] = "(" ~> commaList <~ ")"

  private lazy val functionCall: PackratParser[FunctionCall] = simpleIdentifier ~ rep1(arglist) ^^ {
    case id ~ args => args.tail.foldLeft(FunctionCall(id, args.head)) {
      case (tree, elem) => FunctionCall(tree, elem)
    }
  }

  private lazy val value: PackratParser[Expression] = number | string | identifier | functionCall

  private lazy val exponent: PackratParser[Expression] = (value | parenExpr) ~ "^" ~ (value | parenExpr) ^^ {
    case a ~ "^" ~ b => BinaryOp(Exponent, a, b)
  }

  private lazy val factor: PackratParser[Expression] = parenExpr | exponent | value

  private lazy val term: PackratParser[Expression] = factor ~ rep(("*" | "/") ~ factor) ^^ {
    case head ~ tail => tail.foldLeft(head) {
      case (tree, "*" ~ e) => BinaryOp(Multiply, tree, e)
      case (tree, "/" ~ e) => BinaryOp(Divide, tree, e)
    }
  }

  private lazy val sum: PackratParser[Expression] = term ~ rep(("+" | "-") ~ term) ^^ {
    case head ~ tail => tail.foldLeft(head) {
      case (tree, "+" ~ e) => BinaryOp(Add, tree, e)
      case (tree, "-" ~ e) => BinaryOp(Subtract, tree, e)
    }
  }

  private lazy val assignment: PackratParser[Assignment] = identifier ~ "=" ~ expr ^^ {
    case id ~ "=" ~ exp => Assignment(id, exp)
  }

  private lazy val ifThenElse: PackratParser[IfThenElse] = "if" ~ expr ~ "then" ~ (expr | block) ~ "else" ~ (expr | block) ^^ {
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

  private lazy val equality: PackratParser[Expression] =
    noEqExpr ~ rep("==|!=|<=|>=|<|>".r ~ noEqExpr) ^^ {
      case head ~ tail => tail.foldLeft(head) {
        case (tree, op ~ e) => Equality(op match {
          case "==" => |==
          case "!=" => |!=
          case "<" => |<
          case ">" => |>
          case "<=" => |<=
          case ">=" => |>=
        }, tree, e)
      }
    }

  private lazy val noEqExpr: PackratParser[Expression] = ifThenElse | assignment | sum

  private lazy val expr: PackratParser[Expression] = functionDef | equality | noEqExpr

  private lazy val parenExpr: PackratParser[Expression] = "(" ~> expr <~ ")"

  private lazy val exprList: PackratParser[List[Expression]] = rep("\n") ~> repsep(expr, rep1("\n")) <~ rep("\n")

  private lazy val block: PackratParser[List[Expression]] =
    ("{" ~ rep("\n")) ~> repsep(expr, rep("\n")) <~ (rep("\n") ~ "}") |
      expr ^^ (List(_))

  private lazy val functionDef: PackratParser[Expression] =
    "func" ~> opt(simpleIdentifier) ~ rep1("(" ~> repsep(simpleIdentifier, ",") <~ ")") ~ block ~ opt(arglist) ^^ {
      case id ~ args ~ body ~ params => {
        def build(argListList: List[List[Identifier]]): List[Expression] = argListList match {
          case Nil => body
          case head :: tail => List(FunctionDef(None, head, build(tail)))
        }
        val f = FunctionDef(id, args.head, build(args.tail))
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
package com.edofic.scrat.nut

import com.edofic.scrat.Tokens._

/**
 * User: andraz
 * Date: 10/10/12
 * Time: 9:06 AM
 */
object GenerateJs {
  def apply(lst: List[Expression]): String = lst map apply mkString ";\n"

  def apply(exp: Expression): String  =  exp match {
    case Number(d) => d.toString
    case BinaryOp(op, left, right) =>
      val l = "("+apply(left)+")"
      val r = "("+apply(right)+")"
      op match {
        case BinaryOp.Add => l+"+"+r
        case BinaryOp.Subtract => l+"-"+r
        case BinaryOp.Multiply => l+"*"+r
        case BinaryOp.Divide => l+"/"+r
        case BinaryOp.Exponent => "Math.pow(%s, %s)" format (l,r)
      }
    case Identifier(id) => id
    //case This extends Identifier("this")
    case SString(s: String) => "\"" + s + "\""
    case ExpList(lst: List[Expression]) => lst map apply mkString ", "
    case FunctionCall(func, args) => "%s(%s)" format (apply(func), args.lst map apply mkString ", ")

    case Assignment(DotAccess(List(Identifier(name))), `exp`) => "var %s = %s" format (name, apply(exp))
    case Assignment(to: DotAccess, from: Expression) => "%s = %s" format (apply(to), apply(from))
    case IfThenElse(predicate: Expression, then: List[Expression], els: List[Expression]) =>
      """(function(){
        |    if(%s){
        |       %s
        |       return %s;
        |    } else {
        |       %s;
        |       return %s;
        |    }
        |}())""".stripMargin format (
        apply(predicate),
        (then.init) map apply mkString ";\n",
        apply(then.last),
        (els.init) map apply mkString ";\n",
        apply(els.last))
    case Equality(op: Equality.Operator, left: Expression, right: Expression) => {
      val opstr = op match {
        case Equality.|== => "==="
        case Equality.|!= => "!=="
        case Equality.|<= => "<="
        case Equality.|>= => ">="
        case Equality.|< => "<"
        case Equality.|> => ">"
      }
      apply(left) + opstr + apply(right)
    }
    //case FunctionDef(name: Option[Identifier], args: List[Identifier], body: List[Expression]) extends Expression
    case DotAccess(lst: List[Expression]) => lst map apply mkString "."
    case ArrayLiteral(xs: Array[Expression]) => xs map apply mkString ("[", ",", "]")
  }

}

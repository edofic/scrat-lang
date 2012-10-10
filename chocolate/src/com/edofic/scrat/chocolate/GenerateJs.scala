package com.edofic.scrat.chocolate

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
  }

}

package com.edofic.scrat

import com.edofic.scrat.Tokens._
import collection.immutable.Queue

/**
 * User: andraz
 * Date: 10/23/12
 * Time: 10:25 AM
 */
object TreeOps {
  def children(expr: Expression): List[Expression] = expr match {
    case  Number(value) => Nil

    case  Identifier(id) => Nil

    case  SString(s) => Nil

    case  BinaryOp(op, left , right ) => List(left, right)

    case  ExpList(lst) => lst

    case  FunctionCall(function, args: ExpList) => List(function, args)

    case  Assignment(to, from ) => List(to, from)

    case  IfThenElse(predicate, then, els) => predicate::then:::els

    case  Equality(op, left, right ) => List(left, right)

    case  FunctionDef(name, args, body) => (if (name.isDefined) name.get::Nil else Nil):::args:::body

    case  DotAccess(lst) => lst

    case WhileLoop(condition, body) => condition::body

    case  ArrayLiteral(xs: Array[Expression]) => xs.toList
  }

  private val expId: PartialFunction[Expression, Expression] = {
    case any => any
  }

  type Modification = PartialFunction[Expression, Expression]

  def applyRecursiveModificaton(expr: Expression)(func: Modification): Expression = {
    def rec[A<:Expression](e:A) = applyRecursiveModificaton(e)(func).asInstanceOf[A]
    val e = expr match {
      case  e: Number => e

      case  e: Identifier => e

      case  e: SString => e

      case  BinaryOp(op, left , right ) => BinaryOp(op, rec(left), rec(right))

      case  ExpList(lst) => ExpList(lst map rec)

      case  FunctionCall(function, args: ExpList) => FunctionCall(rec(function), rec(args))

      case  Assignment(to, from ) => Assignment(rec(to), rec(from))

      case  IfThenElse(predicate, then, els) => IfThenElse(rec(predicate), then map rec, els map rec)

      case  Equality(op, left, right ) => Equality(op, rec(left), rec(right))

      case  FunctionDef(name, args, body) => FunctionDef(name map rec, args map rec, body map rec)

      case  DotAccess(lst) => DotAccess(lst map rec)

      case WhileLoop(condition, body) => WhileLoop(rec(condition), body map rec)

      case  ArrayLiteral(xs: Array[Expression]) => ArrayLiteral(xs map rec)
    }
    //println("e: "+e+ " func "+func+" expId "+expId)
    (func orElse expId)(e)
  }

  def bfTraverse(expr: Expression): Stream[Expression] =  {
    import Stream.{#::,empty}
    def queueStream(queue: Queue[Expression]): Stream[Expression] = {
      if (queue.isEmpty)
        empty
      else{
        val (head, dequed) = queue.dequeue
        head #:: queueStream(dequed.enqueue(children(head)))
      }
    }
    queueStream(Queue(expr))
  }

  def bfTraverse(lst: Seq[Expression]): Stream[Expression] = {
    import com.edofic.scrat.Tokens.ExpList
    val str = bfTraverse(ExpList(lst.toList))
    if (str.isEmpty)
      Stream.empty
    else
      str.tail
  }
}

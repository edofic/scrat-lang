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

    case  ArrayLiteral(xs: Array[Expression]) => xs.toList
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

package com.edofic.scrat

import com.edofic.scrat.Util.Exceptions._

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:25 PM
 */
object StdLib {
  type FunctionVarArg = Any => Any

  val identifiers: Map[String, Any] = Map(
    ("pi" -> math.Pi),
    ("e" -> math.E),
    ("ln" -> functions.ln),
    ("log" -> functions.log),
    ("print" -> functions.sprint),
    ("println" -> functions.sprintln)
  )

  def apply(key: String): Option[Any] = identifiers.get(key)

  private object functions {
    lazy val ln: FunctionVarArg = {
      case (d: Double) :: Nil => math.log(d)
      case other => throw ScratInvalidTypeError("expected single double but got " + other)
    }

    lazy val log: FunctionVarArg = {
      case (d: Double) :: Nil => math.log10(d)
      case other => throw ScratInvalidTypeError("expected single double but got " + other)
    }

    lazy val sprint: FunctionVarArg = {
      case lst: List[_] => print(lst.mkString(" "))
      case other => throw ScratInvalidTypeError("expected a list but got " + other)
    }

    lazy val sprintln: FunctionVarArg = {
      case lst: List[_] => println(lst.mkString(" "))
      case other => throw ScratInvalidTypeError("expected a list but got " + other)
    }
  }

}

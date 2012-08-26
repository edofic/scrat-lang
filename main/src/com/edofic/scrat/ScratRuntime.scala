package com.edofic.scrat

import com.edofic.scrat.Util.Exceptions._
import Util.Implicits._

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:25 PM
 */
object ScratRuntime {
  type FunctionVarArg = Any => Any
}

class ScratRuntime {

  import ScratRuntime.FunctionVarArg

  val evaluator = new Evaluator(this)

  private val identifiers: collection.mutable.Map[String, Any] = collection.mutable.Map(
    ("pi" -> math.Pi),
    ("e" -> math.E),
    ("ln" -> functions.ln),
    ("log" -> functions.log),
    ("mkString" -> functions.mkString),
    ("print" -> functions.sprint),
    ("println" -> functions.sprintln),
    ("readln" -> functions.sreadln)
  )

  val get: String => Option[Any] = identifiers.get
  val put: (String, Any) => Option[Any] = identifiers.put

  def eval(s: String) = s --> Parser.apply --> evaluator.apply

  private object functions {
    lazy val ln: FunctionVarArg = {
      case (d: Double) :: Nil => math.log(d)
      case other => throw ScratInvalidTypeError("expected single double but got " + other)
    }

    lazy val log: FunctionVarArg = {
      case (d: Double) :: Nil => math.log10(d)
      case other => throw ScratInvalidTypeError("expected single double but got " + other)
    }

    lazy val mkString: FunctionVarArg = {
      case lst: List[_] => lst.mkString(" ")
      case other => throw ScratInvalidTypeError("expected a list but got " + other)
    }

    lazy val sprint: FunctionVarArg = {
      case lst: List[_] => lst --> mkString --> print
      case other => throw ScratInvalidTypeError("expected a list but got " + other)
    }

    lazy val sprintln: FunctionVarArg = {
      case lst: List[_] => lst --> mkString --> println
      case other => throw ScratInvalidTypeError("expected a list but got " + other)
    }

    lazy val sreadln: FunctionVarArg = _ => {
      readLine()
    }
  }

}

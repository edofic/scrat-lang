package com.edofic.scrat

import Util.Implicits._
import com.edofic.scrat.Util.Exceptions._

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:25 PM
 */
object ScratRuntime {
  type FunctionVarArg = Any => Any
}

class ScratRuntime {
  val evaluator = new Evaluator
  implicit val globalScope = new SScope(Some(StdLib))

  val eval = Parser.apply _ andThen Optimizer.apply andThen evaluator.apply

  def cleanRoomEval(s: String) = {
    implicit val globalScope = new SScope(Some(StdLib))
    s --> (Parser.apply _ andThen Optimizer.apply andThen evaluator.apply)
  }
}

object StdLib extends SScope(None) {
  val map = Map(
    ("none" -> None),
    ("pi" -> math.Pi),
    ("e" -> math.E),
    ("ln" -> functions.ln),
    ("log" -> functions.log),
    ("mkString" -> functions.mkString),
    ("print" -> functions.sprint),
    ("println" -> functions.sprintln),
    ("readln" -> functions.sreadln),
    ("toNum" -> functions.toNum),
    ("dir" -> functions.dir)
  )

  override def put(key: String, value: Any) {
    throw new ScratNotAllowedError("this is not implemented and shouldn't be")
  }

  override def get(key: String) = map.get(key)

  private object functions {

    import ScratRuntime._

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
      case other => throw ScratInvalidTypeError("expected a commaList but got " + other)
    }

    lazy val sprint: FunctionVarArg = {
      case lst: List[_] => lst --> mkString --> print
      case other => throw ScratInvalidTypeError("expected a commaList but got " + other)
    }

    lazy val sprintln: FunctionVarArg = {
      case lst: List[_] => lst --> mkString --> println
      case other => throw ScratInvalidTypeError("expected a commaList but got " + other)
    }

    lazy val sreadln: FunctionVarArg = _ => {
      readLine()
    }

    lazy val toNum: FunctionVarArg = {
      case (s: String) :: Nil => s.toDouble
      case (d: Double) :: Nil => d
      case other => throw ScratInvalidTypeError("expected a string or a double but got " + other)
    }

    lazy val dir: FunctionVarArg = {
      case (s: SScope) :: Nil => s.getDecription
      case other => throw ScratInvalidTypeError("expected a scope::Nil but got " + other)
    }


  }

}

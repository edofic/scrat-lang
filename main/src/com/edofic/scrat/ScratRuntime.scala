package com.edofic.scrat

import com.edofic.scrat.Util.Exceptions._
import Util.Implicits._
import com.edofic.scrat.Tokens.{Identifier, Expression}

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:25 PM
 */
object ScratRuntime {
  type FunctionVarArg = Any => Any
  type StoredFunction = (Evaluator, Any) => Any

  def createFunFromAst(arglist: List[Identifier], body: List[Expression], scope: SScope): StoredFunction =
    (eval: Evaluator, args: Any) => args match {
      case lst: List[Any] => {
        if (lst.length != arglist.length) {
          throw new ScratInvalidTypeError("expected " + arglist.length + " arguments, but got " + lst.length)
        } else {
          val closure = new SScope(Some(scope))
          (arglist zip lst) foreach {
            t => closure.put(t._1.id, t._2)
          }
          eval(body)(closure)
        }
      }
      case other => throw new ScratInvalidTypeError("expected list of arguments but got" + other)
    }
}

class ScratRuntime {
  val evaluator = new Evaluator(this)
  implicit val globalScope = new SScope(Some(StdLib))

  def eval(s: String) = s --> Parser.apply --> evaluator.apply
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
    ("toNum" -> functions.toNum)
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
  }

}

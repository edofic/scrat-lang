package com.edofic.scrat.translater

import com.edofic.scrat._
import Util.Exceptions._
import ScratRuntime._

/**
 * User: andraz
 * Date: 9/3/12
 * Time: 3:19 PM
 */
abstract class BaseClass {
  implicit val scope = new SScope(Some(StdLib))

  def applyFunc(name: String, args: Any) = scope.get(name) match {
    case Some(f: FunctionVarArg) => f.apply(args)
    case _ => throw new ScratSemanticError("function " + name + "not found")
  }

  def getId(name: String) = scope.get(name) match {
    case Some(v) => v
    case None => throw new ScratSemanticError(name + "not found")
  }

  def main(args: Array[String])
}

package com.edofic.scrat

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 11:08 PM
 */
object Util {
  object Exceptions {
    case class SyntaxError(msg: String) extends Exception(msg)
    case class SemanticError(msg: String) extends Exception(msg)
    case class InvalidToken(msg: String) extends Exception(msg)
  }

  object Implicits{
    //allow visual representation of data flow
    implicit def any2applyFunc[A](a: A) = new AnyRef{
      def -->[B](f: A=>B): B = f(a)
    }
  }
}

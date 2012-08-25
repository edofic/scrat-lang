package com.edofic.lang

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:16 PM
 */
object Repl {
  def apply(s: String) = Parser(s) map Evaluator.apply

  def repl = {
    def rep: Unit = {
      print(">> ")
      readLine match {
        case "exit" => return
        case "help" => println("enter an expression\nsupported: floats + - * / ( )")
        case exp => {
          println(
            apply(exp) match {
              case Some(res) => res
              case None => "invalid syntax"
            })
          rep
        }
      }
    }
    rep
  }

  def main(args: Array[String]) = repl
}

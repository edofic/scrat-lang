package com.edofic.scrat

import com.edofic.scrat.Util.Exceptions._

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:16 PM
 */
object Repl {
  val runtime = new ScratRuntime

  def apply(s: String) = runtime.eval(s)

  def repl() {
    def rep() {
      print(">> ")
      readLine() match {
        case "exit" => return
        case "help" => println("enter an expression\nsupported: floats + - * / ( )")
        case exp => {
          try {
            println(apply(exp))
          } catch {
            case ScratSemanticError(msg) => println("semantic error: " + msg)
            case ScratSyntaxError(msg) => println("syntax error: " + msg)
            case ScratInvalidTypeError(msg) => println("type error: " + msg)
          }
        }
      }
      rep()
    }
    rep()
  }

  def main(args: Array[String]) {
    repl()
  }
}

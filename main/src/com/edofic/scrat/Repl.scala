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

  private val help =
    """
      |Commands
      |:exit          exits program
      |:run filename  loads and interprets the file
      |:help          prints this message
      |
      |Language:
      |-mathematical expressions containing
      |   + - * / ( ) ^(exponent) variable-names function application-e.g. ln(10), println(1,2,3)
      |-strings
      |-assignments
      |   x = ln(4)
      |   println("x is ", x)
      |-standard library
      |   constants: pi, e
      |   functions: ln, log, print, println, readln, mkString
    """.stripMargin

  def repl() {
    val Run = """:run (.+)""".r

    println("Welcome to Scrat Language REPL")
    println("type in expressions to evaluate them")
    println("type :help for more information")
    def rep() {
      println()
      print(">> ")
      readLine() match {
        case ":exit" => return
        case ":help" => println(help)
        case Run(filename) => Interpreter.main(Array(filename))
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

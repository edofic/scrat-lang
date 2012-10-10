package com.edofic.scrat

import com.edofic.scrat.Util.Exceptions._
import Util.Implicits._

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
      |:run filename  loads and interprets the path in new scope
      |:load filename loads and interprets the path in global scope
      |:help          prints this message
      |
      |Append | to then end of a line to enter multiline expressions
    """.stripMargin

  def repl() {
    val Run = """:run (.+)""".r
    val Load = """:load (.+)""".r

    println("Welcome to Scrat Language REPL")
    println("type in expressions to evaluate them")
    println("type :help for more information")
    def rep(buffer: String) {
      print(">> ")
      readLine() match {
        case ":exit" => return
        case ":help" => println(help)
        case Run(filename) => Interpreter.runFile(filename)
        case Load(filename) => Interpreter.interpretFile(filename, runtime)
        case exp => {
          try {
            if (exp.charAt(exp.length - 1) == '|') {
              (buffer + "\n" + exp.substring(0, exp.length - 1)) --> rep
            } else {
              (buffer + "\n" + exp) --> runtime.eval --> println
            }
          } catch {
            case ScratSemanticError(msg) => println("semantic error: " + msg)
            case ScratSyntaxError(msg) => println("syntax error: " + msg)
            case ScratInvalidTypeError(msg) => println("type error: " + msg)
            case ScratNotAllowedError(msg) => println("not allowed: "+ msg)
            case _: StringIndexOutOfBoundsException => () //empty line
          }
        }
      }
      rep("")
    }
    rep("")
  }

  def main(args: Array[String]) {
    repl()
  }
}

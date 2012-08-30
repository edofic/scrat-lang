package com.edofic.scrat

import java.io.File

/**
 * User: andraz
 * Date: 8/26/12
 * Time: 12:13 PM
 */
object Interpreter {
  def main(args: Array[String]) {
    if (args.length != 1) {
      println("no filename in arguments -> interactive mode")
      Repl.main(args)
    } else {
      runFile(args(0))
    }
  }

  def runFile(path: String) = interpretFile(path, new ScratRuntime)

  def interpretFile(path: String, runtime: ScratRuntime) {
    if (new File(path).canRead) {
      val source = io.Source.fromFile(path)
      val content = source.mkString
      source.close()
      runtime.eval(content)
    } else {
      println("cannot open path " + path)
    }
  }
}

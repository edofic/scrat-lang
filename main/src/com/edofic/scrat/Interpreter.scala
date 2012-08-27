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
      interpretFile(new File(args(0)))
    }
  }

  def interpretFile(file: File) {
    val runtime = new ScratRuntime
    if (file.canRead) {
      val source = io.Source.fromFile(file)
      val content = source.mkString
      source.close()
      runtime.eval(content)
    } else {
      println("cannot open file " + file.getPath)
    }
  }
}

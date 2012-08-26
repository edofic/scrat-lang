package com.edofic.scrat

import java.io.File

/**
 * User: andraz
 * Date: 8/26/12
 * Time: 12:13 PM
 */
object Interpreter {
  def main(args: Array[String]) = {
    if (args.length != 1) {
      println("parameters: filename to interpret")
    } else {
      interpretFile(new File(args(0)))
    }
  }

  val runtime = new ScratRuntime

  def interpretFile(file: File) {
    if (file.canRead) {
      val source = io.Source.fromFile(file)
      source.getLines().foreach(runtime.eval)
      source.close()
    } else {
      println("cannot open file " + file.getPath)
    }
  }
}

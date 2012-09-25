package com.edofic.scrat.test

import testing.Benchmark
import com.edofic.scrat._
import scala.Some

/**
 * User: andraz
 * Date: 9/3/12
 * Time: 9:24 AM
 */
object Benchmarks {
  val program = TestPrograms.tuples(11)._2
  val runtime = new ScratRuntime
  val evaluator = new Evaluator
  implicit val globalScope = new SScope(Some(StdLib))
  val parser = Parser
  val tree = parser(program)

  def main(args: Array[String]) {
    println("Parser benchmark")
    ParserBench.main(Array("10"))
    println()
    println("Evaluator benchmark")
    EvaluatorBench.main(Array("10"))
  }


  object ParserBench extends Benchmark {
    def run = parser(program)
  }

  object EvaluatorBench extends Benchmark {
    def run = evaluator(tree)
  }

}

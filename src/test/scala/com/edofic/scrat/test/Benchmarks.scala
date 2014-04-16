package com.edofic.scrat.test

import testing.Benchmark
import com.edofic.scrat._
import scala.Some
import com.edofic.scrat.Tokens.Expression

/**
 * User: andraz
 * Date: 9/3/12
 * Time: 9:24 AM
 */
object Benchmarks {
  def eval(program: List[Expression]) = {
    implicit val global = new SScope(Some(StdLib))
    Evaluator(program)
  }

  def main(args: Array[String]) {
    TestPrograms.tuples foreach { t=>
      val (name, source, result) = t
      val tree = Parser(source)
      val optimal = Optimizer(tree)
      eval(tree) //dry run to load everything

      println(name)
      println("Parser benchmark")
      new FunctionBenchmark(source, Parser.apply).main(Array("10"))
      println("Optimizer benchmark")
      new FunctionBenchmark[List[Expression]](tree, Optimizer.apply).main(Array("10"))
      println("Evaluator benchmark")
      new FunctionBenchmark(optimal, eval).main(Array("10"))
      println()
    }
  }

  class FunctionBenchmark[A](val input: A, func: A=>Any) extends Benchmark{
    def run() {
      func(input)
    }
  }

}

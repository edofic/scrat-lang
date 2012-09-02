package com.edofic.scrat.test

import org.scalatest.FunSuite
import com.edofic.scrat.ScratRuntime

/**
 * User: andraz
 * Date: 8/27/12
 * Time: 11:43 AM
 */
class Acceptance extends FunSuite {
  val runtime = new ScratRuntime

  import TestPrograms._

  def testProgramTuple(t: Tuple2[String, Any]) = assert(runtime.eval(t._1) === t._2)

  test("addition and subtraction") {
    val exp = "1+2+3+4-7-2"
    val result = runtime.eval(exp)
    val expected = 1 + 2 + 3 + 4 - 7 - 2
    assert(result === expected)
  }

  test("multiplication and division") {
    val exp = "1*3/5*7/2"
    val result = runtime.eval(exp)
    def d(n: Int) = n: Double
    val expected = d(1) * d(3) / d(5) * d(7) / d(2)
    assert(result === expected)
  }

  test("+-*/()^") {
    val exp = "(1+3)*5/2^(5-2)"
    val result = runtime.eval(exp)
    val expected = (1 + 3) * 5 / math.pow(2, (5 - 2))
    assert(result === expected)
  }

  test("dot access recursive functions") {
    testProgramTuple(dotAccessRecFunctions)
  }
}

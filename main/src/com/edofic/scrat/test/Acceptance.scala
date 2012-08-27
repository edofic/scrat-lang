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

  test("random stuff...math, strings, eq, if, functions") {
    val exp =
      """
        |1+1
        |2*2+1
        |2^2
        |"hi"
        |hi = 53
        |ln(ln(ln(ln(1000000))))
        |println("hi")
        |bla = 1
        |boo = "goo"
        |if bla then boo else foo
        |1 == log(10)
        |func f(){}
        |func g(a){
        |
        |}
        |func h(a,bt,e,g) {
        |a
        |bt
        |1+1
        |println("hi")
        |g
        |}
        |func i(){
        |hi }
        |if 1 then {} else {}
        |1
      """.stripMargin
    val result = runtime.eval(exp)
    val expected = 1 //the big problem is parsing here...not concerned with value
    assert(result === expected)
  }
}

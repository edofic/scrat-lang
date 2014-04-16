package com.edofic.scrat.test

import org.scalatest.FunSuite
import com.edofic.scrat.ScratRuntime
import com.edofic.scrat.Util.Exceptions.ScratNotAllowedError

/**
 * User: andraz
 * Date: 8/27/12
 * Time: 11:43 AM
 */
class Acceptance extends FunSuite {
  val runtime = new ScratRuntime

  def testProgramTuple(t: (String, String, Any)) {
    test(t._1)(assert(runtime.cleanRoomEval(t._2) === t._3))
  }

  TestPrograms.tuples foreach testProgramTuple

  test("assigning to this"){
    intercept[ScratNotAllowedError]{
      runtime.cleanRoomEval(
        """
          |this = 1
        """.stripMargin)
    }
  }
}

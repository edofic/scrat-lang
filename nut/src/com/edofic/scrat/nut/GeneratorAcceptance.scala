package com.edofic.scrat.nut

import org.scalatest.FunSuite
import com.edofic.scrat.test.TestPrograms
import com.edofic.scrat.Util.Exceptions.ScratNotAllowedError
import com.edofic.scrat.Util.Implicits._
import sys.process._
import java.io.ByteArrayInputStream
import com.edofic.scrat.Parser
import com.edofic.scrat.Tokens.Expression


/**
 * User: andraz
 * Date: 10/10/12
 * Time: 9:35 AM
 */
class GeneratorAcceptance extends FunSuite {
  def runJs(js: String): String = ("node" #< new ByteArrayInputStream(js.getBytes) !!)

  def eval(scrat: String): String = scrat --> Parser.apply --> modifyTree --> GenerateJs.apply --> runJs
  val e = (Parser.apply _) andThen (modifyTree _) andThen (GenerateJs.apply _) andThen (runJs _)

  def modifyTree(lst: List[Expression]): List[Expression] = {
    import com.edofic.scrat.Tokens._
    if (lst == Nil) {
      Nil
    } else {
      lst.init ::: List(FunctionCall(Identifier("console.log"), ExpList(List(lst.last))))
    }
  }

  def testProgramTuple(t: (String, String, Any)) {
    test(t._1)(assert(eval(t._2).trim === t._3.toString))
  }

  TestPrograms.tuples foreach testProgramTuple

  test("assigning to this"){
    intercept[ScratNotAllowedError]{
      eval(
        """
          |this = 1
        """.stripMargin)
    }
  }
}

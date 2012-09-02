package com.edofic.scrat.test

/**
 * User: andraz
 * Date: 2.9.12
 * Time: 12:32
 */
object TestPrograms {
  val dotAccessRecFunctions = (
    """
      |func object(){this}
      |o = object()
      |o.get = func (n) { if n==0 then n else o.get(n-1) }
      |o.get(2)
    """.stripMargin, 0 toDouble)

}

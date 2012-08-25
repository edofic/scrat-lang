package com.edofic.scrat

/**
 * User: andraz
 * Date: 8/25/12
 * Time: 10:25 PM
 */
object StdLib {
  val identifiers: Map[String, Double] = Map(
    ("pi" -> math.Pi),
    ("e" -> math.E)
  )

  def apply(key: String): Option[Double] = identifiers.get(key)
}

package com.edofic.scrat

/**
 * User: andraz
 * Date: 8/27/12
 * Time: 12:04 PM
 */
class SScope(val parent: Option[SScope]) {
  private val map = collection.mutable.Map[String, Any]()

  map.put("this", this)

  def get(key: String): Option[Any] = map.get(key) match {
    case s: Some[Any] => s
    case None => parent.flatMap(_.get(key))
  }

  def put(key: String, value: Any) {
    map.put(key, value)
  }
}

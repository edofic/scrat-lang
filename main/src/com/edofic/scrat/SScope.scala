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

  def getStrict(key: String): Option[Any] = map.get(key)

  def put(key: String, value: Any) {
    map.put(key, value)
  }

  /**
   * Create view that has no parent
   * @return new view of the scope
   */
  def unlinked: SScope = {
    val that = this
    new SScope(None) {
      override def get(key: String) = that.getStrict(key)

      override def getStrict(key: String) = that.getStrict(key)

      override def put(key: String, value: Any) {
        that.put(key, value)
      }
    }
  }
}

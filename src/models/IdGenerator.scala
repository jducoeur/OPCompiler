package models

trait IdGenerator {
  var _nextId = 0
  def nextId() = {
    _nextId += 1
    _nextId
  }
}
package models

case class Reign(id:Int, king:Persona, queen:Persona, start:OPDate, filename:String) {
  def end:OPDate = {
    val nextReign = id + 1
    if (Reign.allReigns.contains(nextReign))
      Reign.allReigns(nextReign).start
    else
      OPDate.Invalid
  }
  
  override def toString = {
    id.toString + ": " + king.scaName + " / " + queen.scaName + " - from " + start + " to " + end + " (" + filename + ")"
  }
}

object Reign {
  var _nextId = 0
  def nextId() = {
    _nextId += 1
    _nextId
  }
  
  implicit object ReignOrdering extends Ordering[Reign] {
    def compare(x:Reign, y:Reign) = x.id compare y.id
  }
  
  import collection.immutable.SortedMap
  var allReigns = SortedMap.empty[Int,Reign]
  
  var byFilename = Map.empty[String,Reign]
}
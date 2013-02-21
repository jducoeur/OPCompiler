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

object Reign extends IdGenerator {
  implicit val ReignOrdering = Ordering.by { reign:Reign => (reign.id) }
  
  import collection.immutable.SortedMap
  var allReigns = SortedMap.empty[Int,Reign]
  
  var byFilename = Map.empty[String,Reign]
}
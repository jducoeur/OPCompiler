package process

import models._

import scala.math.min

import scala.util.Sorting._
 
/**
 * The system that attempts to come up with good suggestions for de-duplicating the
 * database. Doesn't actually make changes, but suggests fixes in the tables, and
 * additions to names.conf.
 */
object Deduper {
  
  def editDist[A](a: Iterable[A], b: Iterable[A]) =
    ((0 to b.size).toList /: a)((prev, x) =>
      (prev zip prev.tail zip b).scanLeft(prev.head + 1) {
          case (h, ((d, v), y)) => min(min(h + 1, v + 1), d + (if (x == y) 0 else 1))
        }) last
  
  implicit object RecOrdering extends Ordering[Recognition] {
    def compare(x:Recognition, y:Recognition) = {
      val byDate = x.bestDate compare y.bestDate
      if (byDate == 0)
        x.id compare y.id
      else
        byDate
    }
  }
  
  def printCandidate(rec:Recognition):String = {
    rec.recipient.scaName + " (" + rec.as.name + ", " + rec.bestDate.shortString + ")"
  }
  
  case class WithDist(target:Recognition, candidate:Recognition) {
    val dist = editDist(target.recipient.scaName, candidate.recipient.scaName)
  }
  implicit object DistOrdering extends Ordering[WithDist] {
    def compare(x:WithDist, y:WithDist) = {
      val byDist = x.dist compare y.dist
      if (byDist == 0)
        x.candidate.recipient.scaName compare y.candidate.recipient.scaName
      else
        byDist
    }
  }
  
  import scala.annotation.tailrec
  @tailrec def doWindowing(l:List[Recognition], f:Iterable[Recognition] => Unit):Unit = {
    l match {
      case head :: rest => {
        val curDate = head.bestDate
        val candidates = rest.takeWhile(_.bestDate.matches(curDate))
        if (head.inAlpha && !head.inCourt) {
          val plausible = candidates filter { candidate =>
            candidate.inCourt && !candidate.inAlpha && candidate.award == head.award
          }
          if (plausible.length > 0) {
            val withDists = plausible.map(candidate => WithDist(head, candidate)).toArray
            quickSort(withDists)
            val good = withDists filter (_.dist < 10)
            if (good.size > 0) {
              Log.print("  It looks like " + printCandidate(head) + " might be:")
              good.foreach { pair => 
                Log.print("    " + pair.dist + " " + printCandidate(pair.candidate)) 
              }
            }
          }
        }
        
        doWindowing(rest, f)
      }
      case Nil => {}
    }
  }
  
  def dedupe = {
    Log.pushContext("Deduplicating")
    
    val recs = allRecs
    Log.print("Starting with " + recs.size + " recognitions")
    val realRecs = allRecs.filterNot(_.isCommentary).toArray
    Log.print("  Of those, " + realRecs.size + " are real")
    val incomplete = realRecs.filterNot(_.isComplete)
    Log.print("  Of those, " + incomplete.size + " are incomplete")
    
    quickSort(realRecs)
    val startList = realRecs.toList
    doWindowing(startList, (_ => Unit))
    
    Log.popContext
  }
  
  def allRecs = {
    for (
      person <- Person.allPeople.toSeq;
      persona <- person.personae;
      rec <- persona.awards
        )
      yield rec;
  }
}
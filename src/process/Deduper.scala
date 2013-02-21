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
  
  implicit object RecOrdering extends MultiOrdering[Recognition] {
    val transforms = List[CompF[_]](
      t(_.bestDate),
      t(_.id)
    )
  }
  
  def printCandidate(rec:Recognition):String = {
    rec.recipient.scaName + " (" + rec.as.name + ", " + rec.bestDate.shortString + ")"
  }
  
  case class CandidatePair(target:Recognition, candidate:Recognition) {
    val dist = editDist(target.recipient.scaName, candidate.recipient.scaName)
  }
  implicit object DistOrdering extends MultiOrdering[CandidatePair] {
    val transforms = List[CompF[_]](
      t(_.dist),
      t(_.candidate.recipient.scaName)
      )
  }
  object TargetOrdering extends MultiOrdering[CandidatePair] {
    val transforms = List[CompF[_]](
      t(_.candidate.recipient.scaName),
      t(_.dist)
    )
  }
  
  // TODO: expand this algorithm to work with any gaps. It is trying to match a record that
  // is lacking a characteristic with those that have it. It should work with any combination
  // of alpha, court and list.
  //
  // XXXX: enhance recognition.isComplete to not worry about the cases we don't expect to
  // find. In particular:
  // xx If the award is Baronial, we don't expect a Court Report.
  // xx If the award is out-Kingdom, we don't expect a Court Report.
  // xx If the award doesn't have a List that was read in, we don't expect a List record.
  // These should slice out a lot of falsely incomplete records. (Actually removed 2000+.)
  //
  // TODO: now that we are identifying candidates, build them up. Collect all the candidate
  // pairings into a multimap. For each pairing, we maintain a list of candidate joins,
  // sorted by edit distance. (This goes in both directions.)
  // Then, once we have all the candidates, we go through and merge the candidates. If we
  // have multiple independent relationships between two names, that makes a much higher-scoring
  // relationship. "Independent" is tricky to define, but is likely multiple pairings on
  // different dates.
  // Then turn each grouped pair into a proposal for names.conf. The head name should come
  // from the "best" record: Alpha if possible, Court if not. Whether it counts as a typo or
  // alternate depends on the edit distance between names -- a low edit distance means it
  // probably should count as a typo; a high edit distance means an alternate name.
  // Make sure that these proposals include all the current information: all currently-known
  // alternate names should be included in the proposed entry in the appropriate spot.
  //
  // TODO: once that is done, run it successive times. Each time, take the good-looking proposals,
  // and iterate. We should be able to refine the list pretty fast, with some public inquiries
  // about the iffy cases.
  //
  // TODO: introduce a mechanism to say that two names are *not* synonymous, so that we can
  // weed out the false positives once we know about them. This is probably at the end of the
  // names.conf entry, a "!" operator followed by these.
  
  import scala.annotation.tailrec
  @tailrec def doWindowing(
      l:List[Recognition], 
      f:(Recognition, List[Recognition]) => Seq[CandidatePair], 
      soFar:Seq[CandidatePair] = Seq.empty):Seq[CandidatePair] = 
  {
    l match {
      case head :: rest => {
        val curDate = head.bestDate
        val candidates = rest.takeWhile(_.bestDate.matches(curDate))
        val newCandidates = f(head, candidates)
        doWindowing(rest, f, soFar ++ newCandidates)
      }
      case Nil => soFar
    }
  }
  
  def checkCandidateMatches(head:Recognition, candidates:List[Recognition]):Seq[CandidatePair] = {
    var ret = Seq.empty[CandidatePair]
    if (head.inAlpha && !head.inCourt) {
      val plausible = candidates filter { candidate =>
        candidate.inCourt && !candidate.inAlpha && candidate.award == head.award
      }
      if (plausible.length > 0) {
        val withDists = plausible.map(candidate => CandidatePair(head, candidate)).toArray
        quickSort(withDists)
        val good = withDists filter (_.dist < 10)
        if (good.size > 0) {
          Log.print("  It looks like " + printCandidate(head) + " might be:")
          good.foreach { pair => 
            Log.print("    " + pair.dist + " " + printCandidate(pair.candidate)) 
          }
          ret = good
        }
      }
    }
    ret
  }
  
  def dedupe = {
    Log.pushContext("Deduplicating")
    
    val recs = allRecs
    Log.print("Starting with " + recs.size + " recognitions: ")
    val realRecs = allRecs.filterNot(_.isCommentary).toArray
    Log.print("  Of those, " + realRecs.size + " are real")
    val incomplete = realRecs.filterNot(_.isComplete)
    Log.print("  Of those, " + incomplete.size + " are incomplete")
    
    quickSort(incomplete)
    val startList = incomplete.toList
    val allCandidates = doWindowing(startList, checkCandidateMatches)
    
    
    
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
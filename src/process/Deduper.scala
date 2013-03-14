package process

import models._

import collection.SortedSet

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
  
  implicit val RecOrdering = Ordering.by{ rec:Recognition =>
    (rec.bestDate, rec.id)
  }
  
  def printCandidate(rec:Recognition):String = {
    rec.recipient.scaName + " (" + rec.as.name + ", " + rec.bestDate.shortString + ")"
  }
  
  case class CandidatePair(target:Recognition, candidate:Recognition) {
    val dist = {
      // Find the closest distance between *any* of the target's names and the candidate:
      (1000 /: target.recipient.person.personae) { (best, persona) =>
        math.min(best, editDist(persona.scaName, candidate.recipient.scaName))
      }
      //editDist(target.recipient.scaName, candidate.recipient.scaName)
    }
  }
  implicit val DistOrdering = Ordering.by { pair:CandidatePair =>
    (pair.dist, pair.candidate.recipient.scaName)
  }
  val TargetOrdering = Ordering.by { pair:CandidatePair =>
    (pair.candidate.recipient.scaName, pair.dist)
  }
  
  case class MergeOptions(target:Persona, candidatesIn:Seq[CandidatePair]) {
    val candidates = candidatesIn.distinct
    
    lazy val bestMatch:Seq[CandidatePair] = {
      val groups = candidates.groupBy(_.candidate.recipient)
      val byMostMatched = (Ordering.by { info:(Int, Int, Seq[CandidatePair]) => info._1 }).reverse
      val byDist = Ordering.by { info:(Int, Int, Seq[CandidatePair]) => info._2 }
      val byAll = Ordering.by{ info:(Int, Int, Seq[CandidatePair]) => (info, info) }(Ordering.Tuple2(byMostMatched, byDist))
      val withSizes = groups.values.map( pairs => (pairs.length, pairs(0).dist, pairs) ).toArray
      quickSort(withSizes)(byAll)
      withSizes(0)._3
    }
    
    lazy val bestPersona = bestMatch(0).candidate
    
    lazy val num = bestMatch.length
    
    lazy val dist = bestMatch(0).dist
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
  
  def excluded(head:Recognition, candidate:Recognition) = {
    head.recipient.person.falsePositives.contains(candidate.recipient.scaName)
  }
  
  def checkCandidateType(head:Recognition, candidates:List[Recognition],
      in:Recognition => Boolean, against:Recognition => Boolean, other:Recognition => Boolean,
      andAlso:Recognition => Boolean = (_ => true)):Seq[CandidatePair] = {
    var ret = Seq.empty[CandidatePair]
    if (in(head) && !against(head) && andAlso(head)) {
      val plausible = candidates filter { candidate =>
        against(candidate) && !in(candidate) && candidate.award == head.award &&
        (if (other(head)) !other(candidate) else true)
      }
      if (plausible.length > 0) {
        ret = plausible.map(candidate => CandidatePair(head, candidate))
      }
    }
    ret    
  }
  
  def checkCandidateMatches(head:Recognition, candidatesIn:List[Recognition]):Seq[CandidatePair] = {
    val candidates = candidatesIn.filterNot(excluded(head, _))
    
    //Log.print("\nChecking against " + head.toString + "; candidates:")
    //candidates.foreach(rec => Log.print("    " + rec.toString))
    
    // If the head is from somewhere else, we don't expect to find a match:
    val result = checkCandidateType(head, candidates, (_.inAlpha), (_.inCourt), (_.inList), (_.where.isEmpty)) ++
    	checkCandidateType(head, candidates, (_.inAlpha), (_.inList), (_.inCourt))
    	
    //Log.print("  Filtered:")
    //result.foreach(pair => Log.print("    " + pair.candidate.toString + " (dist: " + pair.dist + ")"))
    result
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
    
    Log.print("Best Candidates:")
    val byTarget = allCandidates.groupBy(_.target.recipient)
    val merges = byTarget.collect({ case pair => MergeOptions(pair._1, pair._2) }).toArray
    val byMostMatched = (Ordering.by { merge:MergeOptions => merge.num }).reverse
    val byTargetName = Ordering.by { merge:MergeOptions => merge.target.scaName }
    val byDist = Ordering.by { merge:MergeOptions => merge.dist }
    val byAll = Ordering.by{ merge:MergeOptions => (merge, merge, merge) }(Ordering.Tuple3(byMostMatched, byDist, byTargetName))
    Log.print("Sorting " + merges.length + " merge candidates")
    quickSort(merges)(byAll)
    merges.foreach { merge =>
      val best = merge.bestMatch
//      Log.print("  " + merge.target.scaName + " (" + best.length + ", " +
//          "dist: " + best.head.dist + ":" +
//          (if (best.head.dist < 5) "typo" else "alternate") +
//          "):")
//      best.foreach { candidate => Log.print("    " + printCandidate(candidate.candidate)) }
      
      def isStrong = {
        merge.num > 1 || merge.dist < 18
      }
      
      val person = merge.target.person
      if (person.merges.isEmpty && isStrong)
        person.merges = Some(merge)
        
      def newlineIf(it:Seq[_]) = {
        if (it.length > 0)
          "\n"
        else
          ""
      }
      
      val matchStrs = best map { pair:CandidatePair =>
        "#   <- " + pair.candidate.toString + " (person: " + pair.candidate.recipient.person.hashCode() + ", persona: " + pair.candidate.recipient.hashCode() + ")" 
      }
      val targetPersonaStrs = best(0).target.recipient.person.personae map { persona =>
        "#           " + persona.scaName + ": " + persona.hashCode()
      }
      val falseStrs = best(0).target.recipient.person.falsePositives map { notName =>
        "#           NOT " + notName
      }
      //val allStrs = merge.candidates map { candidate =>
      //  "#         ? " + candidate.candidate.toString
      //}
      Log.print("#      " + best(0).target.toString + 
          " (person: " + best(0).target.recipient.person.hashCode() + "; " + merge.num + " matches, dist: " + merge.dist + ")\n" +
          //allStrs.mkString("\n") + newlineIf(allStrs) +
          targetPersonaStrs.mkString("\n") + newlineIf(targetPersonaStrs) +
          falseStrs.mkString("\n") + newlineIf(falseStrs) +
          matchStrs.mkString("\n")
          )
      Log.print(formatPerson(person, true))
    }
    
    writeNameConfig()
    
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
  
  import java.io._
  
  def formatPerson(person:Person, allowMerges:Boolean) = {
    var ret = ""
    // Note that we intentionally do not rely on the isMain flag any more! That can get easily
    // corrupted by the process.
    val alphaPersonae = person.personae.filter(_.hasAlphas)
    val mainPersona = 
      (if (alphaPersonae.length > 1) {
        ret += "# Multiple Alpha entries! " + alphaPersonae.map(_.scaName).mkString(", ") + "\n"
        alphaPersonae(0)
      } else if (alphaPersonae.length == 1)
        alphaPersonae(0)
      else
        person.mainPersona)
    val nonMain = person.personae.filterNot(_ == mainPersona)
    val (typos,alternates) = nonMain.partition(_.isTypo)
    val altsPlusMerge =
      (if (allowMerges && person.merges.isDefined)
         alternates :+ person.merges.get.bestPersona.recipient
       else
         alternates)
    val falsePositives =
      (if (!allowMerges && person.merges.isDefined)
         person.falsePositives :+ person.merges.get.bestPersona.recipient.scaName
        else
         person.falsePositives)
    ret += mainPersona.scaName + " == " +
      (if (altsPlusMerge.length > 0) altsPlusMerge.map(_.scaName).mkString("; ") else "") +
      (if (typos.length > 0) " || " + typos.map(_.scaName).mkString("; ") else "") +
      (if (falsePositives.length > 0) " ! " + falsePositives.mkString("; ") else "")
    ret
  }
  
  def writeNameConfig() = {
    val writer = new PrintWriter(new File("names.conf.proposed"), "UTF-8")
    def print(msg:String) = writer.print(msg)
    def println(msg:String) = writer.println(msg)

    Person.allPeople.foreach { person => 
      if (person.merges.isDefined) {
        val matches = person.merges.get.bestMatch
        val matchStrs = matches map { pair:CandidatePair =>
          "#   <- " + pair.candidate.toString
        }
        println("#      " + matches(0).target.toString + 
            " (" + person.merges.get.num + " matches, dist: " + person.merges.get.dist + ")\n" +
            matchStrs.mkString("\n")
            )
        println("+ " + formatPerson(person, true))
        println("- " + formatPerson(person, false))
      } else if (person.hasAlternates || person.hasFalsePositives) {
        println(formatPerson(person, false))
      }
    }
    
    writer.flush
    writer.close
  }
}
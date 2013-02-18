package models

// TODO: late in the process, once everything is compiling, scan the records. When we
// find Recognitions that exist in Alpha but not in Court, try to match them up by date
// with the same award in Court but not Alpha, and produce a report making guesses of
// apparent correlations. Ideally, making a sorted-by-date list of unmatched Recognitions;
// that should make the cleanup easier.

// The heart and soul of the OP: the record of who got what when. Note that
// this object gets recorded into a bunch of separate tables, indexed in the
// various ways. The index is the item number of this business within the Court.
// The separate date is intended for cases where we don't know the actual Court.
case class Recognition(recipient:Persona, award:Award, as:AwardName, 
    when:Option[Court] = None, index:Option[Int] = None, date:Option[OPDate] = None,
    inAlpha:Boolean = false, inList:Boolean = false, comment:Option[String] = None,
    where:Option[Branch] = None) extends Ordered[Recognition] {
  def bestDate:OPDate = {
    date match {
      case Some(d) => d
      case None => {
        when match {
          case Some(court) => court.date
          case None => OPDate.Invalid
        }
      }
    }
  } 
  
  // Note that two recognitions are considered equivalent if they were given to the same
  // *person*, but possibly recorded under different names in different places
  def matches(other:Recognition):Boolean = {
    (recipient.person == other.recipient.person) &&
    (award == other.award) &&
    (bestDate.matches(other.bestDate))
  }
  
  def inCourt:Boolean = when.isDefined
  
  // True iff this item of business is *just* a comment
  def isCommentary:Boolean = award.commentary
  
  // This should only be called if this.matches(other) returned true; it combines the known
  // information between them
  def merge(other:Recognition):Recognition = {
    Recognition(recipient, award, as,
      if (inCourt) when else other.when,
      if (inCourt) index else other.index,
      if (date.isDefined) date else other.date,
      inAlpha || other.inAlpha,
      inList || other.inList,
      if (comment.isDefined) comment else other.comment
    )
  }
  
  // For now, we'll just sort by date; there's no obvious reason to need to get fancy here:
  def compare(other:Recognition) = bestDate.compare(other.bestDate)
  
  def flagStr:String = {
    (if (inAlpha) "A" else " ") +
    (if (inCourt) "C" else " ") +
    (if (inList) "L" else " ")
  }
  override def toString = "  " + bestDate.shortString + " " + flagStr + " " +
		  (if (isCommentary) "(" else "") +
		  as.name +
		  (if (!award.branch.isDefault) " (" + award.branch.name + ")" else "") +
		  " -- " +
		  recipient.scaName +
		  (if (where.isDefined) " [from " + where.get.name + "]" else "") +
		  (if (comment.isDefined) " [" + comment.get + "]" else "") +
		  (if (isCommentary) ")" else "")
}

// Represents a single Court, held by somebody, where awards were given.
class Court(val title:String, val date:OPDate, reign:Reign) {
  // The actual business of the Court. This is a var mainly because of the
  // relationship between Recognition and Court -- we need to create the
  // Court, then build the Recognitions, then put them into it.
  var business:Seq[Recognition] = Seq[Recognition]()
  
  val id = Court.nextId()
  
  Court.allCourts += this
  
  def isChampionship = title.contains("Champion")
  val champRegex = """^.* (\S*) Champion.*$""".r
  def champName = {
    title match {
      case champRegex(name) => name
      case _ => ""
    }
  }
  
  override def toString = {
    "== " + title + " == (" + date + ", " + reign.king.scaName + " and " + reign.queen.scaName + ")\n" +
    business.foldLeft("")((str, item) => str + item + "\n")
  }
}

object Court extends IdGenerator {
  implicit object CourtDateOrdering extends Ordering[Court] {
    def compare(x:Court, y:Court) = x.date compare y.date
  }
  
  import collection.immutable.SortedSet
  var allCourts = SortedSet.empty[Court]
}
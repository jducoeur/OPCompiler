package models

import scala.util.matching._

import process.Log

object Month extends Enumeration {
  type Month = Value
  
  val Unknown, January, February, March, April, May, June, July, August, September, October, November, December = Value
  
  val byName = Map(
    ("January" -> January),
    ("Jan" -> January),
    ("February" -> February),
    ("Feb" -> February),
    ("March" -> March),
    ("Mar" -> March),
    ("April" -> April),
    ("Apr" -> April),
    ("May" -> May),
    ("June" -> June),
    ("Jun" -> June),
    ("July" -> July),
    ("Jul" -> July),
    ("August" -> August),
    ("Aug" -> August),
    ("September" -> September),
    ("Sep" -> September),
    ("Sept" -> September),
    ("October" -> October),
    ("Oct" -> October),
    ("November" -> November),
    ("Nov" -> November),
    ("December" -> December),
    ("Dec" -> December),
    ("Unk" -> Unknown)
  )
}

import Month._

// Yes, I went and wrote my own Date class. The Java version in infamously crappy; Joda is wildly
// overkill for my purposes; and *nothing* has anywhere near the ability to parse the sort of
// terrible data I'm working with. So it's easier to just reinvent the wheel.
//
// Note that this class is very simple; it's immutable; and it provides no ability to do interesting
// date math. I don't care about any of that: mostly, I care about the ability to parse, and to
// output into a standard format.
trait OPDate extends Ordered[OPDate] {
  val dateMatch:Option[Regex.Match]
  def isValid = dateMatch.nonEmpty
  protected def ifMatched[A](f: Regex.Match => A, default:A) = {
	dateMatch match {
	  case Some(m) => f(m)
	  case None => default
	}	  
  }
  protected def getField[A](fieldName:String, f: String => A, default:A) = {
	ifMatched((m => f(m.group(fieldName))), default)
  }  
  
  def month:Month
  def day:Int
  def year:Int
  
  override def equals(other:Any) = other match {
    case that:OPDate => this.isValid == that.isValid && this.month == that.month && this.day == that.day && this.year == that.year
    case _ => false
  }
  // Yes, this is a crappy hashcode alorithm. But it should be unique, and it's not worth getting fancier.
  override def hashCode = {
    if (!isValid)
      // All invalid dates are considered the same
      0
    else
      day + (100 * month.id) + (10000 * year)  
  }
  
  def compare(other:OPDate) = hashCode.compare(other.hashCode)
  
  // Note the crucial difference between this and equals: this does not guarantee a perfect match.
  // Rather, matches returns true within the known precision of both dates.
  def matches(other:OPDate):Boolean = {
    if (!this.isValid || !other.isValid)
      true
    else {
      def closeEnough(v1:Int, v2:Int, unknown:Int):Boolean = v1 == v2 || v1 == unknown || v2 == unknown
      
      closeEnough(month.id, other.month.id, Month.Unknown.id) &&
      closeEnough(day, other.day, OPDate.Unknown) &&
      closeEnough(year, other.year, OPDate.Unknown)
    }
  }
  
  override def toString = {
	if (isValid)
	  (if (month == Month.Unknown) "?????" else month.toString) + " " + 
	  (if (day == OPDate.Unknown) "??" else day.toString) + ", " + 
	  year.toString
	else
	  "Unknown Date"
  }
 
  def padDateNum(num:Int):String = {
    if (num > 9) num.toString
    else "0" + num.toString
  }
  def shortString = {
	if (isValid)
	  (if (month == Month.Unknown) "??" else padDateNum(month.id)) + "/" + 
	  (if (day == OPDate.Unknown) "??" else padDateNum(day)) + "/" + 
	  (if (year == OPDate.Unknown) "????" else year.toString)
	else
	  "Unknown Date"
  }
  
  def sqlString = {
    (if (year == OPDate.Unknown) "1964" else year.toString) + "-" +
    (if (month == OPDate.Unknown) "01" else padDateNum(month.id)) + "-" +
    (if (day == OPDate.Unknown) "01" else padDateNum(day))
  }
}

abstract class OPDateFromString(val fromStr:String) extends OPDate {
  // This expects each date part to be either two digits, or "??"
  def optionalPart(digits:String) = {
    if (digits == "??" || digits == "????" || digits == "xx" || digits == "xxxx" || digits == "19??")
      OPDate.Unknown
    else
      try {
        // Allow a one-digit month, since that is how it works in Lucan5
        if (digits(0) == ' ')
          digits(1).toString.toInt
        else
          digits.toInt
      } catch {
        case e:Exception => Log.error("Bad date part: " + digits); OPDate.Unknown
      }
  }

  def prefix = ifMatched(_.before, fromStr)
  def suffix = ifMatched(_.after, fromStr)  
}

// This version parses the modern court-report format, eg, "February 22, 1998"
class OPCourtDate(f:String) extends OPDateFromString(f) {
	val dateMatch:Option[Regex.Match] = {
	  OPDate.dateRegex.findFirstMatchIn(fromStr) orElse OPDate.abbrevRegex.findFirstMatchIn(fromStr)
	}
	
	def month = getField("month", Month.byName(_), Month.Unknown)
	def day = getField("day", optionalPart(_), OPDate.Unknown)
	def year = getField("year", _.toInt, OPDate.Unknown)
}

// This version parses the short version in the alpha list, eg, "2/22/98"
class OPShortDate(f:String) extends OPDateFromString(f) {
  val dateMatch:Option[Regex.Match] = { 
    OPDate.shortRegex.findFirstMatchIn(fromStr) orElse OPDate.shorterRegex.findFirstMatchIn(fromStr)
  }      

  def monthPart(digits:String) = {
    val raw = optionalPart(digits)
    if (raw == OPDate.Unknown)
      Month.Unknown
    else
      Month(raw)
  }
  
  def padYear(y:Int):Int = {
    if (y < 100) {
      if (y < 50)
        2000 + y
      else
        1900 + y
    } else
      y
  }

  def day = getField("day", optionalPart(_), OPDate.Unknown)
  def year = padYear(getField("year", optionalPart(_), OPDate.Unknown))
  def month = getField("month", monthPart(_), Month.Unknown)
}

class InvalidOPDate extends OPDate {
  val dateMatch = None
  
  def day = OPDate.Unknown
  def year = OPDate.Unknown
  def month = Month.Unknown
}

object OPDate {
	val dateRegex:Regex = new Regex("""(January|February|March|April|May|June|July|August|September|October|November|December) ([\d\?][\d\?]?)(th|st)?,? ?(\d\d\d\d)""", "month", "day", "suffix", "year")
	val abbrevRegex:Regex = new Regex("""(Unk|Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sept|Sep|Oct|Nov|Dec)\.? ([\d\?][\d\?]?)(th|st)?,? ?(\d\d\d\d)""", "month", "day", "suffix", "year")
	val shortRegex:Regex = new Regex("""(..?)/(..?)/(..\S\S)""", "month", "day", "year")
	val shorterRegex:Regex = new Regex("""(..?)/(..?)/(..)""", "month", "day", "year")
	val Unknown = -1
	val Invalid = new InvalidOPDate
	
	implicit val OPDateOrdering = Ordering.by { date:OPDate => (date.year, date.month, date.day) }
}
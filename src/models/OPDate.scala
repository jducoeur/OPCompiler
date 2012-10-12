package models

import scala.util.matching._

import process.Log

object Month extends Enumeration {
  type Month = Value
  
  val Unknown, January, February, March, April, May, June, July, August, September, October, November, December = Value
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
	  year.toString
	else
	  "Unknown Date"
  }
}

// This version parses the modern court-report format, eg, "February 22, 1998"
class OPCourtDate(val fromStr:String) extends OPDate {
	val dateMatch:Option[Regex.Match] = OPDate.dateRegex.findFirstMatchIn(fromStr)
	
	def month = getField("month", Month.withName(_), Month.Unknown)
	def day = getField("day", _.toInt, OPDate.Unknown)
	def year = getField("year", _.toInt, OPDate.Unknown)
	
	def prefix = ifMatched(_.before, fromStr)
	def suffix = ifMatched(_.after, fromStr)
}

// This version parses the short version in the alpha list, eg, "2/22/98"
class OPShortDate(val fromStr:String) extends OPDate {
  val dateMatch:Option[Regex.Match] = OPDate.shortRegex.findFirstMatchIn(fromStr)
  
  // This expects each date part to be either two digits, or "??"
  def optionalPart(digits:String) = {
    if (digits == "??")
      OPDate.Unknown
    else
      try {
        digits.toInt
      } catch {
        case e:Exception => Log.error("Bad date part: " + digits); OPDate.Unknown
      }
  }

  def monthPart(digits:String) = {
    val raw = optionalPart(digits)
    if (raw == OPDate.Unknown)
      Month.Unknown
    else
      Month(raw)
  }

  def day = getField("day", optionalPart(_), OPDate.Unknown)
  def year = getField("year", optionalPart(_), OPDate.Unknown)
  def month = getField("month", monthPart(_), Month.Unknown)
}

class InvalidOPDate extends OPDate {
  val dateMatch = None
  
  def day = OPDate.Unknown
  def year = OPDate.Unknown
  def month = Month.Unknown
}

object OPDate {
	val dateRegex:Regex = new Regex("""(January|February|March|April|May|June|July|August|September|October|November|December) (\d\d?)(th|st)?, (\d\d\d\d)""", "month", "day", "suffix", "year")
	val shortRegex:Regex = new Regex("""(..)/(..)/(....)""", "month", "day", "year")
	val Unknown = -1
	val Invalid = new InvalidOPDate
}
package parsers

import scala.util.matching._
import scala.xml._
import models._
import process._

trait CourtReportParser extends OPFileParser {

}

class CurrentCourtReportParser extends CourtReportParser {
  // Why a Seq? Because a single row often turns out to describe several awards, joined with
  // "&" or suchlike
  def processRowGuts(court:Court, index:Int, awardName:String, personaName:String):Seq[Recognition] = {
    val persona = getPersona(personaName, true)
    val (parsedAwardName, comment) = ParseUtils.extractComment(awardName)
    
    // This weirdness is to deal with the very common case where we have, eg, the
    // "King and Queen's Archery Championship", and the award is just listed as
    // "King's Champion". We have to derive the actual award name from context.
    val checkedAwardName = if (parsedAwardName.contains("Champion")) {
      if (Award.contains(parsedAwardName))
        // Okay, it's fully spelled out
        parsedAwardName
      else if (court.isChampionship) {
        // We couldn't find the award, so try inserting the championship name
        val subbedName = parsedAwardName.replace("Champion", court.champName + " Champion")
        if (Award.contains(subbedName))
          subbedName
        else
          // Give up, and assume it's Rattan:
          parsedAwardName.replace("Champion", "Rattan Champion")
      } else
        // Give up, and assume it's Rattan:
        parsedAwardName.replace("Champion", "Rattan Champion")
    } else
      parsedAwardName
    
    val awardAses = Award.find(checkedAwardName)
    def makeRec(as:AwardAs) = {
      val recognition = Recognition(persona, as.award, as.name, Some(court), Some(index), comment=comment)
      persona.addAward(recognition)
      recognition
    }
    val recs = for (award <- awardAses) yield makeRec(award)
    recs    
  }
  
  def processRow(itemRow:Node, court:Court, index:Int):Seq[Recognition] = {
    val cells = itemRow \ "td" toList
    val awardCell :: nameCell :: dummy = cells
    val awardName = StringUtils.scrub(awardCell.text)
    val personaName = StringUtils.scrub(nameCell.text)
    
    processRowGuts(court, index, awardName, personaName)
  }
  
  import scala.util.matching.Regex;
  val stripDash:Regex = """,?\s*-?\s*$""".r
  
  def buildBusinessFromTable(businessNodes:NodeSeq)(court:Court):Seq[Recognition] = {
    val start:Seq[Recognition] = Seq.empty
    businessNodes.foldLeft(start)((vec, businessRow) => vec ++: processRow(businessRow, court, vec.size))
  }
  
  def processGuts(caption:String, filename:String, builder:(Court => Seq[Recognition])) = {
    // ... remove basic junk...
    val scrubbedCaption = process.StringUtils.scrub(caption)
    Log.pushContext(scrubbedCaption)
    // ... separate it into date and event name...
    var courtDate:OPDateFromString = new OPCourtDate(scrubbedCaption)
    if (!courtDate.isValid)
      courtDate = new OPShortDate(scrubbedCaption)
    if (!courtDate.isValid)
      Log.error("Can't find a date in " + scrubbedCaption)
    val courtTitleMessy = courtDate.prefix.toString
    // ... strip out the separators between event name and date...
    val tryStrip:Option[Regex.Match] = stripDash.findFirstMatchIn(courtTitleMessy)
    val courtTitle =
      if (tryStrip.nonEmpty)
        tryStrip.get.before.toString
      else
        courtTitleMessy
    // ... and now we're finally ready to start building the court:
    val court = new Court(courtTitle, courtDate, Reign.byFilename(filename))
    var index = 0;
    // The general assumption is that the caller will use a closure to capture the information
    // about the business nodes:
    court.business = builder(court)
    Log.print(court.toString)
    Log.popContext    
  }
  
  def processTable(tableNode:Node, filename:String) = {
    val rows = tableNode \\ "tr"
    // Split the rows into the ones with a colspan of 2 -- the caption -- and the rest, which
    // business
    def isCaption(row:Node) = {
      val colspanAttr = (row \ "td").head \ "@colspan"
      if (colspanAttr.length > 0)
        colspanAttr.head.text == "2"
      else
        false
    }
    val (captions, businessNodes) = rows partition isCaption
    // First, get the raw caption...
    var caption = 
      if (captions.length > 0) 
        captions.head.text.trim.replace("\n", " - ") 
      else {
        val captions = (tableNode \\ "caption")
        if (captions.length > 0) captions.head.text.trim
        else {
          val captionDivs = (tableNode \\ "caption" \\ "div")
          if (captionDivs.length > 0) captionDivs.head.text.trim else ""
        }
      }
    processGuts(caption, filename, buildBusinessFromTable(businessNodes))
  }
  
  def processFile(fileNode:Elem, name:String) = {
    // Each of these tables represents a single Court Report
    val reportTables = fileNode \\ "table"
    reportTables.foreach(table => processTable(table, name))
  }
}
object CurrentCourtReportParser extends CurrentCourtReportParser {}


/**
 * This deals with the "old" style of Court Report, originally maintained in Word. These are
 * similar to the current style, but the captions are simply paragraphs preceding the tables,
 * which makes parsing a tad trickier.
 */
trait WordCourtReportParser extends CurrentCourtReportParser {
  override def processFile(fileNode:Elem, name:String) = {
    val bodyNode = fileNode \\ "body" head
    val wordSection = bodyNode \\ "div" head
    val elements = wordSection.child filter (_.isInstanceOf[Elem])
    // This pairs up each node with the one before it
    val pairs = elements.tail.zip(elements)
    val tablePairs = pairs filter (_._1.label == "table")
    
    tablePairs.foreach(pair => processGuts(pair._2.text.trim, name, buildBusinessFromTable(pair._1 \\ "tr")))
  }
}
object WordCourtReportParser extends WordCourtReportParser {}


/**
 * This deals with the really antique files -- Siegfried I and earlier. These are in a highly
 * simplified HTML format that uses lines instead of tables, so the parsing is a bit different.
 * Each paragraph is a court; each line is an award. The result is more like the AlphaParser.
 */
trait AncientCourtReportParser extends CurrentCourtReportParser {
  
  // Note that this allows either en or em dashes, because both get used in at least
  // Siegfried I:
  val awardLineRegex = new Regex("""^(.*)\s[-–]\s(.*)$""", "award", "recipient")
  
  def buildBusinessFromLines(lines:Seq[Node])(court:Court):Seq[Recognition] = {
    val start:Seq[Recognition] = Seq.empty
    lines.foldLeft(start) { (vec, line) => 
      val lineMatch = awardLineRegex.findFirstMatchIn(StringUtils.scrub(line.text))
      lineMatch match {
        case Some(m) => vec ++: processRowGuts(court, vec.size, m.group("award"), m.group("recipient"))
        case None => vec
      }
    }
  }
  
  def processParagraph(paraNode:Node, name:String) = {
    val lines = paraNode.child filter (_.isInstanceOf[Text])
    val courtName = lines.head.text.trim
    processGuts(courtName, name, buildBusinessFromLines(lines.tail))
  }
  
  override def processFile(fileNode:Elem, name:String) = {
    val bodyNode = fileNode \\ "body" head
    val tables = bodyNode \\ "p"
    tables foreach (table => processParagraph(table, name))
  }
}
object AncientCourtReportParser extends AncientCourtReportParser {}


/**
 * This parser handles the table format that was common prior to Lucan V.
 */
trait OneTableCourtReportParser extends CourtReportParser with BigTableParser {
  case class RowInfo(date:OPDate, award:Seq[AwardAs], recipient:Persona, eventName:String)
  type TypedRow = RowInfo
  
  def transformRow(rowVals:List[String]):TypedRow = {
    val dateStr :: awardStr :: recipientStr :: courtStr :: excess = rowVals
    RowInfo(new OPShortDate(dateStr), Award.find(awardStr), Persona.find(recipientStr), courtStr)
  }
  
  // TBD: is this correct? Do we need to also match on court name?
  def groupMatch(first:TypedRow, second:TypedRow):Boolean = first.date == second.date
  
  def makeRecognition(row:TypedRow, court:Court, index:Int):Recognition = {
    // Current assumption: each node only contains one award. Is that right?
    val as = row.award.head
    val recognition = Recognition(row.recipient, as.award, as.name, Some(court), Some(index))
    row.recipient.addAward(recognition)
    recognition
  }
  
  def handleGroup(group:RowGroup, name:String) = {
    val firstRow = group(0)
    val court = new Court(firstRow.eventName, firstRow.date, Reign.byFilename(name))
    val (nFound, recs) = ((0, Seq.empty[Recognition]) /: group) { (state, row) =>
      val (index, seq) = state
      (index + 1, seq :+ makeRecognition(row, court, index)) 
    }
    court.business = recs
    Log.pushContext(court.title)
    Log.print(court.toString)
    Log.popContext
  }
}
object OneTableCourtReportParser extends OneTableCourtReportParser {}

object ThreeColumnCourtReportParser extends OneTableCourtReportParser {
  override def transformRow(rowVals:List[String]):TypedRow = {
    val dateStr :: awardStr :: recipientStr :: excess = rowVals
    RowInfo(new OPShortDate(dateStr), Award.find(awardStr), Persona.find(recipientStr), "Court Name Not Recorded")
  }  
}
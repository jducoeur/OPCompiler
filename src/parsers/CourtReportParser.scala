package parsers

import scala.xml._
import models._
import process._

trait CourtReportParser extends OPFileParser {

}

class CurrentCourtReportParser extends CourtReportParser {
  // Why a Seq? Because a single row often turns out to describe several awards, joined with
  // "&" or suchlike
  def processRow(itemRow:Node, court:Court, index:Int):Seq[Recognition] = {
    val cells = itemRow \ "td" toList
    val awardCell :: nameCell :: dummy = cells
    val awardName = awardCell.text.trim
    val personaName = nameCell.text.trim
    val persona = Persona.find(personaName)
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
        parsedAwardName.replace("Champion", court.champName + " Champion")
      } else
        parsedAwardName
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
  
  import scala.util.matching.Regex;
  val stripDash:Regex = """,?\s*-?\s*$""".r
  
  def processTable(tableNode:Node) = {
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
    val court = new Court(courtTitle, courtDate)
    var index = 0;
    val start:Seq[Recognition] = Seq.empty
    court.business = businessNodes.foldLeft(start)((vec, businessRow) => vec ++: processRow(businessRow, court, vec.size))
    Log.print(court.toString)
    Log.popContext
    court
  }
  
  def processFile(fileNode:Elem, name:String) = {
    // Each of these tables represents a single Court Report
    val reportTables = fileNode \\ "table"
    reportTables.foreach(processTable)
  }
}
object CurrentCourtReportParser extends CurrentCourtReportParser {}
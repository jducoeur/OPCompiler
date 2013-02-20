package parsers

import scala.xml._

import models.{Award, AwardAs, OPShortDate, Persona, Recognition}
import process.{Log, StringUtils}

class AwardListingParser extends OPFileParser {
  
  val awardLineRegex = """^§?\*?(..?/../..\S?\S?) (.*)$""".r
  def processLine(line:String, award:AwardAs) = {
    try {
      val awardLineRegex(dateStr, content) = line
      val date = new OPShortDate(dateStr)
      val (name, rawComment) = ParseUtils.extractComment(content)
      val branchOpt = ParseUtils.branchFromComment(rawComment)
      val comment = if (branchOpt.isDefined) None else rawComment
      val persona = Persona.find(name)
      val rec = Recognition(persona, award.award, award.name, date = Some(date), 
          inList=true, comment=comment, where=branchOpt)
      persona.addAward(rec)
      Log.info(rec)
    } catch {
      case _:MatchError => Log.info(line)
    }
  }
  
  def processNodes(nodes:Seq[Node], award:AwardAs):Unit = {
    for (node <- nodes) node match {
      case elem:Elem => {
        processNodes(elem.child, award)
      }
      case text:Text => {
    	val line = text.text
        val scrubbed = StringUtils.scrub(line)
        if (scrubbed.length > 0)
          processLine(scrubbed, award)
      }
      case _:Node => {
        Log.info("Other node: " + node)
      }
    }
  }

  def processFile(fileNode: Elem, name:String) = {  
    val award = Award.findOne(name)
    val body = fileNode \\ "body"
    processNodes(body.head.child, award)
    Award.awardsWithListings += award.award
  }
  
}

object AwardListingParser extends AwardListingParser {}
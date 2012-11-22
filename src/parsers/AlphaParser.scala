package parsers

import scala.util.matching._
import scala.xml._

import models.{Award, AwardAs, Branch, OPShortDate, Persona, Recognition}
import process.{Log, StringUtils}

// The parser for the alpha listing is necessarily a state machine, because the files
// have no real structure. It's a flat pile of lines, where names are distinguished by
// the fact that they are bold-faced. There is a lot of internal junk that isn't visible
// from looking at them; we mostly gloss over that.
class AlphaParser extends OPFileParser {

  // **********************
  //
  // This section deals with the messy problem that name lines often contain
  // references to other entries, in fairly irregular syntax
  def chopAsterisk(name:String) = {
    if (name.charAt(0) == '*')
      name.substring(1)
    else
      name
  }
  case class OneName(name:String, isCurrent:Boolean)
  case class NameBreakdown(curName:String, others:Seq[OneName])
  val separators = new Regex("""^(.*?),? \(?(see|was|formerly):? (.*?)\)?$""", "curName", "relation", "rest")
  
  def parseRest(sep:String, rest:String):Seq[OneName] = {
    val isCurrent = sep match {
      case "see" => true
      case "was" => false
      case "formerly" => false
      case _ => throw new Exception("Somehow got an unknown name separator!")
    }
    
    val m = separators.findFirstMatchIn(rest)
    m match {
      case Some(foundMatch) => OneName(chopAsterisk(m.get.group("curName")), isCurrent) +: parseRest(m.get.group("relation"), m.get.group("rest"))
      case None => Seq(OneName(chopAsterisk(rest), isCurrent))
    }
  }
  
  // The entry point: given the raw line of text, check whether it contains
  // any references. Returns a NameBreakdown, which separates this name from
  // any others it may be referencing, and says whether it indicates that this
  // name is an old name.
  def checkReferences(rawName:String):NameBreakdown = {
    val m = separators.findFirstMatchIn(rawName)
    m match {
      case Some(foundMatch) => NameBreakdown(m.get.group("curName"), parseRest(m.get.group("relation"), m.get.group("rest")))
      case None => NameBreakdown(rawName, Seq.empty)
    }
  }
  
  // ***********************
  
  var currentPersona:Option[Persona] = None
  
  def startPerson(scrubbed:String) = {
    if (currentPersona != null) {
      Log.popContext
      currentPersona = null
    }
    val deceased = (scrubbed.charAt(0).toInt == 167)
    var name = if (deceased) scrubbed.substring(1) else scrubbed
    val reg = (name.charAt(0) == '*')
    name = if (reg) name.substring(1) else name
    val breakdown = checkReferences(name)
    name = breakdown.curName.trim
    // The current name is current iff none of the others are
    val currentName = breakdown.others.find(_.isCurrent)
    val isCurrent = currentName.isEmpty
    val others = breakdown.others map (_.name)
    // Get a person that has the correct "main" name, then get the current persona from it:
    val persona =
      (if (isCurrent)
        Persona.find(name, others)
      else
        Persona.find(currentName.get.name, name +: others)).person.getPersona(name)  
    val person = persona.person
    if (reg)
      persona.registeredName = true
    if (deceased)
      persona.person.deceased = true
    currentPersona = Some(persona)
    Log.pushContext(persona.scaName)
  }
  
  var nameBuffer:String = ""
  def recordNamePart(part:String) = {
    nameBuffer += part
  }
  def emitName = {
    nameBuffer = StringUtils.scrub(nameBuffer)
    if (nameBuffer.length > 0) {
      startPerson(nameBuffer)
    }
    nameBuffer = ""
  }
  
  val awardLineRegex:Regex = new Regex("""^(../../....)\s(.*)$""", "date", "awardName")

  def checkCommentForBranch(awards:Seq[AwardAs], rawComment:Option[String]):(Option[Branch],Option[String]) = {
    val branchOpt = ParseUtils.branchFromComment(rawComment)
    if (branchOpt.isDefined) {
      // Okay, the comment appears to be a branch name:
      val branch = branchOpt.get //Branch.byName.get(rawComment.get).get
      if (awards.head.award.branch == branch)
        // The comment just tells us what we already knew: that this award came from the
        // appropriate Kingdom:
        (None, None)
      else
        // This is telling us that the award came from a non-obvious location -- for instance,
        // an AoA from Calontir:
        (Some(branch), None)
    } else {
      (None, rawComment)
    }
  }
  
  def processAwardFields(date:String, awardName:String) = {
    if (currentPersona.isEmpty)
      throw new Exception("Somehow got into an award field without a person-name line first: " + date + " / " + awardName)
    val persona = currentPersona.get
    val parsedDate = new OPShortDate(date)
    
    // Check whether there is a parenthetical comment, and extract the parts if so.
    // This is for cases like "Queen's Cypher (Jana)".
    val (parsedAwardName, rawComment) = ParseUtils.extractComment(awardName)
    
    val awards = Award.find(parsedAwardName)
    val (branch, comment) = checkCommentForBranch(awards, rawComment)
    awards.foreach(award => {
      val rec = Recognition(persona, award.award, award.name, date = Some(parsedDate), 
          inAlpha=true, comment=comment, where=branch)
      persona.addAward(rec)
    })
  }
  
  def processAwardLine(scrubbed:String) = {
    val matched = awardLineRegex.findFirstMatchIn(scrubbed)
    matched match {
      case Some(m) => processAwardFields(m.group("date"), m.group("awardName"))
      case None => Log.error("Unparsed line: " + scrubbed)
    }
  }
  
  // Note that processNodes is recursive. It needs to be, to cope with
  // embedded <br> nodes and the like.
  // The notion here is that we're looking for distinct lines of text.
  // If a line is inside boldfacing, then it's a name section; otherwise,
  // it's an award section.
  def processNodes(nodes:Seq[Node], inName:Boolean):Unit = {
    for (node <- nodes) node match {
      case element:Elem => {
        // This has to be done state-machine style, because the name line
        // is occasionally made up of several nodes:
        node.label match {
          case "b" => processNodes(node.child, true)
          // This is iffy -- we might need to check that it's actually boldfacing
          case "span" => processNodes(node.child, true)
          case "br" => emitName
          case _ => processNodes(node.child, inName)
        }
      }
      case text:Text => {
        if (inName) {
          recordNamePart(text.text)
        } else {
          val scrubbed = StringUtils.scrub(text.text)
          if (scrubbed.length > 0) {
            processAwardLine(scrubbed)
          }
        }
      }
      case _ => Log.print("Node: " + node)
    }     
  }

  def processFile(fileNode: Elem, name:String) = {  
    val body = fileNode \\ "body"
    processNodes(body.head.child, false)
  }
}

object AlphaParser extends AlphaParser {}
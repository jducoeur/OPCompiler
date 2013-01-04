package parsers

import models.Persona

import process._

import scala.util.matching._
import scala.xml._

abstract trait OPFileParser {
  
  def loadFile(fileInfo:OldFile) = {
    XML.load(fileInfo.name)
  }
  
  // This is the heart of a parser, taking a loaded XHTML file and handling it.
  // Note that it returns Unit. Yes, this is kind of evil, but I'm mutating the
  // world in place instead of passing an immutable world around. I may yet regret
  // that decision; we'll see.
  //
  // Actual parsers fill this method in.
  def processFile(fileNode:Elem, name:String):Unit
  
  def handleFile(fileInfo:OldFile) = {
    Log.pushContext(fileInfo.simpleName)
    val fileNode = loadFile(fileInfo)
    // Drop the ".xhtml" at the end
    val baseName = fileInfo.simpleName dropRight 6
    processFile(fileNode, baseName)
    Log.popContext
  }

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
  val separators = new Regex("""^(.*?),?\-? \(?(see|was|formerly|aka|also known as|known as|called):? (.*?)\)?(.*?)$""", "curName", "relation", "rest", "rest2")
  // If it's in parens, the relation is optional:
  val parens = new Regex("""^(.*?),?\-? \((see|was|formerly|aka|also known as|known as|called|):?\s?(.*?)\)(.*?)$""", "curName", "relation", "rest", "rest2")

  val additions = new Regex("""^(.*?) \(((?:of|the|von) .*)\)$""", "prefix", "addition")
  
  def parseRest(sep:String, rest:String):Seq[OneName] = {
    val isCurrent = sep match {
      case "see" => true
      case "was" => false
      case "formerly" => false
      case "aka" => false
      case "also known as" => false
      case "known as" => false
      case "called" => false
      case "" => false
      case _ => throw new Exception("Somehow got an unknown name separator!")
    }
    
    val m = parens.findFirstMatchIn(rest) orElse separators.findFirstMatchIn(rest)
    m match {
      case Some(foundMatch) => OneName(chopAsterisk(m.get.group("curName")), isCurrent) +: parseRest(m.get.group("relation"), m.get.group("rest") + m.get.group("rest2"))
      case None => Seq(OneName(chopAsterisk(rest), isCurrent))
    }
  }
  
  // The entry point: given the raw line of text, check whether it contains
  // any references. Returns a NameBreakdown, which separates this name from
  // any others it may be referencing, and says whether it indicates that this
  // name is an old name.
  def checkReferences(rawName:String):NameBreakdown = {
    // First, add in common interpolations, like "Benjamin (of Aldea)"
    val combinedMatch = additions.findFirstMatchIn(rawName)
    val combinedName = combinedMatch match {
      case Some(cm) => cm.group("prefix") + " " + cm.group("addition")
      case None => rawName
    }
    
    val m = parens.findFirstMatchIn(combinedName) orElse separators.findFirstMatchIn(combinedName)
    m match {
      case Some(foundMatch) => NameBreakdown(m.get.group("curName"), parseRest(m.get.group("relation"), m.get.group("rest") + m.get.group("rest2")))
      case None => NameBreakdown(combinedName, Seq.empty)
    }
  }

    
  def getPersona(nameEntry:String):Persona = {
    val breakdown = checkReferences(nameEntry)
    val name = breakdown.curName.trim
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
    persona
  }
  
}

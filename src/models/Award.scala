package models

import process._
import Gender._

// This captures the fact that *some* award names imply gender (although most don't)
// Hence, there are gendered synonyms that should be used correctly
case class AwardName(name:String, gender:Gender = Gender.Unknown) {
  def canonName = AwardName.canonName(name)
}
object AwardName {
    def canonName(name:String) = name.toLowerCase.replaceAll("'", "")
}

case class Award(branch:Branch, name:AwardName, commentary:Boolean, synonyms:Seq[AwardName]) {
  def awardKeys:Seq[AwardName] = name +: synonyms
  
  def as(s:String):AwardAs = {
    if (name.name == s) AwardAs(this,name)
    else {
      val syn = synonyms.find(_.name == s)
      syn match {
        case Some(awardName) => AwardAs(this,awardName)
        case None => throw new Exception("Couldn't find " + s + " in " + name)
      }
    }
  }
}

// Temporary holder so that we can retain the info during Config2 declaration
case class AwardInfo(name:AwardName, commentary:Boolean = false, synonyms:Seq[AwardName] = Seq.empty)

// Wrapper to preserve the knowledge of which name this award was given under
case class AwardAs(award:Award, name:AwardName)

object Award {
  var knownAwards:Map[String,AwardAs] = Map.empty
  
  implicit def string2AwardInfo(name:String) = AwardInfo(AwardName(name, Gender.Unknown))
 
  // Simple builder, for the most common case:
  def apply(name:String, synonyms:String*) = {
    val syns = synonyms.map(AwardName(_, Gender.Unknown))
    AwardInfo(AwardName(name, Gender.Unknown), false, syns)
  }
  // Detailed builder, for more interesting cases:
  def apply(name:String, 
      gender:Gender = Gender.Unknown, 
      isCommentary:Boolean = false, 
      synonyms:Seq[AwardName] = Seq.empty) = {
    AwardInfo(AwardName(name, gender), isCommentary, synonyms)
  }
  def build(info:AwardInfo, branch:Branch) = {
    addAward(new Award(branch, info.name, info.commentary, info.synonyms))
  }
  
  // This is a horribly ad-hoc but critical method, that examines a piece of business and
  // decides whether it is a real award, or just a bit of commentary. We basically decide it
  // based on some common keywords that indicate non-award business that tends to show up on
  // court reports.
  def isCommentaryBusiness(rawName:String) = {
    val name = rawName.toLowerCase
    val commentRegexes:Seq[scala.util.matching.Regex] = Vector(
    		"""writ""".r,
    		"""charter""".r,
    		"""fealty""".r,
    		"""elect""".r,
    		"""out\s*going""".r,
    		"""backlog""".r,
    		"""vigil""".r)
    commentRegexes.exists(regex => regex.findFirstIn(name) != None)
  }
  
  val noSynonyms:Seq[AwardName] = Seq.empty
  
  def addUnknownAward(name:String) = {
    val awardName = AwardName(name, Gender.Unknown)
    val newAward = Award(UnknownBranch, awardName, isCommentaryBusiness(name), noSynonyms)
    val as = newAward.as(name)
    if (newAward.commentary)
      Log.info("Adding commentary \"" + name + "\"")
    else
      Log.error("Adding unknown award \"" + name + "\"")
    addAwardAs(newAward, awardName)
  }
  
  def addAwardAs(award:Award, name:AwardName) = {
    val as = award.as(name.name)
    knownAwards = knownAwards + (name.canonName -> as)
    as
  }
  
  def addAward(award:Award) = {
    addAwardAs(award, award.name)
    award.synonyms.foreach(addAwardAs(award, _))
  }
  
  def scrubName(inName:String) = {
    var name:String = inName
    // Neuter whitespace, and eliminate duplicates:
    name = name.replaceAll("\\s+", " ")
    name = name.replaceAll("’", "'")
    def stripNoise(strips:String*) = strips.foreach(strip => name = name.replace(strip, ""))
    stripNoise("Order of the ",
    		"Order of ",
    		"Award of the ",
    		"Companion of the ",
    		"Companion of ",
    		"- In Coming",
    		"- Incoming",
    		"Incoming ",
    		"In Coming ")
    // 160 is &nbsp;, which screws things up:
    name = name.filter(_.toInt != 160)
    // This is sometimes used to signify corrections:
    name = name.replace("*", "")
    name = name.trim
    name
  }
  
  def splitNames(name:String):Seq[String] = {
    def splitOn(str:String, splitStr:String):Seq[String] = {
      val i = str.indexOfSlice(splitStr)
      if (i == -1)
        Seq[String](str)
      else
        Seq[String](str.substring(0,i)) ++: splitOn(str.substring(i + splitStr.length), splitStr)
    }

    // HACK: avoid splitting the phrase "Arts & Sciences", or an award that really does
    // have a conjunction such as "Sun and Soil"
    if (name.contains("Science") || name.contains("A&S") || knownAwards.contains(AwardName.canonName(scrubName(name))))
      Seq[String](name)
    else {
      val splitters = Seq(" and ", " & ", " with ", ", ", "w/")
      val noResult:Seq[String] = Seq(name)
      splitters.foldLeft(noResult)((prev, splitter) =>
        if (prev.length == 1)
          splitOn(name, splitter)
        else
          prev
        )
    }
  }

  // This is the accessor for finding the canonical copy of an Award, given a string
  // representation in the OP. I confess, I'm still twitchy about using the
  // Singleton pattern for something like this, but for purposes of this project
  // it's probably good enough.
  def findOne(inName:String):AwardAs = {
    val name = scrubName(inName)
    knownAwards.getOrElse(AwardName.canonName(name), addUnknownAward(name))
  }
  
  def find(inName:String):Seq[AwardAs] = {
    for (name <- splitNames(inName)) yield findOne(name)
  }
}

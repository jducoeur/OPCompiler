package models

import scala.util.matching._

import process._
import Gender._

/**
 * This enumeration roughly matches the one hard-coded into the OP database. Note, however, that
 * the database's actual precedence order is *totally* wrong for the East, and needs rewriting!
 */
object AwardLevel {
  type AwardLevel = Int
  
  val Monarch = 1
  val Heir = 2
  val Ducal = 5
  val County = 6
  val Viscounty = 7
  val Peerage = 8
  val TerritorialBaron = 10
  val RetiredBaron = 22
  val Grants = 14
  val HighMerit = 12
  val CourtBaron = 16
  val Armiger = 17
  val OrderOfHonor = 14
  val KingdomAward = 18
  val PrincipalityAward = 19
  val Baronial = 20
  val Unknown = 100
}

import AwardLevel._

// This captures the fact that *some* award names imply gender (although most don't)
// Hence, there are gendered synonyms that should be used correctly
case class AwardName(name:String, gender:Gender = Gender.Unknown) extends Gendered {
  def canonName = AwardName.canonName(name)
}
object AwardName {
    def canonName(name:String) = name.toLowerCase.replaceAll("'", "")
}

case class Award(branch:Branch, name:AwardName, commentary:Boolean, synonyms:Seq[AwardName], level:AwardLevel) {
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
  
  val shouldEmit = !commentary
  
  val id = Award.nextId()
  
  Award.allAwards += this
}

// Temporary holder so that we can retain the info during Config2 declaration
case class AwardInfo(name:AwardName, commentary:Boolean = false, synonyms:Seq[AwardName] = Seq.empty, level:Option[AwardLevel] = None)

// Wrapper to preserve the knowledge of which name this award was given under
case class AwardAs(award:Award, name:AwardName)

object AwardGroup {
  def apply(level:AwardLevel, infos:AwardInfo*):Seq[AwardInfo] = {
    infos map { info =>
      info.copy(level = Some(level))
    }
  }
}

// Champions are fairly complicated, because each uses a variety of names *and* we
// need to account for any of them being chosen by any of the royalty. So we build up
// this big combinatoric whohah.
object Champion {
  val royals = Seq("King", "Queen", "Prince", "Princess")

  // Given all the possible names and a single Royal, this computes all the ways they
  // *might* be combined in the OP. Note that most of them probably aren't used, but
  // it's easier to just prepare for all the possibilities
  def permutations(titles:Seq[String], royal:String):Seq[String] = {
	// Given one version of this championship name, and one royalty type, give all the names
	def permuteOne(kind:String):Seq[String] = {
	  Seq(royal + "'s Champion of " + kind, 
	      royal + "'s Champion " + kind,
	      royal + "'s " + kind + " Champion", 
	      royal + "'s " + kind + " Championship",
	      royal + "'s " + kind)
	}
	titles flatMap permuteOne
  }
  
  // Given the list of possible names and a specific royal, this builds the actual Award
  // object. Note that we specifically assume that they are all Society-wide.
  def awardForRoyal(titles:Seq[String], royal:String) = {
	val perms = permutations(titles, royal)
	AwardInfo( 
	  AwardName(perms.head, Gender.Unknown), 
	  false, 
	  perms.tail map (synName => AwardName(synName, Gender.Unknown)))
  }
  
  // This takes the list of possible titles used for this championship, with the
  // preferred one first:
  def apply(titles:String*) = royals.map(awardForRoyal(titles, _))
}

object Award extends IdGenerator {
  implicit object AwardOrdering extends Ordering[Award] {
    def compare(x:Award, y:Award) = {
      x.name.name compare y.name.name
    }
  }
  var knownAwards:Map[String,AwardAs] = Map.empty
  import collection.immutable.SortedSet
  var allAwards:SortedSet[Award] = SortedSet.empty

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
    addAward(new Award(branch, info.name, info.commentary, info.synonyms, 
      info.level match {
        case Some(level) => level
        case None if branch.defaultAwardLevel.isDefined => branch.defaultAwardLevel.get
        case _ => AwardLevel.Unknown
    }))
  }
  
  // This is a horribly ad-hoc but critical method, that examines a piece of business and
  // decides whether it is a real award, or just a bit of commentary. We basically decide it
  // based on some common keywords that indicate non-award business that tends to show up on
  // court reports.
  def isCommentaryBusiness(rawName:String) = {
    val name = rawName.toLowerCase
    val commentRegexes:Seq[scala.util.matching.Regex] = Vector(
            """business""".r,
    		"""writ""".r,
    		"""charter""".r,
    		"""fealty""".r,
    		"""elect""".r,
    		"""out\s*going""".r,
    		"""backlog""".r,
    		"""vicar""".r,
    		"""vigil""".r)
    commentRegexes.exists(regex => regex.findFirstIn(name) != None)
  }

  val businessMarker = new Regex("""^\[(.*)\]$""", "actual")
  def markedBusiness(rawName:String) = {
    val mOpt = businessMarker.findFirstMatchIn(rawName)
    mOpt match {
      case Some(m) => (true, m.group("actual"))
      case None => (isCommentaryBusiness(rawName), rawName)
    }
  }
  
  val noSynonyms:Seq[AwardName] = Seq.empty
  
  def addUnknownAward(rawName:String) = {
    val (awardIsCommentary, name) = markedBusiness(rawName)
    val awardName = AwardName(name, Gender.Unknown)
    val newAward = Award(UnknownBranch, awardName, awardIsCommentary, noSynonyms, AwardLevel.Unknown)
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
    		"Companions of the ",
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
  
  def contains(inName:String):Boolean = {
    val name = scrubName(inName)
    knownAwards.contains(AwardName.canonName(name))
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

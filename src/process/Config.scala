package process

import scala.io.Source
import scala.xml._

import models._
import BranchType._
import Gender._

// This loads in the name-synonym file, which is how we deal with the horrible mess of
// the name data. This file is flat text, where each line contains an entry that represents
// a Person. The format is:
//
//   [primary] == [syn[; syn]*] || [typo [; typo*]]
//
// where "primary" is the name for the main entry (registered name if possible); "syn"s are
// the publicly-visible synonyms that should each get a cross-reference enter; and "typo"s
// are misspelling and tweaks found in the data that are too minor to be worth exposing
// visibly.
//
// This information is used to pre-populate the Persona table. It is not expected to be
// comprehensive -- other names will be added as we find them in the data -- but it is used
// to join up things that we already know look problematic.
trait NameConfigLoader {
  def processLine(line:String) = {
    val primaryVsOthers = line.split("==")
    val primary = primaryVsOthers(0).trim
    val others = primaryVsOthers(1)
    val synVsTypo = others.split("""\|\|""")
    def pullApartSection(section:String) = {
      val str = section.trim
      if (str.length == 0)
        Array.empty[String]
      else {
        val entries = str.split(";")
        entries map (_.trim)
      }
    }
    val syns = pullApartSection(synVsTypo(0))
    val typos = (if (synVsTypo.length > 1) pullApartSection(synVsTypo(1)) else pullApartSection(""))
    Persona.addPerson(primary, syns, typos)
  }
  
  def load(fileName:String) = {
    Log.pushContext("Loading names")
    val fileInput = Source.fromFile(fileName)
    val lines = fileInput.getLines
    lines.foreach(processLine(_))
    fileInput.close()
    Log.popContext
  }
}

object NameConfigLoader extends NameConfigLoader {}

class Config(confFileName:String) {
  // The raw configuration file:
  val confFile = XML.load(confFileName)
  
  def firstText(seq:NodeSeq) = seq.head.text
  
  // Set up logging
  val logPath = firstText(confFile \\ "conf" \\ "logTo")
  Log.logTo(logPath)
  
  // Where are the data files located?
  val dataFileLoc = firstText(confFile \\ "conf" \\ "fileLocation")
  
  // The files we need to run
  import parsers._
  val filesToProcess:FilesToProcess = new FilesToProcess(confFile)
  
  // Load the name file
  val nameConfigLoc = firstText(confFile \\ "conf" \\ "names")
  NameConfigLoader.load(nameConfigLoc)
  
  def genderFromNode(node:Node) = {
    val genderNode = node \ "@gender"
    if (genderNode.length > 0)
      Gender.withName(firstText(genderNode))
    else
      Gender.Unknown
  }
  def synonyms(awardNode:Node) = {
    for (synonymNode <- (awardNode \ "synonym"))
      yield AwardName(synonymNode.text, genderFromNode(synonymNode))
  }
  private def addAwardToMap(awardMap:Map[String,AwardAs], awardAses:Seq[AwardAs]) = {
    val awardMappings = 
  	  for (as <- awardAses)
    	  yield (as.name.canonName -> as)
    awardMap ++ awardMappings
  }
  
  def branchAwards(branchNode:Node, branch:Branch):Seq[Award] = {
  	  val awardNodes = branchNode \ "award"
	  def awardFromNode(node:Node):Award = {
	    val name = AwardName(firstText(node \ "@name"), genderFromNode(node))
	    val syn = synonyms(node)
	    Award(branch, name, false, syn)
	  }
	  for (award <- awardNodes) 
	    yield awardFromNode(award)
  }
  
  // Now add the championships, which are a little more complex, because the names
  // are mostly generated. Note that this may need more finessing, to be more
  // branch-specific, but I'd rather not get into that if I can avoid it:
  def championList(node:Node):Seq[Award] = {
	  val championNodes = node \ "champion"
	  def championFromNode(node:Node):Seq[Award] = {
	    val kind = firstText(node \ "@type")
	    val syn = for (synNode <- node \ "synonym")
	      yield synNode.text
	    val royals = Seq("King", "Queen", "Prince", "Princess")
	    def permutations(royal:String):Seq[String] = {
	      // Given one version of this championship name, and one royalty type, give all the names
	      def permuteOne(kind:String):Seq[String] = {
	        Seq(royal + "'s Champion of " + kind, 
	            royal + "'s Champion " + kind,
	            royal + "'s " + kind + " Champion", 
	            royal + "'s " + kind + " Championship",
	            royal + "'s " + kind)
	      }
	      (kind +: syn) flatMap permuteOne
	    }
	    def awardForRoyal(royal:String) = {
	        val main :: rest = permutations(royal)
	        Award(SCA, 
	            AwardName(main, Gender.Unknown), 
	            false, 
	            rest map (synName => AwardName(synName, Gender.Unknown)))
	    }
        for (royal <- royals)
          yield awardForRoyal(royal)
	  }
	  val champAwards = for (champion <- championNodes) 
	      yield championFromNode(champion)
	  champAwards.flatten
  }
  
  def baronList(node:Node, barony:Branch):Seq[Award] = {
	  def awardForGender(baronyName:String, gender:Gender, term:String):Award = {
	    val primaryName = AwardName(term + " " + baronyName, gender)
	    val synNames = Seq(AwardName(term + " of " + baronyName, gender),
	    				   AwardName("Founding " + term + " " + baronyName, gender))
	    Award(barony.parent, primaryName, false, synNames)
	  }
	  def OstgardrHack:Seq[Award] = {
	    if (barony.name == "Ostgardr")
	    	Seq(awardForGender("Ostgardr", Gender.Male, "Viceroy"),
	    		awardForGender("Ostgardr", Gender.Female, "Vicerene"),
	    		awardForGender("Ostgardr", Gender.Female, "Vicereine"))
	    else
	      Seq.empty
	  }
	  val nameVariants = (for (baronyName <- barony.names)
		  yield Seq(awardForGender(baronyName, Gender.Male, "Baron"),
			        awardForGender(baronyName, Gender.Female, "Baroness"))) :+ OstgardrHack
	  nameVariants.flatten
  }
  
  def parseChildBranches(node:Node, branch:Branch, childType:BranchType*):Unit = {
    childType.foreach(kind => {
      val typeName = kind.toString
      val childNodes = node \ typeName
      childNodes.foreach(parseBranch(_, branch))
    })
  }
  
  def addAwards(awards:Seq[Award]) = awards.foreach(Award.addAward(_))
  
  def parseBranchContents(node:Node, branch:Branch) = {
    branch.isDefault = (node \ "@isDefault").nonEmpty
    addAwards(branchAwards(node, branch))
    if (branch.kind == BranchType.Barony)
      addAwards(baronList(node, branch))
    parseChildBranches(node, branch, BranchType.Kingdom, BranchType.Principality, BranchType.Barony)
    branch
  }
  
  def addBranchSynonyms(node:Node, branch:Branch) = {
    val synNodes = node \ "synonym"
    val synonyms = synNodes map (firstText(_))
    synonyms.foreach(branch.addName(_))
  }
  
  def parseBranch(node:Node, parent:Branch) = {
    val name:String = firstText(node \ "@name")
    val branch = Branch.create(node.label, name, parent)
    addBranchSynonyms(node, branch)
    parseBranchContents(node, branch)
  }
  
  // Deal with the main SCA node:
  def parseSCA(root:Node) = {
    val node = (root \ "SCA").head.asInstanceOf[Elem]
    parseBranchContents(node, SCA)
    addAwards(championList(node))
  }
  
  //parseSCA(confFile)
}

object Config {
  // Is there a good way to avoid this Singleton badness? I want to be able to simply say
  // Config.dataFileLoc and things like that, but the vals don't get filled until we've actually
  // read the file in. So I wind up with Config.get.dataFileLoc instead.
  var instance:Option[Config] = None
  def get = instance match {
    case Some(config) => config
    case None => throw new Exception("No Config instance!")
  }
  
  def getFullPath(file:parsers.OldFile) = get.dataFileLoc + file.name
}
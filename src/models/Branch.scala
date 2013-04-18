package models

object BranchType extends Enumeration {
  type BranchType = Value
  
  val Unknown, SCAWide, Kingdom, Principality, Barony = Value
}

import BranchType._

// Represents an SCA branch of any sort, at least conceptually. In practice, we're mainly
// worrying about branches that have ruling heads and give precedential awards: Baronies,
// Principalities and Kingdoms. (Plus possibly some outliers such as Provinces that were
// once Baronies.)
trait Branch extends DelayedInit {
  val name:String
  val kind:BranchType
  
  val id:Int = Branch.nextId
  def emitId:Option[Int] = Some(id)
  val branchType:Int
  val emit:Boolean = true
  
  val defaultAwardLevel:Option[AwardLevel.AwardLevel] = None
  
  private var _parent:Option[Branch] = None
  def parent = {
    _parent match {
      case Some(p) => p
      case None => throw new Exception("No parent defined for branch!")
    }
  }
  private[models] def parent_=(p:Branch) = _parent = Some(p)
  
  // This contains the "primary" name, as well as any synonyms we expect to find as we
  // go:
  var names:Vector[String] = Vector.empty
  
  // Set this to true if it is one of the "default" locations that we don't bother noting
  // in printouts. (Eg, the East)
  var isDefault:Boolean = false
  
  def addName(n:String) = {
    names = names :+ n
    Branch.byName = Branch.byName + (n -> this)
    Branch.allBranches += this
  }
  
  override def delayedInit(subInit: => Unit) = {
	subInit
	addName(name)
	addName(kind.toString + " of " + name)
	addName(kind.toString + " of the " + name)
  }
  
  def requiresBranchSelect = 0
  
  def parentId = _parent map (_.id)
}

trait DeclareableBranch {
  def instantiate(name:String):Branch
  def apply(name:String, 
      isDefault:Boolean = false, 
      synonyms:Seq[String] = Seq.empty,
      awards:Seq[AwardInfo] = Seq.empty,
      children:Seq[Branch] = Seq.empty) = {
    val k = instantiate(name)
    children.foreach(_.parent = k)
    synonyms.foreach(k.addName(_))
    k.isDefault = isDefault
    awards.foreach(Award.build(_, k))
    k
  }
}

// Represents the Society as a whole. Note that it is its own parent! Any recursive algorithms must
// test for this as the ultimate root.
object SCA extends Branch with DeclareableBranch {
  val name = "SCA"
  parent = SCA
  val kind = BranchType.SCAWide
  val branchType = -1
  override val emit = false
  
  def instantiate(name:String) = this
  
  override val id = -1
  override def emitId = None
  override def requiresBranchSelect = 1
}

// There should be one record here for each Kingdom. Note that, in the case of Kingdom,
// "parent" is either "SCA" or the Kingdom that it was originally spawned from when it was
// a Principality. (Keep in mind that most Kingdoms were originally Principalities.)
class SCAKingdom(val name:String) extends Branch {
  val kind = BranchType.Kingdom
  val branchType = 1
  override val defaultAwardLevel = Some(AwardLevel.KingdomAward)
  
  if (name == "East") Kingdom.East = this
}
object Kingdom extends DeclareableBranch {  
  def instantiate(name:String) = new SCAKingdom(name)
  
  // Horrible hack, but useful:
  var East:SCAKingdom = null
}

class Principality(val name:String) extends Branch {
  val kind = BranchType.Principality
  val branchType = 2
  override val defaultAwardLevel = Some(AwardLevel.PrincipalityAward)
}
object Principality extends DeclareableBranch {
  def instantiate(name:String) = new Principality(name)
}

class Barony(val name:String) extends Branch {
  val kind = BranchType.Barony
  val branchType = 3
  override val defaultAwardLevel = Some(AwardLevel.Baronial)
  
  override private[models] def parent_=(p:Branch) = {
    super.parent_=(p)
    awardsForBarons.foreach(Award.addAward(_))
  }
  
  // Once we know the parent Kingdom (so we can add the award), we need to
  // create a whole lot of awards, permuting all of the possible group names,
  // various possible ways you can address a ruling head, and their possible
  // titles
  import models.Gender._
  def awardForGender(nameVariant:String, gender:Gender, term:String):Award = {
    val primaryName = AwardName(term + " " + nameVariant, gender)
    val synNames = Seq(AwardName(term + " of " + nameVariant, gender),
    				   AwardName("Founding " + term + " " + nameVariant, gender),
    				   AwardName("Founding " + term + " of " + nameVariant, gender))
    Award(parent, primaryName, false, synNames, AwardLevel.TerritorialBaron, Some("baron.gif"))
  }
  // Do the Canadian Baronies have French titles instead? If so, I may need to
  // come up with something more general for this.
  def OstgardrHack:Seq[Award] = {
    if (name == "Ostgardr")
    	Seq(awardForGender("Ostgardr", Gender.Male, "Viceroy"),
    		awardForGender("Ostgardr", Gender.Female, "Vicerene"),
    		awardForGender("Ostgardr", Gender.Female, "Vicereine"))
    else
      Seq.empty
  }
  def awardsForBarons = { 
    val v = (for (baronyName <- names)
      yield Seq(awardForGender(baronyName, Gender.Male, "Baron"),
		        awardForGender(baronyName, Gender.Female, "Baroness"))) :+ OstgardrHack
    v.flatten
  }
}
object Barony extends DeclareableBranch {
  def instantiate(name:String) = new Barony(name)
}

object UnknownBranch extends Branch {
  val name = "Unknown Branch"
  val kind = BranchType.Unknown
  parent = SCA
  val branchType = -1
  override val emit = false
  override val id = -1
  override val defaultAwardLevel = Some(AwardLevel.Unknown)
}

object Branch extends IdGenerator {
  var byName:Map[String,Branch] = Map.empty
  
  var allBranches:Set[Branch] = Set.empty
  
  def sortedByIndex:Seq[Branch] = {
    val all = allBranches.toSeq
    all.sortWith { _.id < _.id }
  }
}
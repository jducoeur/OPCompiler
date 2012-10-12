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
  val parent:Branch
  
  // This contains the "primary" name, as well as any synonyms we expect to find as we
  // go:
  var names:Vector[String] = Vector.empty
  
  // Set this to true if it is one of the "default" locations that we don't bother noting
  // in printouts. (Eg, the East)
  var isDefault:Boolean = false
  
  def addName(n:String) = {
    names = names :+ n
    Branch.byName = Branch.byName + (n -> this)
  }
  
  override def delayedInit(subInit: => Unit) = {
	subInit
	addName(name)
	addName(kind.toString + " of " + name)
	addName(kind.toString + " of the " + name)
  }
}

// Represents the Society as a whole. Note that it is its own parent! Any recursive algorithms must
// test for this as the ultimate root.
object SCA extends Branch {
  val name = "SCA"
  val parent = SCA
  val kind = BranchType.SCAWide
}

// There should be one record here for each Kingdom. Note that, in the case of Kingdom,
// "parent" is either "SCA" or the Kingdom that it was originally spawned from when it was
// a Principality. (Keep in mind that most Kingdoms were originally Principalities.)
class SCAKingdom(val name:String, val parent:Branch) extends Branch {
  val kind = BranchType.Kingdom
}

class Principality(val name:String, val parent:Branch) extends Branch {
  val kind = BranchType.Principality
}

class Barony(val name:String, val parent:Branch) extends Branch {
  val kind = BranchType.Barony
}

object UnknownBranch extends Branch {
  val name = "Unknown Branch"
  val kind = BranchType.Unknown
  val parent = SCA
}

object Branch {
  var byName:Map[String,Branch] = Map.empty
  
  def create(kindStr:String, name:String, parent:Branch):Branch = {
    val kind = BranchType.withName(kindStr)
    kind match {
      case BranchType.Kingdom => new SCAKingdom(name, parent)
      case Principality => new Principality(name, parent)
      case Barony => new Barony(name, parent)
      case _ => throw new Exception("I don't know what kind of branch a " + kindStr + " is.")
    }
  }
}
package models

object Gender extends Enumeration {
  type Gender = Value
  val Unknown, Male, Female, Conflict = Value
  
  // Utility for taking a bunch of gendered things and getting the consensus gender:
  def foldGender[T](coll:Iterable[T], getGender:(T) => Gendered):Gender = {
    (Gender.Unknown /: coll) { (gender, item) =>
      val gendered = getGender(item)
      if (gendered.hasGender) {
        if (gender == Unknown || gendered.gender == gender)
          gendered.gender
        else
          Conflict
      } else
        gender
    }
  }
  
  def emit(gender:Gender) = {
    gender match {
      case Male => "M"
      case Female => "F"
      case _ => "U"
    }
  } 
}

import Gender._
trait Gendered {
  def gender:Gender
  def hasGender = gender match {
    case Male => true
    case Female => true
    case _ => false
  }
}

// Represents the game-side view of a person. Note that a single Person can
// have any number of Personae. (They could also have any number of mundane
// identities, but we're not worrying about that yet.)
class Persona(val scaName:String, var person:Person, val isTypo:Boolean = false) extends Ordered[Persona] with Gendered {
  
  override def compare(that:Persona) = scaName.compare(that.scaName)
  
  // We suss the gender of a persona from its awards.
  import Gender._
  def gender:Gender = Gender.foldGender(awards, {recog:Recognition => recog.as})

  // We can sometimes find this out from the Alpha list. Note that "false"
  // really means "we don't know"; "true" means there is reason to believe
  // it is registered.
  var registeredName:Boolean = false
  
  // What has been given to this persona?
  var awards:Vector[Recognition] = Vector.empty
  def allAwards = (person.personae :\ Vector.empty[Recognition]) (_.awards ++ _)

  import scala.util.Sorting
  def sortedAwards = Sorting.stableSort(awards.toSeq)
  
  person.personae = person.personae :+ this
  
  Persona.byPersona = Persona.byPersona + (scaName -> this)
  
  def addAward(award:Recognition) = {
    val existing = allAwards.find(_.matches(award))
    val recipient = (if (existing.isDefined) existing.get.recipient else this)
    recipient.awards = existing match {
      // There's already a record, so merge them
      case Some(other) => other.recipient.awards.filter(_ != other) :+ other.merge(award)
      // Didn't find it, so tack it on
      case None => award +: awards
    }
  }
  
  def isMain = this == person.mainPersona
  
  def mainStr:String = {
    def personaStr(p:Persona):String = {
      if (p == this) ""
      else if (p.isTypo) "" else "  AKA: " + p.scaName
    }
    def referenceStr = {
	  val stringed = person.personae map personaStr
	  val refs = stringed.filter(_.length > 0).mkString("\n")
	  if (refs.length > 0)
	    refs + "\n"
	  else
	      ""
    }
    def awardStr = {
      val sortedAwards = Sorting.stableSort(allAwards.toSeq)
      sortedAwards.mkString("\n")
    }
    referenceStr + awardStr
  }
  
  def refStr = "  SEE: " + person.mainPersona.scaName
  
  override def toString = {
    if (isTypo) ""
    else {
	    "== " + scaName +
	    (if (registeredName) " [registered]" else "") +
	    (if (person.deceased) " [deceased]" else "") + " ==\n" +
	    (if (isMain) mainStr else refStr) +
	    "\n"
    }
  }
}

// The Person record pulls together a lot of information. It is currently
// built as semi-mutable, mostly because it doesn't seem worth the extra effort
// to make it immutable and it clearly evolves as we read in more files.
class Person(mainPersonaName:String) extends Gendered {
  // This contains any number of personae, including the "main" one (which might change):
  var personae:Vector[Persona] = Vector() 
  
  val id = Person.nextId()
  
  // Note that this is subject to change, especially if we wind up merging:
  val mainPersona:Persona = new Persona(mainPersonaName, this)
  
  // TODO: we should have a mechanism to guess Gender based on what's known about
  // award Genders.
  
  var deceased:Boolean = false
  def emitDeceased = if (deceased) 1 else 0
  
  // We attempt to suss the person's gender from their personae -- if they are
  // consistent, we use that. Otherwise, we play it safe and leave it unknown for now;
  // it can always get fixed in the DB later.
  import Gender._
  def gender:Gender = Gender.foldGender(personae, { persona:Persona => persona })
  def emitGender = Gender.emit(gender)
  
  def emitAlternateNames = {
    val nameList = (List.empty[String] /: personae) { (list,persona) =>
      if (persona.isMain || persona.isTypo)
        list
      else
        list :+ persona.scaName
    }
    if (nameList.length > 0)
      Some(nameList.mkString(","))
    else
      None
  }
  
  def getPersona(name:String) = {
	personae.find(_.scaName == Persona.scrub(name)).getOrElse(throw new Exception("Can't find persona " + name))
  }
  
  def addAward(personaName:String, award:Recognition) = getPersona(personaName).addAward(award)
  
  // Adds the other Person record into this one. The "winning" record should be the one we currently
  // believe has the best mainPersona.
  def merge(otherPerson:Person) = {
    for (persona <- otherPerson.personae) {
      persona.person = this
      personae = personae :+ persona
    }
    if (otherPerson.deceased)
      deceased = true
      
    Person.allPeople -= otherPerson
  }
  
  def recognitions:Seq[Recognition] =
    for (persona <- personae;
         award <- persona.awards)
      yield award
  
  Person.allPeople += this
}

object Persona {
  var byPersona:Map[String,Persona] = Map.empty
  
  import scala.util.Sorting
  def byName:Seq[Persona] = {
    val all = byPersona.values.toSeq
    Sorting.stableSort(all)
  }
 
  // This is solely intended for use at config time, from the Names file. It adds a
  // full person, with synonyms, with fewer sanity-checks, so it assumes clean data
  def addPerson(mainName:String, syns:Seq[String], typos:Seq[String]) = {
    val person = new Person(mainName)
    syns.foreach(new Persona(_, person, false))
    typos.foreach(new Persona(_, person, true))
  }
  
  def scrub(name:String) = {
    process.StringUtils.scrub(name.replaceAll("""\*""", ""))
  }

  // Note that this can add the personaName to byPersona as a side-effect:
  def find(personaName:String, otherNames:Seq[String] = Seq.empty) = {
    val name = scrub(personaName)
    val mainPersona = byPersona.getOrElse(name, new Person(name).mainPersona)
    val mainPerson = mainPersona.person
    otherNames.foreach(otherName => {
      val otherPersona = byPersona.getOrElse(otherName, new Persona(otherName, mainPerson))
      if (otherPersona.person != mainPerson)
        mainPerson.merge(otherPersona.person)
    })
    mainPersona
  }
}

object Person extends IdGenerator {
  implicit val PersonOrdering = Ordering.by { person:Person => (person.mainPersona.scaName) }
  
  import collection.immutable.SortedSet
  var allPeople:SortedSet[Person] = SortedSet.empty
}
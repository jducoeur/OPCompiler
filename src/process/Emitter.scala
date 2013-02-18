package process

import models._

object Emitter {
  def printValues(vals:Any*) = {
    val concat = vals.mkString(",")
    Log.print("(" + concat + ")")
  }
  
  def sqlStr(str:String) = "'" + str + "'"
  
  def toPHPName(name:String):String = {
    name.replace(" ", "_").toUpperCase().replace("'", "").replace("-", "_")
  }
  
  def emitAll = {
    emitBranches
    emitAwards
    emitPeople
  }
  
  case class SqlField[T](fieldName:String, extractor:T => Any)
  case class SqlInfo[T](tableName:String, gate:Option[T => Boolean], fields:SqlField[T]*) {
    def emit(items:Iterable[T]) = {
      val valueNames = fields map (_.fieldName)
      Log.print("INSERT INTO '"+tableName+"' ("+valueNames.mkString(",")+") VALUES ")
      items.foreach { item =>
        if (gate.isEmpty || gate.get(item)) {
          val fieldValues = fields map (_.extractor(item))
          printValues(fieldValues:_*)
        }
      }
      Log.print(";")
    }
  }
  
  def emitBranches = {
	// Print out the final SQL output:
	Log.pushContext("SQL Branch Output")
	val info = SqlInfo[Branch]("branch", Some(_.emit),
	    SqlField("branch_id", (_.emitIndex)),
	    SqlField("branch", (branch => sqlStr(branch.name))),
	    SqlField("parent_branch_id", (_.emitParentIndex)),
	    SqlField("branch_type_id", (_.branchType))
	    )
	val allBranches = Branch.sortedByIndex
	info.emit(allBranches)
	Log.popContext	  
	
	Log.pushContext("PHP define Branch Output")
	allBranches.foreach { branch =>
	  if (branch.emit)
	    Log.print("$" + toPHPName(branch.name) + " = " + branch.index + ";")
	}
	Log.popContext
  }
  
  def emitAwards = {
    Log.pushContext("SQL Award Output")
    val awards = Award.allAwards filter (_.shouldEmit)
    Log.print("INSERT INTO 'award' (award_id,award_name,select_branch,type_id,branch_id) VALUES ")
    awards.foreach { award =>
      printValues(
        award.id,
        sqlStr(award.name.name),
        award.branch.requiresBranchSelect,
        award.level,
        award.branch.emitIndex
      )
    }
    Log.print(";")
    Log.popContext
    
    Log.pushContext("PHP define Awards")
    awards.foreach { award =>
      Log.print("$" + toPHPName(award.name.name) + "_ID = " + award.id + ";")
    }
    Log.popContext
  }
  
  def emitPeople = {
    Log.pushContext("SQL Person Output")
    val people = Person.allPeople
    Log.print("INSERT INTO 'atlantian' (atlantian_id,sca_name,alternate_names,gender,deceased) VALUES ")
    people.foreach { person =>
      printValues(
        person.id,
        sqlStr(person.mainPersona.scaName),
        sqlStr(person.emitAlternateNames),
        person.emitGender,
        person.emitDeceased
      )
    }
    Log.print(";")
    Log.popContext
  }
}
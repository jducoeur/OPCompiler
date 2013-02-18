package process

import models._

object Emitter {
  def q(str:String) = "`" + str + "`"
  
  def sqlStr(v:Any):String = {
      v match {
        case d:OPDate => sqlStr(d.sqlString)
        case str:String => {
          if (str.length() == 0)
            "NULL"
          else
            q(str)
        }
        case opt:Option[Any] => {
          opt match {
            case Some(s) => sqlStr(s)
            case None => "NULL"
          }
        }
        case _ => v.toString
      }    
  }
  
  def printValues(vals:Any*) = {
    val transformed = vals map sqlStr
    val concat = transformed.mkString(",")
    Log.print("(" + concat + ")")
  }
  
  def toPHPName(name:String):String = {
    name.replace(" ", "_").toUpperCase().replace("'", "").replace("-", "_")
  }
  
  def emitAll = {
    emitBranches
    emitAwards
    emitPeople
    emitReigns
    emitEvents
    emitCourts
    emitRecognitions
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
  
  def emitSql[T](name:String, coll:Iterable[T], info:SqlInfo[T]) = {
    Log.pushContext("SQL "+name+" Output")
    info.emit(coll)
    Log.popContext    
  }
  
  def emitBranches = {
	val allBranches = Branch.sortedByIndex
	emitSql("Branch", allBranches,
	  SqlInfo[Branch]("branch", Some(_.emit),
	    SqlField("branch_id", (_.emitIndex)),
	    SqlField("branch", (_.name)),
	    SqlField("parent_branch_id", (_.emitParentIndex)),
	    SqlField("branch_type_id", (_.branchType))
	    ))	  
	
	Log.pushContext("PHP define Branch Output")
	allBranches.foreach { branch =>
	  if (branch.emit)
	    Log.print("$" + toPHPName(branch.name) + " = " + branch.index + ";")
	}
	Log.popContext
  }
  
  def emitAwards = {
    val awards = Award.allAwards filter (_.shouldEmit)
    emitSql("Award", awards,
      SqlInfo[Award]("award", None,
        SqlField("award_id", (_.id)),
        SqlField("award_name", (_.name.name)),
        SqlField("select_branch", (_.branch.requiresBranchSelect)),
        SqlField("type_id", (_.level)),
        SqlField("branch_id", (_.branch.emitIndex))
        ))
    
    Log.pushContext("PHP define Awards")
    awards.foreach { award =>
      Log.print("$" + toPHPName(award.name.name) + "_ID = " + award.id + ";")
    }
    Log.popContext
  }

  def emitPeople = {
    emitSql("Person", Person.allPeople,
      SqlInfo[Person]("atlantian", None,
        SqlField("atlantian_id", (_.id)),
        SqlField("sca_name", (_.mainPersona.scaName)),
        SqlField("alternate_names", (_.emitAlternateNames)),
        SqlField("gender", (_.emitGender)),
        SqlField("deceased", (_.emitDeceased))
        ))
  }
  
  def emitReigns = {
    emitSql("Reign", Reign.allReigns.values,
      SqlInfo[Reign]("reign", None,
        SqlField("reign_id", (_.id)),
        SqlField("king_id", (_.king.person.id)),
        SqlField("queen_id", (_.queen.person.id)),
        SqlField("reign_start_sequence", (_.id)),
        SqlField("reign_start_date", (_.start)),
        SqlField("reign_end_date", (_.end))
        ))
  }
  
  def emitEvents = {
    emitSql("Event", Court.allCourts,
      SqlInfo[Court]("event", None,
        SqlField("event_id", (_.id)),
        SqlField("event_name", (_.title)),
        SqlField("start_date", (_.date)),
        SqlField("end_date", (_.date)),
        SqlField("branch_id", (_ => Kingdom.East.index))
        ))
  }
  
  def emitCourts = {
    emitSql("Court Report", Court.allCourts,
        SqlInfo[Court]("court_report", None,
          SqlField("court_report_id", (_.id)),
          SqlField("event_id", (_.id)),
          SqlField("reign_id", (_.reign.id)),
          SqlField("court_date", (_.date)),
          SqlField("kingdom_id", (_ => Kingdom.East.index))
        ))
  }
  
  def emitRecognitions = {
    val allRecs:Seq[Recognition] = (Vector.empty[Recognition] /: Person.allPeople) { (vec, person) =>
      vec ++ person.recognitions
    }
    emitSql("Recognition", allRecs,
        SqlInfo[Recognition]("atlantian_award", None,
          SqlField("atlantian_award_id", (_.id)),
          SqlField("atlantian_id", (_.recipient.person.id)),
          SqlField("award_id", (_.award.id)),
          SqlField("award_date", (_.date)),
          SqlField("event_id", (_.when map (_.id))),
          SqlField("sequence", (_.index)),
          SqlField("comments", (_.comment)),
          SqlField("court_report_id", (_.when map (_.id))),
          SqlField("gender", (_.emitGender))
        ))
  }
}
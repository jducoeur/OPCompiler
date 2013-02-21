package process

import java.io._

import models._

object Emitter {
  def q(str:String) = "'" + str.replace("\\", "").replace("'", "''") + "'"
  
  def sqlStr(v:Any):String = {
      v match {
        case d:OPDate => sqlStr(d.sqlString)
        case n:Int => {
          n match {
            case -1 => "NULL"
            case _ => n.toString
          }
        }
        case str:String => {
          if (str.length() == 0)
            "NULL"
          else
            q(str)
        }
        case opt:Option[_] => {
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
    print("(" + concat + ")")
  }
  
  def toPHPName(name:String):String = {
    name.replace(" ", "_").toUpperCase().replace("'", "").replace("-", "_")
  }
  
  var writer:PrintWriter = null;
  def print(msg:String) = writer.print(msg)
  def println(msg:String) = writer.println(msg)
  
  def emitAll = {
    
    writer = new PrintWriter(new File("db_content.sql"), "UTF-8")
    
    println("USE `atlantia_auth`;")
    emitPeople
    
    println("USE `atlantia_op`;")
    emitBranches
    emitAwards
    emitReigns
    emitEvents
    emitCourts
    emitRecognitions
    
    writer.flush
    writer.close
  }
  
  case class SqlField[T](fieldName:String, extractor:T => Any)
  case class SqlInfo[T](tableName:String, gate:Option[T => Boolean], fields:SqlField[T]*) {
    def emit(items:Iterable[T]) = {
      val valueNames = fields map (_.fieldName)
      println("INSERT INTO `"+tableName+"` ("+valueNames.mkString(",")+") VALUES ")
      var first = true
      items.foreach { item =>
        if (gate.isEmpty || gate.get(item)) {
          if (first)
            first = false
          else
            println(",")
            // TEMP:
//      println("INSERT INTO `"+tableName+"` ("+valueNames.mkString(",")+") VALUES ")
          val fieldValues = fields map (_.extractor(item))
          printValues(fieldValues:_*)
//      println(";")
        }
      }
      println(";")
    }
  }
  
  def emitSql[T](name:String, coll:Iterable[T], info:SqlInfo[T]) = {
    println("\n\n-- SQL "+name+" Output\n")
    info.emit(coll)
  }
  
  def emitBranches = {
	val allBranches = Branch.sortedByIndex.reverse
	emitSql("Branch", allBranches,
	  SqlInfo[Branch]("branch", Some(_.emit),
	    SqlField("branch_id", (_.id)),
	    SqlField("branch", (_.name)),
	    SqlField("parent_branch_id", (_.parentId)),
	    SqlField("branch_type_id", (_.branchType))
	    ))	  
	
	Log.pushContext("PHP define Branch Output")
	allBranches.foreach { branch =>
	  if (branch.emit)
	    Log.print("$" + toPHPName(branch.name) + " = " + branch.id + ";")
	}
	Log.popContext
  }
  
  def emitAwards = {
    val awards = Award.allAwards // filter (_.shouldEmit) I need all the awards, even the comments, because they are ref'ed
    emitSql("Award", awards,
      SqlInfo[Award]("award", None,
        SqlField("award_id", (_.id)),
        SqlField("award_name", (_.name.name)),
        SqlField("select_branch", (_.branch.requiresBranchSelect)),
        SqlField("type_id", (_.level)),
        SqlField("branch_id", (_.branch.id))
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
        SqlField("branch_id", (_ => Kingdom.East.id))
        ))
  }
  
  def emitCourts = {
    emitSql("Court Report", Court.allCourts,
        SqlInfo[Court]("court_report", None,
          SqlField("court_report_id", (_.id)),
          SqlField("event_id", (_.id)),
          SqlField("reign_id", (_.reign.id)),
          SqlField("court_date", (_.date)),
          SqlField("kingdom_id", (_ => Kingdom.East.id))
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
          SqlField("sequence", (_.index.getOrElse(0))),
          SqlField("comments", (_.comment)),
          SqlField("court_report_id", (_.when map (_.id))),
          SqlField("gender", (_.emitGender))
        ))
  }
}
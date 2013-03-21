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
    name.replace(" ", "_").toUpperCase().replace("'", "").replace("-", "_").replace(".", "_")
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
    
    Log.print("Emitted " + Person.allPeople.size + " distinct Person records")
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
        SqlField("collective_name", ("Order of the " + _.name.name)),
        SqlField("award_name_male", (_.nameForGender(Gender.Male))),
        SqlField("award_name_female", (_.nameForGender(Gender.Female))),
        SqlField("select_branch", (_.branch.requiresBranchSelect)),
        SqlField("award_file_name", (_.imageFile)),
        SqlField("type_id", (_.level)),
        SqlField("branch_id", (_.branch.emitId))
        ))
    
    // This should get copied into op/html/db:
    val awardWriter = new PrintWriter(new File("award_defines.php"), "UTF-8")
    awardWriter.println("<?php")
    awardWriter.println("// GENERATED by the Eastrealm OP Compiler")
    awardWriter.println()
    awards.foreach { award =>
      awardWriter.println("$" + toPHPName(award.name.name) + "_ID = " + award.id + ";")
    }
    awardWriter.println("?>")
    awardWriter.flush
    awardWriter.close
  }

  def emitPeople = {
    emitSql("Person", Person.allPeople,
      SqlInfo[Person]("atlantian", None,
        SqlField("atlantian_id", (_.id)),
        SqlField("sca_name", (_.mainPersona.scaName)),
        SqlField("alternate_names", (_.emitAlternateNames.map(_.slice(0, 254)))),
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
        SqlField("monarchs_display", (_.emitDisplay)),
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
          SqlField("received_date", (_.date)),
          SqlField("entered_date", (_.date)),
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
          SqlField("award_date", (_.emitDate)),
          SqlField("event_id", (_.when map (_.id))),
          SqlField("sequence", (_.index.getOrElse(0))),
          SqlField("comments", (_.comment)),
          SqlField("court_report_id", (_.when map (_.id))),
          SqlField("branch_id", (_.emitWhere)),
          SqlField("gender", (_.emitGender)),
          SqlField("given_as", (_.recipient.scaName))
        ))
  }
}
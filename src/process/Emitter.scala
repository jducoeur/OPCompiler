package process

import models._

object Emitter {
  def printValues(vals:Any*) = {
    val concat = vals.mkString(",")
    Log.print("(" + concat + ")")
  }
  
  def toPHPName(name:String):String = {
    name.replace(" ", "_").toUpperCase().replace("'", "").replace("-", "_")
  }
  
  def emitBranches = {
	// Print out the final SQL output:
	Log.pushContext("SQL Branch Output")
	Log.print("INSERT INTO `branch` (branch_id,branch,parent_branch_id,branch_type_id) VALUES ")
	val allBranches = Branch.sortedByIndex
	allBranches.foreach { branch =>
	  if (branch.emit)
		  printValues(
		    branch.emitIndex,
		    "'" + branch.name + "'",
		    branch.emitParentIndex,
		    branch.branchType
		    )
	}
	Log.print(";")
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
    Log.print("INSERT INTO 'award' (award_id,award_name,select_branch,) VALUES ")
    Log.popContext
  }
}
package process

import models._

object Emitter {
  def printValues(vals:Any*) = {
    val concat = vals.mkString(",")
    Log.print("(" + concat + ")")
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
  }
}
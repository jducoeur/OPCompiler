package parsers

import scala.util.matching.Regex

import models.Branch

trait ParseUtils {
  val commentRegex2:Regex = new Regex("""^(.*?) - (.*)$""", "name", "comment")
  
  // This finds comments that we want to track but aren't part of the award name, eg,
  // "Queen's Honor of Distinction (Jana IV)" or "Golden Lyre (Silk Pillow)". It returns
  // (content, Option(comment)).
  def extractComment2(field:String) = {
    if (models.Award.isCommentaryBusiness(field)) {
      (field, None)
    } else {
      val commentMatch = commentRegex2.findFirstMatchIn(field)
	  val comment = commentMatch map (_.group("comment"))
	  val parsedField = (commentMatch map (_.group("name"))).getOrElse(field)
	  (parsedField, comment)    
    }
  }
  
  val commentRegex:Regex = new Regex("""^(.*?) \((.*)\)$""", "name", "comment")
  
  // This finds comments that we want to track but aren't part of the award name, eg,
  // "Queen's Honor of Distinction (Jana IV)" or "Golden Lyre (Silk Pillow)". It returns
  // (content, Option(comment)).
  def extractComment(field:String) = {
    val commentMatch = commentRegex.findFirstMatchIn(field)
    val comment = commentMatch map (_.group("comment"))
    val parsedField = (commentMatch map (_.group("name"))).getOrElse(field)
    if (comment.isEmpty)
      extractComment2(field)
    else
      (parsedField, comment)
  }
  
  def branchFromComment(rawComment:Option[String]):Option[Branch] = {
    rawComment match {
      case None => None
      case Some(c) => {
        // Deal with common cases like "Bright Hills, Atlantia"
        val names = c.split(',')
        Branch.byName.get(names.head)
      }
    }
  }
}

object ParseUtils extends ParseUtils {

}
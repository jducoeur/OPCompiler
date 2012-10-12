package parsers

import scala.util.matching.Regex

trait ParseUtils {
  val commentRegex:Regex = new Regex("""^(.*?) \((.*)\)$""", "name", "comment")
  
  // This finds comments that we want to track but aren't part of the award name, eg,
  // "Queen's Honor of Distinction (Jana IV)" or "Golden Lyre (Silk Pillow)". It returns
  // (content, Option(comment)).
  def extractComment(field:String) = {
    val commentMatch = commentRegex.findFirstMatchIn(field)
    val comment = commentMatch map (_.group("comment"))
    val parsedField = (commentMatch map (_.group("name"))).getOrElse(field)
    (parsedField, comment)
  }
}

object ParseUtils extends ParseUtils {

}
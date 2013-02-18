package parsers

import scala.util.matching._

import models._

import process.Log

object ReignsParser extends OPFileParser with BigTableParser {
  type TypedRow = Reign
  
  val PairRegex:Regex = """(.*) and (.*)""".r
  
  def transformRow(rowVals:List[String]):TypedRow = {
    val pairNames :: startDate :: asDate :: fileName :: excess = rowVals
    
    val PairRegex(kingName, queenName) = pairNames
    val king = Persona.find(kingName)
    val queen = Persona.find(queenName)
    val start = new OPShortDate(startDate)
    
    Reign(Reign.nextId(), king, queen, start, fileName)
  }

  def groupMatch(first:TypedRow, second:TypedRow):Boolean = true
  
  def handleGroup(group:RowGroup, name:String) = {
    group.foreach { reign =>
      Reign.allReigns += (reign.id -> reign)
      Reign.byFilename += (reign.filename -> reign)
    }
    
    Reign.allReigns.foreach { reign => Log.info(reign._2) }
  }
}
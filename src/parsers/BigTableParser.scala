package parsers

import scala.xml._
import models._
import process._

/**
 * General mechanism for files which consist of one, big, regular table. This takes the
 * table and turns it into a stream of tuples, to process as Court Reports.
 * 
 * It is expected that this will be mixed in with an OPFileParser.
 */
trait BigTableParser {
  
  /////////////////////////////////
  //
  // Overrides
  //
  
  /**
   * The type to produce for each resulting row.
   */
  type TypedRow
  
  /**
   * Given a list of cell values, turn that into the target type.
   */
  def transformRow(rowVals:List[String]):TypedRow
  
  /**
   * Is the second row in the same grouping as the first?
   */
  def groupMatch(first:TypedRow, second:TypedRow):Boolean
  
  /**
   * Given a group of related transformed rows, actually process them.
   */
  def handleGroup(group:RowGroup)
  
  /////////////////////////////////
  //
  // Common Code
  //
  
  type RowGroup = Vector[TypedRow]
  
  def processRow(rowNode:Node) = {
    val cells = rowNode \ "td" toList
    val cellText = cells map (_.text) map StringUtils.scrub
    transformRow(cellText)
  }
  
  def processTable(tableNode:Node) = {
    // Note that we start with the *second* record -- the assumption is that the first is
    // column headers:
    val rows = (tableNode \\ "tr").tail
    val init = Vector(Vector.empty[TypedRow])
    val allGroups = (init /: rows) { (groups, rawRow) =>
      val row = processRow(rawRow)
      val curGroup = groups.last
      if (curGroup.isEmpty || groupMatch(curGroup.last, row))
        // They're in the same group, so just append:
        groups.dropRight(1) :+ (curGroup :+ row)
      else
        groups :+ Vector(row)
    }
    allGroups foreach handleGroup
  }
  
  /**
   * Standard file-handler, defined in OPFileParser.
   */
  def processFile(fileNode:Elem, name:String) = {
    // Each of these tables represents a single Court Report
    val informationTable = (fileNode \\ "table").head
    processTable(informationTable)
  }
}
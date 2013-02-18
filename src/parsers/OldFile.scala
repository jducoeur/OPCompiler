package parsers

import scala.xml._

import process._

import java.io.File

object FileType extends Enumeration {
  type FileType = Value
  val CourtReport, Alpha, AwardListing, Reigns = Value
}

import FileType._
case class OldFile(kind:FileType, name:String, parser:OPFileParser) {
  def simpleName = new File(name).getName
}

object FilesToProcess {
  val dataDir = "data"
  
  def makeFiles(dirName:String, fileType:FileType, parser:OPFileParser) = {
    val dir = new File(dataDir + dirName)
    dir.listFiles filter (_.isFile) map (file => new OldFile(fileType, file.getAbsolutePath, parser))
  }

  val reigns = new OldFile(FileType.Reigns, new File(dataDir + "\\reigns.xhtml").getAbsolutePath, ReignsParser)
  val alphas = makeFiles("\\alpha", FileType.Alpha, AlphaParser)
  val courtReports = 
    makeFiles("\\chrono", FileType.CourtReport, CurrentCourtReportParser) ++: 
    makeFiles("\\chrono\\oneTable", FileType.CourtReport, OneTableCourtReportParser) ++:
    makeFiles("\\chrono\\oneTable\\threeColumn", FileType.CourtReport, ThreeColumnCourtReportParser) ++:
    makeFiles("\\chrono\\ancient", FileType.CourtReport, AncientCourtReportParser) ++:
    makeFiles("\\chrono\\word", FileType.CourtReport, WordCourtReportParser)
  val awards = makeFiles("\\awards", FileType.AwardListing, AwardListingParser)
  
  def processOneType(files:Seq[OldFile], title:String) = {
    Log.pushContext(title)
    for (file <- files) file.parser.handleFile(file)
    Log.popContext    
  }
  
  def processAll = {
    Log.pushContext("Reigns")
    reigns.parser.handleFile(reigns)
    Log.popContext
    
    processOneType(courtReports, "Court Reports")
    processOneType(alphas, "Alphabetical")
    processOneType(awards, "Award Listings")
  }  
}
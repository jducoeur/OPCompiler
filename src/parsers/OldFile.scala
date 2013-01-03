package parsers

import scala.xml._

import process._

import java.io.File

object FileType extends Enumeration {
  type FileType = Value
  val CourtReport, Alpha, AwardListing = Value
}

abstract trait OldFile {
  import FileType._
  val kind:FileType
  val name:String
  val parser:OPFileParser
  def simpleName = new File(name).getName
}

class CourtReportFile(val name:String) extends OldFile {
  val kind = FileType.CourtReport
  val parser = CurrentCourtReportParser
}

class CourtReportOneTableFile(val name:String) extends OldFile {
  val kind = FileType.CourtReport
  val parser = OneTableCourtReportParser
}

class AlphaFile(val name:String) extends OldFile {
  val kind = FileType.Alpha
  val parser = AlphaParser
}

class AwardFile(val name:String) extends OldFile {
  val kind = FileType.AwardListing
  val parser = AwardListingParser
}

object FilesToProcess {
  val dataDir = "data"
  val courtReportDir = new File(dataDir + "\\chrono")
  val courtReportOneTableDir = new File(dataDir + "\\chrono\\oneTable")
  val alphaDir = new File(dataDir + "\\alpha")
  val awardDir = new File(dataDir + "\\awards")
  
  def makeFiles(dir:File)(builder: String => OldFile) = {
    dir.listFiles filter (_.isFile) map (file => builder(file.getAbsolutePath))
  }
  
  val alphas = makeFiles(alphaDir)(new AlphaFile(_))
  val courtReports = makeFiles(courtReportDir)(new CourtReportFile(_)) ++: makeFiles(courtReportOneTableDir)(new CourtReportOneTableFile(_))
  val awards = makeFiles(awardDir)(new AwardFile(_))
  
  def processOneType(files:Seq[OldFile], title:String) = {
    Log.pushContext(title)
    for (file <- files) file.parser.handleFile(file)
    Log.popContext    
  }
  
  def processAll = {
    processOneType(courtReports, "Court Reports")
    processOneType(alphas, "Alphabetical")
    processOneType(awards, "Award Listings")
  }  
}
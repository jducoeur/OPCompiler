package parsers

import scala.xml._

import process._

object FileType extends Enumeration {
  type FileType = Value
  val CourtReport, Alpha, AwardListing = Value
}

abstract trait OldFile {
  import FileType._
  val kind:FileType
  val name:String
  val parser:OPFileParser
}

class CourtReportFile(val xml:Node) extends OldFile {
  val name = xml.text
  val kind = FileType.CourtReport
  
  // Arguably a little evil to set this up here, with the parser assigned by kind;
  // the separation of concerns is crappy
  val parser = (xml \ "@kind").text match {
    case "current" => CurrentCourtReportParser
  } 
}

class AlphaFile(val xml:Node) extends OldFile {
  val name = xml.text
  val kind = FileType.Alpha
  val parser = AlphaParser
}

class FilesToProcess(val confFile:Elem) {
  private val courtReportNodes = confFile \\ "courtReports" \\ "file"
  val courtReports = courtReportNodes map {new CourtReportFile(_)}
  
  private val alphaNodes = confFile \\ "alphas" \\ "file"
  val alphas = alphaNodes map {new AlphaFile(_)}
  
  def processAll = {
    Log.pushContext("Court Reports")
    for (file <- courtReports) file.parser.handleFile(file)
    Log.popContext
    
    Log.pushContext("Alphabetical")
    for (file <- alphas) 
      file.parser.handleFile(file)
    Log.popContext
  }
}
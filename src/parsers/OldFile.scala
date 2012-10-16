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
}

class CourtReportFile(val name:String) extends OldFile {
  val kind = FileType.CourtReport
  val parser = CurrentCourtReportParser
  
  // Arguably a little evil to set this up here, with the parser assigned by kind;
  // the separation of concerns is crappy
  // TODO: redo this for the new world -- we need some way to specify which parser to use
  // for the older court report files
//  val parser = (xml \ "@kind").text match {
//    case "current" => CurrentCourtReportParser
//  } 
}

class AlphaFile(val name:String) extends OldFile {
  val kind = FileType.Alpha
  val parser = AlphaParser
}

object FilesToProcess {
  val dataDir = "data"
  val courtReportDir = new File(dataDir + "\\chrono")
  val alphaDir = new File(dataDir + "\\alpha")
  
  val alphas = alphaDir.listFiles map (file => new AlphaFile(file.getAbsolutePath))
  val courtReports = courtReportDir.listFiles map  (file => new CourtReportFile(file.getAbsolutePath))
  
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
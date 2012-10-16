package process

import scala.io.Source
import scala.xml._

import models._
import BranchType._
import Gender._

// This loads in the name-synonym file, which is how we deal with the horrible mess of
// the name data. This file is flat text, where each line contains an entry that represents
// a Person. The format is:
//
//   [primary] == [syn[; syn]*] || [typo [; typo*]]
//
// where "primary" is the name for the main entry (registered name if possible); "syn"s are
// the publicly-visible synonyms that should each get a cross-reference enter; and "typo"s
// are misspelling and tweaks found in the data that are too minor to be worth exposing
// visibly.
//
// This information is used to pre-populate the Persona table. It is not expected to be
// comprehensive -- other names will be added as we find them in the data -- but it is used
// to join up things that we already know look problematic.
trait NameConfigLoader {
  def processLine(line:String) = {
    val primaryVsOthers = line.split("==")
    val primary = primaryVsOthers(0).trim
    val others = primaryVsOthers(1)
    val synVsTypo = others.split("""\|\|""")
    def pullApartSection(section:String) = {
      val str = section.trim
      if (str.length == 0)
        Array.empty[String]
      else {
        val entries = str.split(";")
        entries map (_.trim)
      }
    }
    val syns = pullApartSection(synVsTypo(0))
    val typos = (if (synVsTypo.length > 1) pullApartSection(synVsTypo(1)) else pullApartSection(""))
    Persona.addPerson(primary, syns, typos)
  }
  
  def load(fileName:String) = {
    Log.pushContext("Loading names")
    val fileInput = Source.fromFile(fileName)
    val lines = fileInput.getLines
    lines.foreach(processLine(_))
    fileInput.close()
    Log.popContext
  }
}

object NameConfigLoader extends NameConfigLoader {}

class Config(confFileName:String) {
  // The raw configuration file:
  val confFile = XML.load(confFileName)
  
  def firstText(seq:NodeSeq) = seq.head.text
  
  // Set up logging
  val logPath = firstText(confFile \\ "conf" \\ "logTo")
  Log.logTo(logPath)
  
  // Where are the data files located?
  val dataFileLoc = firstText(confFile \\ "conf" \\ "fileLocation")
  
  // Load the name file
  val nameConfigLoc = firstText(confFile \\ "conf" \\ "names")
  NameConfigLoader.load(nameConfigLoc)
}

object Config {
  // Is there a good way to avoid this Singleton badness? I want to be able to simply say
  // Config.dataFileLoc and things like that, but the vals don't get filled until we've actually
  // read the file in. So I wind up with Config.get.dataFileLoc instead.
  var instance:Option[Config] = None
  def get = instance match {
    case Some(config) => config
    case None => throw new Exception("No Config instance!")
  }
}
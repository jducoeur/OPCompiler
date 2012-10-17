package process

import scala.io.Source

import models.Persona

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
package process

import scala.io.Source

import models.Persona

// This loads in the name-synonym file, which is how we deal with the horrible mess of
// the name data. This file is flat text, where each line contains an entry that represents
// a Person. The format is:
//
//   [primary] == [syn[; syn]*] || [typo [; typo*]] ! [falsepos [;falsepos]]
//
// where "primary" is the name for the main entry (registered name if possible); "syn"s are
// the publicly-visible synonyms that should each get a cross-reference enter; and "typo"s
// are misspelling and tweaks found in the data that are too minor to be worth exposing
// visibly. (NOTE: typos are being deprecated at this point, in favor of just using edit
// distance to decide automatically.) "falsepos" are names that the deduper is hypothesizing
// might be synonyms, but which I think are incorrect.
//
// This information is used to pre-populate the Persona table. It is not expected to be
// comprehensive -- other names will be added as we find them in the data -- but it is used
// to join up things that we already know look problematic.
trait NameConfigLoader {
  def processLine(line:String) = {
    val primaryVsOthers = line.split("==")
    val primary = primaryVsOthers(0).trim
    val others = primaryVsOthers(1)
    val synsVsNot = others.split("""!""")
    val allSyns = synsVsNot(0)
    val synVsTypo = allSyns.split("""\|\|""")
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
    val falsePositives = (if (synsVsNot.length > 1) pullApartSection(synsVsNot(1)) else pullApartSection(""))
    Persona.addPerson(primary, syns, typos, falsePositives)
  }
  
  def load(fileName:String) = {
    Log.pushContext("Loading names")
    val fileInput = Source.fromFile(fileName, "UTF8")
    val lines = fileInput.getLines.filterNot(_(0) == '#').zipWithIndex
    lines.foreach { pair => 
      processLine(pair._1)
    }
    fileInput.close()
    Log.popContext
  }
}

object NameConfigLoader extends NameConfigLoader {}
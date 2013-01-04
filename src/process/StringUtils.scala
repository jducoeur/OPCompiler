package process

object StringUtils {
  
  // Essentially "super-trim"
  def scrub(name:String):String = {
    name.map(c => if (c.toInt == 160) ' ' else c).replaceAll("\\s+", " ").trim
  }

}
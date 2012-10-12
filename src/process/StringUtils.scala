package process

object StringUtils {
  
  // Essentially "super-trim"
  def scrub(name:String):String = {
    name.filter(_.toInt != 160).replaceAll("\\s+", " ").trim
  }

}
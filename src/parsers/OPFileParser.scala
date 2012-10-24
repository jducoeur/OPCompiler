package parsers

import process._

import scala.xml._

abstract trait OPFileParser {
  
  def loadFile(fileInfo:OldFile) = {
    XML.load(fileInfo.name)
  }
  
  // This is the heart of a parser, taking a loaded XHTML file and handling it.
  // Note that it returns Unit. Yes, this is kind of evil, but I'm mutating the
  // world in place instead of passing an immutable world around. I may yet regret
  // that decision; we'll see.
  //
  // Actual parsers fill this method in.
  def processFile(fileNode:Elem, name:String):Unit
  
  def handleFile(fileInfo:OldFile) = {
    Log.pushContext(fileInfo.simpleName)
    val fileNode = loadFile(fileInfo)
    // Drop the ".xhtml" at the end
    val baseName = fileInfo.simpleName dropRight 6
    processFile(fileNode, baseName)
    Log.popContext
  }
}

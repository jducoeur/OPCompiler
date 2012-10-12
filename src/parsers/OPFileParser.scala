package parsers

import process._

import scala.xml._

abstract trait OPFileParser {
  
  def loadFile(fileInfo:OldFile) = {
    val filePath = Config.getFullPath(fileInfo)
    XML.load(filePath)
  }
  
  // This is the heart of a parser, taking a loaded XHTML file and handling it.
  // Note that it returns Unit. Yes, this is kind of evil, but I'm mutating the
  // world in place instead of passing an immutable world around. I may yet regret
  // that decision; we'll see.
  //
  // Actual parsers fill this method in.
  def processFile(fileNode:Elem):Unit
  
  def handleFile(fileInfo:OldFile) = {
    Log.pushContext(fileInfo.name)
    val fileNode = loadFile(fileInfo)
    processFile(fileNode)
    Log.popContext
  }
}

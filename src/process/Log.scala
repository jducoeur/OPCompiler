package process

import java.io._

import scala.collection.immutable.Stack

object Log {
  var writer:PrintWriter = null;
  def logTo(path:String) = {
    writer = new PrintWriter(new File(path))
  }
  
  def finish = {
    writeErrors
    writer.flush
    writer.close
  }
  
  type ContextStack = Stack[String]
  private var contexts:ContextStack = new Stack()
  
  private def indentStr(indents:Int)= "  " * indents
  private def indentStr:String = indentStr(contexts.length)
  
  def pushContext(context:Any) = {
    print(context + " {")
    contexts = contexts.push(context.toString)
  }
  def popContext = {
    contexts = contexts.pop
    print("}")
  }
  
  def print(indent:String, msg:String) = {
    val indentedMsg = msg.toString.replace("\n", "\n" + indent)
    writer.println(indent + indentedMsg)    
  }
  def print(msg:Any):Unit = print(indentStr, msg.toString)
  
  def error(msg:Any) = {
    print("ERROR: " + msg)
    errorLog = RecordedError(msg, contexts) +: errorLog
  }
  
  def info(msg:Any) = print("INFO:" + msg)
  
  private case class RecordedError(msg:Any, context:ContextStack)
  // This is a list, in reverse order, of all the errors we encountered in the run:
  private var errorLog = Seq[RecordedError]()
  
  private def writeErrors = {
    def contextHeader(context:ContextStack) = context.tail.foldLeft(context.head)((str, next) => str + " - " + next)
    def writeError(currentContext:ContextStack, error:RecordedError):ContextStack = {
      if (error.context != currentContext) {
        if (currentContext.length > 0)
          writer.println("}\n")
        writer.println(contextHeader(error.context))
        writer.println("{")
      }
      print(indentStr(1), error.msg.toString)
      error.context
    }
    writer.println("\nERROR LOG:\n")
    errorLog.reverse.foldLeft(new ContextStack())(writeError)
  }
}
object OPCompiler extends App {
	val commandLineConfFileName = args(0)
	
	def run(confFileName:String) = {
	  	import process._
		Log.logTo("log.txt")
	  	NameConfigLoader.load("names.conf")
		val world = Config2.getWorld
		
		// Do the actual loading:
		import parsers.FilesToProcess
		FilesToProcess.processAll
		
		// Print out the results:
		Log.pushContext("Alpha List")
		val personae = models.Persona.byName
		personae.foreach(persona => {
			Log.print(persona)
		})
		Log.popContext
		
		// Emit the results:
		Emitter.emitAll

		Log.finish
	}

	run(commandLineConfFileName)
}
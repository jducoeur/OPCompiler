object OPCompiler extends App {
	val commandLineConfFileName = args(0)
	
	def run(confFileName:String) = {
	  	import process.{Config, Config2}
	  	import process.Log
		Config.instance = Some(new Config(confFileName))
		val world = Config2.getWorld
		
		// Do the actual loading:
		Config.get.filesToProcess.processAll
		
		// Print out the results:
		Log.pushContext("Alpha List")
		val personae = models.Persona.byName
		personae.foreach(persona => {
			Log.print(persona)
		})
		Log.popContext
		
		Log.finish
	}

	run(commandLineConfFileName)
}
.onLoad <- function(libname, pkgname) {

	options(eveneo4jurl="http://shark.epi.bris.ac.uk:8474/db/data")
	options(eveneo4juser="mrever")
	options(eveneo4jpw="mrever")
	options(eveneo4jg=connect())
	if(length(options()$eveneo4jg$version) == 1)
	{
		message("Connected to MR-EvE")
	} else {
		message("Failed to connect to MR-EvE")
	}
}

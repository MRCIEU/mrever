.onLoad <- function(libname, pkgname) {

	options(eveneo4jurl="http://shark.epi.bris.ac.uk:8474/db/data")
	options(eveneo4juser="mrever")
	options(eveneo4jpw="mrever")
	message("Run connect() to get started")
}

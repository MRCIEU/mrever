.onLoad <- function(libname, pkgname) {

	options(eveneo4jurl="http://eve-neo4j.mrbase.org/db/data")
	options(eveneo4juser="mrever")
	options(eveneo4jpw="mrever")
	message("Run connect() to get started")
}

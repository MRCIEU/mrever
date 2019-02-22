#' Connect to graph
#'
#' <full description>
#'
#' @param url=NULL <what param does>
#' @param username=NULL <what param does>
#' @param password=NULL <what param does>
#'
#' @export
#' @return graph object
connect <- function(url=NULL, username=NULL, password=NULL)
{
	graph <- RNeo4j::startGraph(
		url = ifelse(is.null(url), options()$eveneo4jurl, url),
		username = ifelse(is.null(username), options()$eveneo4juser, username),
		password = ifelse(is.null(password), options()$eveneo4jpw, password)
	)
	return(graph)
}


#' Get traits
#'
#' <full description>
#'
#' @param trait=NULL <what param does>
#' @param graph <what param does>
#'
#' @export
#' @return dataframe
get_traits <- function(id=NULL, graph = options()$eveneo4jg)
{
	fields <- c("author", "category", "consortium", "bgcidId", "note", "nsnp", "pmid", "population", "priority", "sample_size", "sd", "sex", "subcategory", "trait", "unit", "year")

	if(is.null(id))
	{
		query <- paste0(
			"match (t:TRAIT) return ",
			paste(paste0("t.", fields, " as ", fields), collapse=", ")
		)
	} else {
		query <- paste0(
			"match (t:TRAIT) where t.bgcidId IN ['", paste(id, collapse="','"), "']  return ",
			paste0(paste0("t.", fields, " as ", fields), collapse=", ")
		)
	}
	return(cypher(graph, query))
}


# Get instruments
#' <brief desc>
#'
#' <full description>
#'
#' @param id=NULL <what param does>
#' @param graph = options()$eveneo4jg <what param does>
#'
#' @export
#' @return dataframe
get_instruments <- function(id, graph = options()$eveneo4jg)
{
	fields <- c("beta", "se", "pval", "eaf", "samplesize", "ncase", "ncontrol")
	query <- paste0("match (v:VARIANT)-[i:INSTRUMENT]->(t:TRAIT) where t.bgcidId IN ['", paste(id, collapse="','"), "']  return v.variantId as variantId, v.chr as chr, v.pos as pos, v.ref as ref, v.alt as alt, t.bgcidId as bgcidId, t.unit as unit, ",
		paste(paste0("i.", fields, " as ", fields), collapse=", ")
	)
	print(query)
	return(cypher(graph, query))
}


# Get genetic associations
#' <brief desc>
#'
#' <full description>
#'
#' @param variant=NULL <what param does>
#' @param id=NULL <what param does>
#' @param graph = options()$eveneo4jg <what param does>
#'
#' @export
#' @return dataframe
get_genassoc <- function(variant, id, graph = options()$eveneo4jg)
{
	fields <- c("beta", "se", "pval", "eaf", "samplesize", "ncase", "ncontrol")
	query <- paste0("match (v:VARIANT)-[i:GENASSOC]->(t:TRAIT)
		where t.bgcidId IN ['", paste(id, collapse="','"), "'] 
		and v.variantId IN ['", paste(variant, collapse="','"), "']
		return v.variantId as variantId, v.chr as chr, v.pos as pos, v.ref as ref, v.alt as alt, t.bgcidId as bgcidId, t.unit as unit, ",
		paste(paste0("i.", fields, " as ", fields), collapse=", ")
	)
	print(query)
	return(cypher(graph, query))
}




# Get trait-trait relationship

# Phewas



# graph <- startGraph("http://shark.epi.bris.ac.uk:8474/db/data", username="neo4j", password="123qwe")
# qu <- "match (n:TRAIT) where n.trait = 'Body mass index' return n.trait"

# qu <- "match (v:VARIANT)-[i:INSTRUMENT]->(t:TRAIT) where t.bgcidId='' return 
# v.variantId, 
# v.chr,
# v.pos,
# v.ref,
# v.alt,
# t.bgcidId,
# i.beta,
# i.se,
# i.pval,
# i.samplesize,
# i.ncase,
# i.ncontrol,
# i.eaf
# "

# cypher(graph, qu)

# qu <- "match (t:TRAIT) return keys(t)"

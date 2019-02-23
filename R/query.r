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
	options(eveneo4jg = RNeo4j::startGraph(
		url = ifelse(is.null(url), options()$eveneo4jurl, url),
		username = ifelse(is.null(username), options()$eveneo4juser, username),
		password = ifelse(is.null(password), options()$eveneo4jpw, password)
	))
	if(length(options()$eveneo4jg$version) == 1)
	{
		message("Connected to MR-EvE")
	} else {
		message("Failed to connect to MR-EvE")
	}
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
	return(cypher(graph, query))
}

#' Get MR estimates
#'
#' @param id1 <what param does>
#' @param id2 <what param does>
#' @param graph = options()$eveneo4jg <what param does>
#'
#' @export
#' @return list
get_mr <- function(id1, id2, graph = options()$eveneo4jg)
{
	l <- list()
	query <- paste0("match (id1:TRAIT)-[m:MR]->(id2:TRAIT)
	where id1.bgcidId IN ['", paste(id1, collapse="','"), "']
	and id2.bgcidId IN ['", paste(id2, collapse="','"), "']
	return id1.bgcidId as exposure, id2.bgcidId as outcome, m.method+' - '+m.selection as method, m.b as b, m.se as se, m.ci_low as ci_low, m.ci_upp as ci_upp, m.pval as pval, m.nsnp as nsnp, m.moescore as moescore"
	)
	l$mr <- cypher(graph, query)

	query <- paste0("match (id1:TRAIT)-[m:MRHET]->(id2:TRAIT)
	where id1.bgcidId IN ['", paste(id1, collapse="','"), "']
	and id2.bgcidId IN ['", paste(id2, collapse="','"), "']
	return id1.bgcidId as exposure, id2.bgcidId as outcome, m.method+' - '+m.selection as method, m.q as q, m.df as df, m.pval as pval"
	)
	l$heterogeneity <- cypher(graph, query)

	query <- paste0("match (id1:TRAIT)-[m:MRINTERCEPT]->(id2:TRAIT)
	where id1.bgcidId IN ['", paste(id1, collapse="','"), "']
	and id2.bgcidId IN ['", paste(id2, collapse="','"), "']
	return id1.bgcidId as exposure, id2.bgcidId as outcome, m.method+' - '+m.selection as method, m.b as b, m.se as se, m.pval as pval, m.nsnp as nsnp"
	)
	l$directional_pleiotropy <- cypher(graph, query)

	return(l)
}

#' Get MR-MOE estimates
#'
#' @param id1 <what param does>
#' @param id2 <what param does>
#' @param graph = options()$eveneo4jg <what param does>
#'
#' @export
#' @return data frame
get_mrmoe <- function(id1, id2, graph = options()$eveneo4jg)
{
	query <- paste0("match (id1:TRAIT)-[m:MRMOE]->(id2:TRAIT)
	where id1.bgcidId IN ['", paste(id1, collapse="','"), "']
	and id2.bgcidId IN ['", paste(id2, collapse="','"), "']
	return id1.bgcidId as exposure, id2.bgcidId as outcome, m.method+' - '+m.selection as method, m.b as b, m.se as se, m.ci_low as ci_low, m.ci_upp as ci_upp, m.pval as pval, m.nsnp as nsnp, m.moescore as moescore"
	)
	return(cypher(graph, query))
}

#' Perform phewas
#'
#' <full description>
#'
#' @param id <what param does>
#' @param direction="exposure" <what param does>
#' @param graph = options()$eveneo4jg <what param does>
#'
#' @export
#' @return data frame
phewas <- function(id, direction="exposure", graph = options()$eveneo4jg)
{
	stopifnot(length(id)==1)
	if(direction == "exposure")
	{
		query <- paste0("match (id2:TRAIT)<-[m:MRMOE]-(id1:TRAIT {bgcidId: '", id, "'})
		return id1.bgcidId as exposure, id2.bgcidId as outcome, m.method+' - '+m.selection as method, m.b as b, m.se as se, m.ci_low as ci_low, m.ci_upp as ci_upp, m.pval as pval, m.nsnp as nsnp, m.moescore as moescore"
		)
	} else if(direction == "outcome") {
		query <- paste0("match (id1:TRAIT)-[m:MRMOE]->(id2:TRAIT {bgcidId: '", id, "'})
		return id1.bgcidId as exposure, id2.bgcidId as outcome, m.method+' - '+m.selection as method, m.b as b, m.se as se, m.ci_low as ci_low, m.ci_upp as ci_upp, m.pval as pval, m.nsnp as nsnp, m.moescore as moescore"
		)
	} else {
		stop("Direction must be 'exposure' or 'outcome'")
	}
	return(cypher(graph, query))
}

#' Read in extracted data format
#'
#' @param filename filename e.g. ml.csv.gz
#'
#' @export
#' @return
readml <- function(filename, ao, format="TwoSampleMR")
{
	require(TwoSampleMR)
	require(dplyr)
	a <- read.csv(
		filename, 
		header=FALSE,
		stringsAsFactors=FALSE
	) %>% as_tibble(.)
	names(a) <- c("id", "SNP", "chr", "pos", "other_allele", "effect_allele", "beta", "se", "pval", "eaf", "samplesize", "ncase", "proxy_rsid", "instrument")

	# Fill in missing info
	id <- unique(a$id)
	r <- ao[ao$id == id, ]

	stopifnot(nrow(r) == 1)

	a$samplesize[is.na(a$samplesize)] <- r$sample_size
	a$units <- r$unit
	a$ncase[is.na(a$ncase)] <- r$ncase
	a$ncontrol <- r$ncontrol
	a$Phenotype <- r$trait
	a$units[is.na(a$units)] <- "unknown"

	if(format == "TwoSampleMR")	
	{
		# Convert
		if(sum(a$instrument) > 0)
		{
			exposure_dat <- suppressWarnings(TwoSampleMR::format_data(subset(a, instrument))) %>% dplyr::as_tibble(.)
		} else {
			exposure_dat <- tibble()
		}
		outcome_dat <- suppressWarnings(TwoSampleMR::format_data(subset(a, !instrument), type="outcome")) %>% dplyr::as_tibble(.)
		return(list(exposure_dat = exposure_dat, outcome_dat=outcome_dat))
	} else {
		return(a)
	}
}


#' Write as csv for neo4j
#'
#' @param x Data frame
#' @param basename Filename to write to. Should be gzip
#' @param header Whether to include header. If so, it is written as a separate file. Default=FALSE
#'
#' @export
#' @return NULL
write_out <- function(x, basename, header=FALSE)
{
	g <- gzfile(basename, "w")
	write.table(x, g, row.names=FALSE, col.names=FALSE, na="", sep=",")
	close(g)
	if(header) write.table(x[0,], file=paste0(basename, "_header.csv"), row.names=FALSE, col.names=TRUE, sep=",")
}

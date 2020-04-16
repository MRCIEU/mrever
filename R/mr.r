#' Create TwoSampleMR dat object
#'
#' <full description>
#'
#' @param id1 <what param does>
#' @param id2 <what param does>
#'
#' @export
#' @return
mrever_to_mr <- function(id1, id2)
{
	a <- TwoSampleMR::format_data(
		get_instruments(id1),
		snp_col="variantId",
		effect_allele_col="alt",
		other_allele_col="ref",
		id_col="bgcidId",
		units_col="unit"
	)
	b <- TwoSampleMR::format_data(
		get_genassoc(a$SNP, id2),
		type="outcome",
		snp_col="variantId",
		effect_allele_col="alt",
		other_allele_col="ref",
		id_col="bgcidId",
		units_col="unit"
	)
	return(TwoSampleMR::harmonise_data(a,b))
}

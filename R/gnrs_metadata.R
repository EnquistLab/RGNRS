#'Get GNRS metadata
#'
#'Returns metadata on GNRS including version and citation information
#' @param bibtex_file Optional output file for writing bibtex citations.
#' @return List containing: (1) bibtex-formatted citation information,
#' (2) information about GNRS data sources,
#' (3) GNRS version information, and
#' (4) information that can be used in an acknowledgments statement..
#' @note This function provides citation information in bibtex format that can be used with reference manager software (e.g. Paperpile, Zotero). Please remember to cite both the sources and the GNRS, as the GNRS couldn't exist without these sources!
#' @note This function is a wrapper that returns the output of the functions GNRS_citations, GNRS_sources, GNRS_version, and GNRS_acknowledgments.
#' @export
#' @examples {
#' metadata <- GNRS_metadata()
#' }
#'
GNRS_metadata <- function(bibtex_file=NULL){
  
  output <- list()
  
  output[[1]] <- GNRS_citations()
  output[[2]] <- GNRS_sources()
  output[[3]] <- GNRS_version()
  output[[4]] <- GNRS_acknowledgments()
  
  names(output) <- c("citations", "sources", "version", "acknowledgments")
  
  #Write the bibtex information if a file is specified
  if(!is.null(bibtex_file)){writeLines(text = output$citations$citation, con = bibtex_file)}
  
  return(output)
  
}
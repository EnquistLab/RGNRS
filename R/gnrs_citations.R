#'Get citation information
#'
#'Return information needed to cite the GNRS
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing bibtex-formatted citation information
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples \dontrun{
#' GNRS_citations_metadata <- GNRS_citations()
#' }
#' 
GNRS_citations <- function(...){

  # # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    #return(invisible(NULL))
  }

  return(gnrs_core(mode = "citations", ...))


}#GNRS citations

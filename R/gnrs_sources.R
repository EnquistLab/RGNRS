#'Get metadata on current GNRS sources
#'
#'Return metadata about the current GNRS version
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing current GNRS sources.
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples \dontrun{
#' GNRS_sources_metadata <- GNRS_sources()
#' }
#' 
GNRS_sources <- function(...){
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    #return(invisible(NULL))
  }

  return(gnrs_core(mode = "sources", ...))
  

}#GNRS sources

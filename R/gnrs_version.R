#'Get metadata on current GNRS version
#'
#'Return metadata about the current GNRS version
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing current GNRS version number, build date, and code version.
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples \dontrun{
#' GNRS_version_metadata <- GNRS_version()
#' }
#' 
GNRS_version <- function(...){
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }

return(gnrs_core(mode = "meta", ...))
  
  
}#GNRS version

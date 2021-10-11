#'Get acknowledgment information
#'
#'Return information needed to acknowledge GNRS contributors
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing acknowledgments
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples \dontrun{
#' GNRS_acknowledgments_metadata <- GNRS_acknowledgments()
#' }
#' 
GNRS_acknowledgments <- function(...){
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    #return(invisible(NULL))
  }

  return(gnrs_core(mode = "collaborators", ...))

}#GNRS sources

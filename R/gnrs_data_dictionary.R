#'Get Data Dictionary
#'
#'Return GNRS Data Dictionary
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing GNRS Data Dictionary
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples \dontrun{
#' GNRS_dictionary <- GNRS_data_dictionary()
#' }
#' 
GNRS_data_dictionary <- function(...){
  
  # # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    #return(invisible(NULL))
  }
  
  return(gnrs_core(mode = "dd", ...))
  
  # set option mode.
  mode <- "dd"		
   
}#GNRS sources

#'Get metadata on countries
#'
#'Return metadata about countries used by the GNRS
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing information on countries (e.g. iso code, fips code, continent, standardized name)
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples \dontrun{
#' countries <- GNRS_get_countries()
#' }
#' 
GNRS_get_countries <- function(...){
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }
  
  return(gnrs_core(mode = "countrylist", ...))
  
  
} #GNRS get countries

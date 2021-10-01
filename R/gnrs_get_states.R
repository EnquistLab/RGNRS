#'Get metadata on states
#'
#'Return metadata about states used by the GNRS
#' @param country_id A GNRS country_id, or a vector of country_ids. If empty, will return metadata for all countries.
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing information on states/provinces (e.g. iso code, fips code, continent, standardized name)
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples \dontrun{
#' states <- GNRS_get_states()
#' }
#' 
GNRS_get_states <- function(country_id = "", ...){

  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }
  
  #Check input format
  if(!identical(x = country_id,y = "")){
    
  id_check <- suppressWarnings(all(as.numeric(country_id) %% 1 == 0))
  if(is.na(id_check)) {
    stop("country_id should be an integer(s) or an empty character (the default)")
    }
  
  if(!id_check) {
    stop("country_id should be an integer(s) or an empty character (the default)")
  }
  }
  

  #Format input data
  data_json <- jsonlite::toJSON(data.frame(country = country_id))

  results <- gnrs_core(mode = "statelist", data_json = data_json, ...)
  #results <- gnrs_core(mode = "statelist", data_json = data_json)
  
  
  if (length(results) == 0) {
    
    message("NO matches found for the submitted country_id")
    return(invisible(NULL))
    
  }
  
  return(results)
  

} 

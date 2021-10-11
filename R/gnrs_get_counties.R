#'Get metadata on counties
#'
#'Return metadata about counties, parishes, etc. used by the GNRS
#' @param state_province_id A GNRS state_id, or a vector of state_ids.
#' @param ... Additional parameters passed to internal functions
#' @return Dataframe containing information on counties/parishes (e.g. iso code, fips code, continent, standardized name)
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples \dontrun{
#' states <- GNRS_get_states()
#' us_counties <- GNRS_get_counties(
#' state_province_id = states$state_province_id[
#' which(states$country_iso == "US")])
#' }
#' 

GNRS_get_counties <- function(state_province_id = "", ...) {
  
  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    #return(invisible(NULL))
  }

  #Check input format
  if(!identical(x = state_province_id, y = "")){
    
    id_check <- suppressWarnings(all(as.numeric(state_province_id) %% 1 == 0))
    if(is.na(id_check)) {
      stop("state_province_id should be an integer(s) or an empty character (the default)")
    }
    
    if(!id_check) {
      stop("state_province_id should be an integer(s) or an empty character (the default)")
    }
  
  }
  
  if(length(state_province_id) > 5000){
    stop("At present the GNRS API has a record limit of 5000.")
  
  }

  data_json <- jsonlite::toJSON(data.frame(state = state_province_id))

  results <- gnrs_core(mode = "countylist",data_json = data_json, ...)

  if (length(results) == 0) {
    
    message("NO matches found for the submitted state_id")
    return(invisible(NULL))

  }

  return(results)

}


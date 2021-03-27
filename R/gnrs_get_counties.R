#'Get metadata on counties
#'
#'Return metadata about counties, parishes, etc. used by the GNRS
#' @param state_province_id A GNRS state_id, or a vector of state_ids.
#' @return Dataframe containing information on counties/parishes (e.g. iso code, fips code, continent, standardized name)
#' @import RCurl
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples \dontrun{
#' states <- GNRS_get_states()
#' us_counties <- GNRS_get_counties(
#' state_province_id = states$state_province_id[
#' which(states$country_iso == "US")])
#' }
#' 

GNRS_get_counties <- function(state_province_id = "") {
  
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
  
  # api url
  url = "http://vegbiendev.nceas.ucsb.edu:8875/gnrs_api.php" # production
  #url = "http://vegbiendev.nceas.ucsb.edu:9875/gnrs_api.php" # development
  
  # Construct the request
  headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')
  
  #Format input data
  data_json <- jsonlite::toJSON(data.frame(state = state_province_id))
  
  # Re-form the options json again
  # Note that only 'mode' is needed
  mode <- "countylist"	
  opts <- data.frame(c(mode))
  names(opts) <- c("mode")
  opts_json <- jsonlite::toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  # Form the input json, including both options and data
  input_json <- paste0('{"opts":', opts_json, ',"data":', data_json, '}' )
  results_json <- postForm(url, .opts=list(postfields= input_json, httpheader=headers))
  
  # Display the results
  results <- jsonlite::fromJSON(results_json)
  
  if (length(results) == 0) {
    
    return(cat("No matches found for the submitted state_id"))
    

    
  }
  
  return(results)
  
}


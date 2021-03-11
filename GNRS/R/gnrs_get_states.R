#'Get metadata on states
#'
#'Return metadata about states used by the GNRS
#' @param country_id A GNRS country_id, or a vector of country_ids. If empty, will return metadata for all countries.
#' @return Dataframe containing information on states/provinces (e.g. iso code, fips code, continent, standardized name)
#' @import RCurl
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples{
#' states <- GNRS_get_states()
#' }
#' 
GNRS_get_states <- function(country_id = ""){
  
  
  if(!identical(x = country_id,y = "")){
    
  id_check <- suppressWarnings(all(as.numeric(country_id) %% 1 == 0))
  if(is.na(id_check)) {
    stop("country_id should be an integer(s) or an empty character (the default)")
    }
  
  if(!id_check) {
    stop("country_id should be an integer(s) or an empty character (the default)")
  }
  }
  
  # api url
  url = "http://vegbiendev.nceas.ucsb.edu:8875/gnrs_api.php" # production
  #url = "http://vegbiendev.nceas.ucsb.edu:9875/gnrs_api.php" # development
  
  # Construct the request
  headers <- list('Accept' = 'application/json', 'Content-Type' = 'application/json', 'charset' = 'UTF-8')
  
  #Format input data
  data_json <- jsonlite::toJSON(data.frame(country = country_id))
  
  # Re-form the options json again
  # Note that only 'mode' is needed
  mode <- "statelist"		
  opts <- data.frame(c(mode))
  names(opts) <- c("mode")
  opts_json <- jsonlite::toJSON(opts)
  opts_json <- gsub('\\[','',opts_json)
  opts_json <- gsub('\\]','',opts_json)
  
  # Form the input json, including both options and data
  input_json <- paste0('{"opts":', opts_json, ',"data":', data_json, '}' )
  results_json <- postForm(url, .opts=list(postfields= input_json, httpheader=headers))
  
  #Convert from JSON
  results <- jsonlite::fromJSON(results_json)
  
  #Check whether anything was found
  if (length(results) == 0) {
    
    return(cat("No matches found for the submitted country_id"))
    
    
    
  }
  
  
  # Display the results
  results <- jsonlite::fromJSON(results_json)
  
  return(results)
  
} 

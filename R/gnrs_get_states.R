#'Get metadata on states
#'
#'Return metadata about states used by the GNRS
#' @param country_id A GNRS country_id, or a vector of country_ids. If empty, will return metadata for all countries.
#' @return Dataframe containing information on states/provinces (e.g. iso code, fips code, continent, standardized name)
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples \dontrun{
#' states <- GNRS_get_states()
#' }
#' 
GNRS_get_states <- function(country_id = ""){

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
  
  # api url
  url = "https://gnrsapi.xyz/gnrs_api.php" # public stable version
  #url = "http://vegbiendev.nceas.ucsb.edu:8875/gnrs_api.php" # public development production
  #url = "http://vegbiendev.nceas.ucsb.edu:9875/gnrs_api.php" #bleeding edge development
  
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

  # Send the request in a "graceful failure" wrapper for CRAN compliance
  tryCatch(expr = results_json <- POST(url = url,
                                       add_headers('Content-Type' = 'application/json'),
                                       add_headers('Accept' = 'application/json'),
                                       add_headers('charset' = 'UTF-8'),
                                       body = input_json,
                                       encode = "json"),
           error = function(e) {
             message("There appears to be a problem reaching the API.")
           })
  
  #Return NULL if API isn't working
  if(!exists("results_json")){return(invisible(NULL))}
  
  #Convert from JSON
  results <- as.data.frame( fromJSON( rawToChar( results_json$content ) ) )

  #Check whether anything was found
  if (length(results) == 0) {
    
    return(cat("No matches found for the submitted country_id"))
    
    
    
  }
  
  return(results)
  
} 
